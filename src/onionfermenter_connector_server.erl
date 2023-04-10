-module(onionfermenter_connector_server).
-behaviour(gen_server).

-export([start_link/3]). % used to start the connector server
-export([send/2]). % used to send messages to destinationhost through the server
-export([close/2]). % used to indicate worker that worker socket has been closed
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {socket, worker, destinationhost}).

start_link(Worker, DestinationHost, DstPort) ->
    gen_server:start_link(?MODULE, [Worker, DestinationHost, DstPort], []).

init([Worker, DestinationHost, DstPort]) ->
    %% Start accepting requests
    Ret = gen_tcp:connect(DestinationHost, DstPort, [binary, inet, {send_timeout, 60 * 1000}, {send_timeout_close, true}, {active, once}], 60 * 1000), % 60 second timeout for connect and send, otherwise close

    case Ret of
        {ok, Socket} -> {ok, #state{socket=Socket, worker=Worker, destinationhost=DestinationHost}};
        {error, Reason} ->
            onionfermenter_worker_server:close(Worker, Reason), % let worker know of the error
            {ok, #state{worker=Worker, destinationhost=DestinationHost}}
    end.

% API function to send message to destinationHost
send(Pid, Msg) ->
    gen_server:cast(Pid, {send, Msg}).

% handle messages from destination
handle_info({tcp, Socket, Msg}, State) ->
    ok = inet:setopts(Socket, [{active, once}]), % set as active once again
    onionfermenter_worker_server:send(State#state.worker, Msg), % send unchanged to worker
    {noreply, State};
% let worker know and exit when socket closed
handle_info({tcp_closed, _Socket}, State) ->
    onionfermenter_worker_server:close(State#state.worker, tcp_closed),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    onionfermenter_worker_server:close(State#state.worker, tcp_error),
    {stop, normal, State};
handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

% handle message from worker
handle_cast({send, Msg}, State) ->
    gen_tcp:send(State#state.socket, Msg), % send message to destination
    {noreply, State};
% handle closed worker socket to ourHost
handle_cast({close, _Reason}, State) ->
    {stop, normal, State};
handle_cast(_, State) -> {noreply, State}.

% API function to notify connector that worker socket has been closed or there has been an error
close(Pid, Reason) ->
    gen_server:cast(Pid, {close, Reason}).

handle_call(_E, _From, State) -> {noreply, State}.
terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.