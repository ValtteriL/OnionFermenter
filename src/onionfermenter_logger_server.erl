-module(onionfermenter_logger_server).
-behaviour(gen_server).

-export([start_link/1]). % used to start the logger server
-export([log/3]). % used to send messages to be logged by this server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {iodevice}).

% logger server awaits for messages and writes them to log file
% the log file is Name.log in working directory
% log entries are lines of the following format: unixTimestamp,base64(Msg)

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

init([Name]) ->
    %% Start accepting requests
    %% We must cast this to the worker's process, as it blocks it.
    {ok, IoDevice} = file:open(unicode:characters_to_list([atom_to_list(Name), ".log"], latin1), [append]),
    {ok, #state{iodevice=IoDevice}}.

% API function to log message
log(Pid, Uid, Msg) ->
    gen_server:cast(Pid, {log, Uid, Msg}).

% handle message from worker
handle_cast({log, Uid, Msg}, State) ->
    ok = file:write(State#state.iodevice, format_log_message(Uid, Msg)), % write to log
    {noreply, State};

handle_cast(_, State) ->
    {noreply, State}.

handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.
terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.


%%% helpers
% format message for logging (unix timestamp, uid, base64 body)
format_log_message(Uid, Msg) ->
    Timestamp = integer_to_list(os:system_time()),
    Base64Msg = base64:encode_to_string(binary_to_list(Msg)),
    unicode:characters_to_list([Timestamp, ",", Uid, "," , Base64Msg, "\n"], latin1).