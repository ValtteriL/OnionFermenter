%%%-------------------------------------------------------------------
%% @doc onionfermenter supervisor.
%% @end
%%%-------------------------------------------------------------------

%%
% supervisor opens tcp listen
% supervisor creates one logger process
% supervisor creates 5 worker processes

-module(onionfermenter_sup).
-behaviour(supervisor).

-export([start_link/5, start_socket/1]). % start the onionfermenter with this
-export([empty_listeners/1]).
-export([init/1]).

%-define(SERVER, ?MODULE).

start_link(Port, DestinationHost, Replace, Name, DstPort) ->
    supervisor:start_link({local, Name}, ?MODULE, [Port, DestinationHost, Replace, Name, DstPort]). % register the supervising process locally with name

init([Port, DestinationHost, Replace, Name, DstPort]) ->

    % create logger process (not restarted; if crashes, the whole redirect crashes)
    {ok, Logger} = onionfermenter_logger_server:start_link(Name),

    % listen on port, deliver received packets as binary, use socket for ipv4
    {ok, Listen} = gen_tcp:listen(Port, [binary, inet, {active, once}]),

    % create 5 worker processes (restart = temporary, so a child process is never restarted)
    Pid = self(),
    spawn_link(?MODULE, empty_listeners, [Pid]),

    SupFlags = #{strategy => simple_one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{ id => worker, start => {onionfermenter_worker_server, start_link, [Listen, Logger, DestinationHost, Replace, DstPort, Pid]}, restart => temporary }],

    io:format("sup: port ~p -> ~p:~p, replacing with ~p~n", [Port, DestinationHost, DstPort, Replace]),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
start_socket(Pid) ->
    supervisor:start_child(Pid, []).

%% Start with 5 listeners so that many multiple connections can
%% be started at once, without serialization. In best circumstances,
%% a process would keep the count active at all times to insure nothing
%% bad happens over time when processes get killed too much.
empty_listeners(Pid) ->
    [start_socket(Pid) || _ <- lists:seq(1,5)],
    ok.