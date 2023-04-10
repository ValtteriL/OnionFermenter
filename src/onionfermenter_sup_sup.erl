%%%-------------------------------------------------------------------
%% @doc onionfermenter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(onionfermenter_sup_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([add_onionfermenter/5]). % add servers to the program with this

-define(SERVER, ?MODULE). % register locally with the module name

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
add_onionfermenter(Name, Port, DestinationHost, Replace, DstPort) ->
    ChildSpecs = #{ id => Name, start => { onionfermenter_sup, start_link, [Port, DestinationHost, Replace, Name, DstPort]}, type => supervisor},
    supervisor:start_child(?SERVER, ChildSpecs).