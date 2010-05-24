-module(cloudant).
-author('brad@cloudant.com').

-export([start_nodes/1, stop_nodes/1, wipe/1]).

-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% API
%%%===================================================================

start_nodes(all) ->
    ditz_utils:cmd_loop_thru("{{start_cmd}}").

stop_nodes(all) ->
    ditz_utils:cmd_loop_thru("{{stop_cmd}}");
stop_nodes([]) -> ok;
stop_nodes([NodeNum|Rest]) ->
    [{Server,Node}] = ditz_utils:get_server_node(NodeNum),
    ditz_utils:exec_cmd({Server,Node},"{{stop_cmd}}"),
    stop_nodes(Rest).

wipe([data, all]) ->
    ditz_utils:cmd_loop_thru("rm -rf {{data_dir}}x*");
wipe([state, all]) ->
    ditz_utils:cmd_loop_thru("rm {{data_dir}}membership.*");
wipe([couch, all]) ->
    ditz_utils:cmd_loop_thru("rm {{data_dir}}*.couch").


%%%===================================================================
%%% Internal functions
%%%===================================================================
