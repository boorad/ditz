-module(cloudant).
-author('brad@cloudant.com').

-export([start_nodes/1, stop_nodes/1, wipe/2, nodeup/2, nodeup/3]).
-export([all_dbs_test/3]).

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
    stop_nodes(Rest);
stop_nodes(NodeNum) when is_integer(NodeNum) ->
    stop_nodes([NodeNum]).

wipe(data, all) ->
    ditz_utils:cmd_loop_thru("rm -rf {{data_dir}}x*");
wipe(state, all) ->
    ditz_utils:cmd_loop_thru("rm {{data_dir}}membership.*");
wipe(couch, all) ->
    ditz_utils:cmd_loop_thru("rm {{data_dir}}*.couch").

nodeup(ExistingNodeNum, NewNodeNum) ->
    nodeup(ExistingNodeNum, NewNodeNum, 5000).

nodeup(_, _, Wait) when Wait < 0 -> timeout;
nodeup(ExistingNodeNum, NewNodeNum, Wait) ->
    NewNode = ditz_utils:get_node(NewNodeNum),
    BinNode = atom_to_binary(NewNode, utf8),
    JsonResp = httpclient:get(ExistingNodeNum, "_cloudant/membership"),
    {Resp} = mochijson2:decode(JsonResp),
    AllNodes = proplists:get_value(<<"all_nodes">>, Resp),
    case lists:member(BinNode, AllNodes) of
    true -> ok;
    _ ->
        timer:sleep(250),
        io:format("~nWait: ~p~n", [Wait-250]),
        nodeup(ExistingNodeNum, NewNodeNum, Wait-250)
    end.

all_dbs_test(NodeNum, TestType, DbName) when is_list(DbName) ->
    all_dbs_test(NodeNum, TestType, list_to_binary(DbName));
all_dbs_test(NodeNum, TestType, DbName) ->
    Json = httpclient:get(NodeNum, "_all_dbs"),
    Resp = mochijson2:decode(Json),
    int_all_dbs_test(TestType, DbName, Resp).

%%
%% internal
%%

int_all_dbs_test(present, DbName, List) ->
    lists:member(DbName, List);
int_all_dbs_test(not_present, DbName, List) ->
    (not lists:member(DbName, List)).
