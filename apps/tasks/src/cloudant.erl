-module(cloudant).

-export([start_nodes/1, stop_nodes/1, wipe/1]).

-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% API
%%%===================================================================

start_nodes(all) ->
    ditz_utils:cmd_loop_thru("~s~s", [base_dir, start_cmd], fun node_ctx/1).

stop_nodes(all) ->
    ok.

wipe([data, all]) ->
    ditz_utils:cmd_loop_thru("rm -rf ~s/x*", [data_dir], fun node_ctx/1);
wipe([state, all]) ->
    ditz_utils:cmd_loop_thru("rm ~s/membership.*", [data_dir], fun node_ctx/1);
wipe([couch, all]) ->
    ditz_utils:cmd_loop_thru("rm ~s/*.couch", [data_dir], fun node_ctx/1).


%%%===================================================================
%%% Internal functions
%%%===================================================================

% int_start_nodes(_StartCmd, []) -> ok;
% int_start_nodes(StartCmd, [Node|Rest]) ->
%     int_start_node(StartCmd, Node),
%     int_start_nodes(StartCmd, Rest).

% int_start_node(StartCmd, {Order, Node, _Host}) ->
%     Cmd = io_lib:format("~s ~s ~s",
%         [StartCmd, integer_to_list(Order), atom_to_list(Node)]),
%     run_cmd(Cmd, Order).

% int_stop_nodes(_PidDir, []) -> ok;
% int_stop_nodes(PidDir, [Node|Rest]) ->
%     int_stop_node(PidDir, Node),
%     int_stop_nodes(PidDir, Rest).

% int_stop_node(PidDir, {Order, _Node, _Host}) ->
%     Filename = io_lib:format("~scouchdb~s.pid",
%                              [PidDir, integer_to_list(Order)]),
%     {ok, BinPid} = file:read_file(Filename),
%     Pid = binary_to_list(BinPid),
%     Cmd = io_lib:format("kill ~s", [Pid]),
%     run_cmd(Cmd, Order).

% run_cmd(Cmd, Order) ->
%     case os:cmd(Cmd) of
%     [] -> ok;
%     Error ->
%         ?debugFmt("~nNode ~p~nError: ~p~n", [Order, Error]),
%         Error
%     end.

node_ctx({NodeNum, NodeName, Host}) ->
    dict:from_list([{nodenum, NodeNum},
                    {nodename, NodeName},
                    {host, Host}]).
