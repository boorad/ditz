-module(ditz_utils).
-author('brad@cloudant.com').

-export([cmd_loop_thru/1, get_node/1, get_ctx/0, get_ctx/2]).

-include_lib("eunit/include/eunit.hrl").


%% loops thru Servers & the Nodes on each Server, executing Cmd template, and
%% filling in the values with Options
cmd_loop_thru(CmdTemplate) ->
    ServersNodes = servers_nodes(),
    Results = lists:map(fun(ServerNode) ->
        exec_cmd(ServerNode, CmdTemplate)
    end, ServersNodes),
    case lists:usort(Results) of
    [ok] -> ok;
    Other -> Other
    end.

get_node(Num) ->
    {ok, Nodes} = ditz_server:nodelist(),
    lists:keyfind(Num, 1, Nodes).

get_ctx() ->
    get_ctx(node(), no_node).

get_ctx(Server, Node) ->
    List = lists:append([sys_ctx(), node_ctx(Node), conf_ctx(Server)]),
    render_ctx(List).


%%%===================================================================
%%% Internal functions
%%%===================================================================

% get list of servers/nodes based on config file
servers_nodes() ->
    Servers = lists:append([node()], nodes()),
    lists:flatmap(fun(Server) ->
        {ok, NodeList} = ditz_server:nodelist(Server),
        lists:map(fun(Node) -> {Server, Node} end, NodeList)
    end, Servers).

% construct the command and execute
exec_cmd({Server, Node}, CmdTemplate) ->
    Ctx = get_ctx(Server, Node),
    Cmd = mustache:render(CmdTemplate, Ctx),
    ditz_server:cmd(Server, Cmd).

% internal system context
sys_ctx() ->
    [
     {ditz_dir, code:priv_dir(ditz) ++ "/"},
     {conf_dir, "{{ditz_dir}}conf/"}
    ].

%% conf context context (itests file options)
conf_ctx(Server) ->
    {ok, Options} = ditz_server:options(Server),
    Options.

% node-specific context
node_ctx({NodeNum, NodeName, Host}) ->
    [{nodenum, NodeNum},
     {nodename, NodeName},
     {host, Host}];
node_ctx(no_node) ->
    [].

% traverse proplist, rendering templates with earlier-rendered context values
% the list should have options in the following order: sys, node, conf
% we use this order rather than a dict's undetermined key order
render_ctx(List) when is_list(List) ->
    render_ctx(List, dict:from_list(List)).

render_ctx([], Dict) ->
    %?debugFmt("~nCtx: ~p~n", [dict:to_list(Dict)]),
    Dict;
render_ctx([{K,_V}|Rest], Dict) ->
    NewDict = try
        dict:update(K, fun(V1) -> mustache:render(V1, Dict) end, Dict)
    catch
        _:{badfun,_} -> Dict;
        _:Err ->
            ?debugFmt("~nErr: ~p~n", [Err]),
            Dict
     end,
    render_ctx(Rest, NewDict).
