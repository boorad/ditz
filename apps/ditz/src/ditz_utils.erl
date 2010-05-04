-module(ditz_utils).

-export([cmd_loop_thru/1]).

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
    {ok, Options} = ditz_server:options(Server),
    Ctx = full_ctx(Node, Options),
    Cmd = mustache:render(CmdTemplate, Ctx),
    ditz_server:cmd(Server, Cmd).
    %?debugFmt("~nCmd: ~s~n", [Cmd]).

% take node context and add in itests file context (options)
full_ctx(Node, Options) ->
    lists:foldl(fun({Option, Value}, AccIn) ->
        % some of the Options need mustache rendering, too
        Value1 = mustache:render(Value, AccIn),
        dict:append(Option, Value1, AccIn)
    end, node_ctx(Node), Options).

% node-specific context
node_ctx({NodeNum, NodeName, Host}) ->
    dict:from_list([{nodenum, NodeNum},
                    {nodename, NodeName},
                    {host, Host}]).
