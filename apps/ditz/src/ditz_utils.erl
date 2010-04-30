-module(ditz_utils).

-export([cmd_loop_thru/3]).

-include_lib("eunit/include/eunit.hrl").


%% loops thru Servers & the Nodes on each Server, executing Cmd template, and
%% filling in the values with Options
cmd_loop_thru(CmdTemplate, OptionNames, CtxFun) ->
    ServersNodes = servers_nodes(),
    Results = lists:map(fun(ServerNode) ->
        build_cmd(ServerNode, CmdTemplate, OptionNames, CtxFun)
    end, ServersNodes),
    case lists:usort(Results) of
    [ok] -> ok;
    Other -> Other
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

servers_nodes() ->
    Servers = lists:append([node()], nodes()),
    lists:flatmap(fun(Server) ->
        {ok, NodeList} = ditz_server:nodelist(Server),
        lists:map(fun(Node) -> {Server, Node} end, NodeList)
    end, Servers).

build_cmd({Server, Node}, CmdTemplate, OptionNames, CtxFun) ->
    {ok, Options} = ditz_server:options(Server),
    OptionValues = lists:map(fun(Option) ->
        OptionTemplate = proplists:get_value(Option, Options),
        mustache:render(OptionTemplate, CtxFun(Node))
    end, OptionNames),
    Cmd = io_lib:format(CmdTemplate, OptionValues),
    %ditz_server:cmd(Server, Cmd).
    ?debugFmt("~nCmd: ~s~n", [Cmd]).
