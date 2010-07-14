-module(httpclient).
-author('brad@cloudant.com').

-export([get/2, get/3, put/2, put/3, put/4, post/3, delete/2]).

-include_lib("eunit/include/eunit.hrl").

%% TODO: ripe for a rewrite, ffs.

get(NodeNum, Uri) ->
    get(front, NodeNum, Uri).

get(front, NodeNum, Uri) ->
    [{_Server, {NodeNum, _NodeName, Frontdoor, _Backdoor}}|_] =
        ditz_utils:get_server_node(NodeNum),
    int_get(Frontdoor, Uri);
get(back, NodeNum, Uri) ->
    [{_Server, {NodeNum, _NodeName, _Frontdoor, Backdoor}}|_] =
        ditz_utils:get_server_node(NodeNum),
    int_get(Backdoor, Uri).

put(NodeNum, Uri) ->
    put(front, NodeNum, Uri, <<"{}">>).

put(NodeNum, Uri, Payload) when is_integer(NodeNum) ->
    put(front, NodeNum, Uri, Payload);
put(front, NodeNum, Uri) ->
    put(front, NodeNum, Uri, <<"{}">>);
put(back, NodeNum, Uri) ->
    put(back, NodeNum, Uri, <<"{}">>).

put(front, NodeNum, Uri, Payload) ->
    [{_,{_,_,Front,_}}|_] = ditz_utils:get_server_node(NodeNum),
    int_put(Front, Uri, Payload);
put(back, NodeNum, Uri, Payload) ->
    [{_,{_,_,_,Back}}|_] = ditz_utils:get_server_node(NodeNum),
    int_put(Back, Uri, Payload).

post(NodeNum, Uri, Payload) ->
    post(front, NodeNum, Uri, Payload).

post(front, NodeNum, Uri, Payload) ->
    [{_Server, {_, _, Front, _}}|_] =
        ditz_utils:get_server_node(NodeNum),
    int_post(Front, Uri, Payload);
post(back, NodeNum, Uri, Payload) ->
    [{_Server, {_, _, _, Back}}|_] =
        ditz_utils:get_server_node(NodeNum),
    int_post(Back, Uri, Payload).

delete(NodeNum, Uri) ->
    delete(front, NodeNum, Uri).

delete(front, NodeNum, Uri) ->
    [{_Server, {_, _, Front, _}}|_] =
        ditz_utils:get_server_node(NodeNum),
    int_delete(Front, Uri);
delete(back, NodeNum, Uri) ->
    [{_Server, {_, _, _, Back}}|_] =
        ditz_utils:get_server_node(NodeNum),
    int_delete(Back, Uri).


%%%===================================================================
%%% Internal functions
%%%===================================================================

render_payload(P) when is_list(P) ->
    GetFromFile = string:str(P,"@") == 1,
    if
    GetFromFile -> payload_from_file(string:substr(P,2));
    true -> P
    end;
render_payload(P) when is_binary(P) -> P;
render_payload(P) -> mochijson2:encode(P).

payload_from_file(Template) ->
    Ctx = ditz_utils:get_ctx(),
    Filename = mustache:render(Template, Ctx),
    {ok, Bin} = file:read_file(Filename),
    Contents = binary_to_list(Bin),
    % TODO: call mustache:render, and solve erlang compile error on init1.json
    %mustache:render(Contents, Ctx).
    Contents.

int_get(Host, Uri) ->
    Url = Host ++ Uri,
    Raw = case ibrowse:send_req(Url, [], get) of
        {ok, "200", _Headers, Body} ->
            Body;
        Error ->
            throw(Error)
    end,
    mochijson2:decode(Raw).

int_put(Host, Uri, Payload) ->
    Ctx = ditz_utils:get_ctx(),
    Url = mustache:render(Host ++ Uri, Ctx),
    Payload1 = render_payload(Payload),
    Raw = case ibrowse:send_req(Url, [], put, Payload1) of
        {ok, "201", _Headers, Body} ->
            Body;
        {ok, "202", _Headers, Body} ->
            Body;
        Error ->
            throw(Error)
    end,
    mochijson2:decode(Raw).

int_post(Host, Uri, Payload) ->
    Url = Host ++ Uri,
    Payload1 = render_payload(Payload),
    case ibrowse:send_req(Url, [], post, Payload1) of
        {ok, "200", _Headers, Body} ->
            Body;
        Error ->
            throw(Error)
    end.

int_delete(Host, Uri) ->
    Url = Host ++ Uri,
    Raw = case ibrowse:send_req(Url, [], delete) of
        {ok, "200", _Headers, Body} ->
            Body;
        Error ->
            throw(Error)
    end,
    mochijson2:decode(Raw).
