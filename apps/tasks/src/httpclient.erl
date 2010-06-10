-module(httpclient).
-author('brad@cloudant.com').

-export([get/1, post/1]).

-include_lib("eunit/include/eunit.hrl").

get([NodeNum, Uri]) ->
    {_Server, {NodeNum, _NodeName, NodeHost}} =
        ditz_utils:get_server_node(NodeNum),
    Url = NodeHost ++ Uri,
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _Headers, Body} ->
            Body;
        Error ->
            throw(Error)
    end.

post([NodeNum, Uri, Payload]) ->
    {NodeNum, _NodeName, NodeHost} = ditz_utils:get_node(NodeNum),
    Url = NodeHost ++ Uri,
    Payload1 = render_payload(Payload),
    case ibrowse:send_req(Url, [], post, Payload1) of
        {ok, "200", _Headers, Body} ->
            Body;
        Error ->
            throw(Error)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

render_payload(P) ->
    GetFromFile = string:str(P,"@") == 1,
    if
    GetFromFile -> payload_from_file(string:substr(P,2));
    true -> P
    end.

payload_from_file(Template) ->
    Ctx = ditz_utils:get_ctx(),
    Filename = mustache:render(Template, Ctx),
    {ok, Bin} = file:read_file(Filename),
    Contents = binary_to_list(Bin),
    % TODO: call mustache:render, and solve erlang compile error on init1.json
    %mustache:render(Contents, Ctx).
    Contents.
