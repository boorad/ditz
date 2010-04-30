-module(httpc).

-export([get/1]).

get([_Node, _Uri]) ->
    Url = "http://node2.boorad.local:5985/_cloudant/membership",
    case ibrowse:send_req(Url, [], get) of
        {ok, "200", _Headers, Body} ->
            Body;
        Error ->
            throw(Error)
    end.
