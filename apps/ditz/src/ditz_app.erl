-module(ditz_app).
-author('brad@cloudant.com').

-behaviour(application).

-include_lib("eunit/include/eunit.hrl").

%% Application callbacks
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    ditz_sup:start_link([]).

stop(_State) ->
    ok.
