-module(ditz).
-author('brad@cloudant.com').

-export([start/0, start_dev/0, stop/0, restart/0, run/0]).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(ditz).

start_dev() ->
    application:start(sasl),
    application:start(ibrowse),
    application:start(ditz).

stop() ->
    application:stop(ditz).

restart() ->
    stop(),
    start().

run() ->
    ditz_run:run().
