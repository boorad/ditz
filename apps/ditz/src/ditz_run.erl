-module(ditz_run).
-author('brad@cloudant.com').

-export([run/0, run/1]).

-include_lib("eunit/include/eunit.hrl").

-define(DEV, true).

%%%===================================================================
%%% API
%%%===================================================================

run() ->
    Conf = code:priv_dir(ditz) ++ "/../../../conf", % hack
    run(Conf ++ "/cloudant/cloudant.itests").

run(TestFile) ->
    % read setup/test file
    {ok, Setup} = file:consult(TestFile),
    io:format("~nditz starting...~n", []),
    io:format("Tests file: ~p~n", [TestFile]),
    Servers = proplists:get_value(servers, Setup),
    Tests = proplists:get_value(tests, Setup),

    % start ditz servers, run tests, stop ditz servers
    ok = start_servers(Servers),
    %timer:sleep(1000), % let servers start up
    run_tests(Tests, Servers),
    ok = stop_servers(Servers).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

start_servers([]) -> ok;
start_servers([Server|Rest]) ->
    start_server(Server),
    start_servers(Rest).

start_server({Server, Setup}) ->
    pong = net_adm:ping(Server),
    NodeList = proplists:get_value(nodelist, Setup),
    ditz_server:nodelist(Server, NodeList),
    Options = proplists:get_value(options, Setup),
    ditz_server:options(Server, Options).

stop_servers([]) -> ok;
stop_servers([Server|Rest]) ->
    stop_server(Server),
    stop_servers(Rest).

stop_server({Server, _Options}) ->
    ditz_server:stop(Server).

run_tests([], _Servers) -> ok;
run_tests([Test|Rest], Servers) ->
    {TestName, TestDesc, Tasks} = Test,
    io:format("~nTest '~p' starting~n  ~s~n", [TestName, TestDesc]),
    run_test(TestName, Tasks),
    run_tests(Rest, Servers).

run_test(TestName, []) ->
    io:format("Test '~p' completed.~n~n", [TestName]);
run_test(TestName, [Task|Rest]) ->
    io:format("Task: ~p~n", [Task]),
    process_task(Task),
    run_test(TestName, Rest).

process_task({{M,F,A}, equal, Result}) ->
    ?assertEqual(Result, M:F(A));
process_task({{M,F,A}, match, Result}) ->
    ?assertMatch(Result, M:F(A)).
