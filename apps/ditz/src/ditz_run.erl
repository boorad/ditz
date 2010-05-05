-module(ditz_run).
-author('brad@cloudant.com').

-export([run/0, run/1]).

-include_lib("eunit/include/eunit.hrl").

-define(DEV, true).

%%%===================================================================
%%% API
%%%===================================================================

run() ->
    Conf = code:priv_dir(ditz) ++ "/conf",
    run(Conf ++ "/cloudant/cloudant.itests").

% run tests
% assumes all servers have ditz running
run(TestFile) ->
    % read setup/test file
    {ok, Setup} = file:consult(TestFile),
    io:format("~nditz starting...~n", []),
    io:format("Tests file: ~p~n", [TestFile]),
    Servers = proplists:get_value(servers, Setup),
    Tests = proplists:get_value(tests, Setup),

    % init ditz servers & run tests
    ok = init_servers(Servers),
    run_tests(Tests, Servers).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

init_servers([]) -> ok;
init_servers([Server|Rest]) ->
    init_server(Server),
    init_servers(Rest).

init_server({Server, Setup}) ->
    pong = net_adm:ping(Server),
    NodeList = proplists:get_value(nodelist, Setup),
    ditz_server:nodelist(Server, NodeList),
    Options = proplists:get_value(options, Setup),
    ditz_server:options(Server, Options).

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
