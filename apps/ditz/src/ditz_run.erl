-module(ditz_run).
-author('brad@cloudant.com').

-export([run/0, run/1, run/2]).

-include_lib("eunit/include/eunit.hrl").

-define(DEV, true).

%%%===================================================================
%%% API
%%%===================================================================

run() ->
    % TODO: cloudant-only convenience, remove at some point
    run("cloudant").

% run tests
% assumes all servers have ditz running
run(Suite) when is_atom(Suite) ->
    run(atom_to_list(Suite));
run(Suite) ->
    run(Suite, "*").

run(Suite, FilePattern) when is_atom(Suite) ->
    run(atom_to_list(Suite), FilePattern);
run(Suite, FilePattern) ->
    io:format("~nditz starting...~n", []),
    TopDir = check_path(code:priv_dir(ditz)) ++ Suite ++ "/",
    TestDir = TopDir ++ "tests/",
    {ok, Filenames} = file:list_dir(TestDir),
    Testfiles = lists:foldl(fun(Filename, AccIn) ->
        case FilePattern of
        "*" -> [TestDir ++ Filename | AccIn];
        Start ->
            case string:str(Filename, Start) of
            1 -> [TestDir ++ Filename | AccIn];
            _ -> AccIn
            end
        end
    end, [], Filenames),
    run_tests(TopDir, lists:sort(Testfiles)).


%%%===================================================================
%%% Internal Functions
%%%===================================================================

output_test(File, Name, Desc) ->
    io:format("Test name: ~p~n", [Name]),
    io:format("Test file: ~p~n", [File]),
    io:format("Test desc: ~p~n", [Desc]).

init_servers([]) -> ok;
init_servers([Server|Rest]) ->
    init_server(Server),
    init_servers(Rest).

init_server({Server, ServerConfig}) ->
    case net_adm:ping(Server) of
    pong -> ok;
    pang -> throw({error, {ping_error, Server}})
    end,
    NodeList = proplists:get_value(nodelist, ServerConfig),
    ditz_server:nodelist(Server, NodeList),
    Options = proplists:get_value(options, ServerConfig),
    ditz_server:options(Server, Options).

run_tests(_TopDir, []) -> ok;
run_tests(TopDir, [TestFile|Rest]) ->
    % read test file
    {ok, [Test]} = file:consult(TestFile),
    {TestName, TestDesc, ConfigFile, Tasks0} = Test,
    output_test(TestFile, TestName, TestDesc),
    {ok, Config} = do_config(TopDir, ConfigFile),
    {ok, Tasks} = setup_teardown(TopDir, Tasks0),
    ok = run_test(Config, Tasks),
    io:format("Test completed: ~p~n~n", [TestName]),
    run_tests(TopDir, Rest).

run_test(_Config, []) ->
    ok;
 run_test(Config, [Task|Rest]) ->
     io:format("Task: ~p~n", [Task]),
     process_task(Task),
     run_test(Config, Rest).

process_task({{M,F,A}, notest}) ->
    apply(M,F,A);
process_task({{M,F,A}, equal, Result}) ->
    ?assertEqual(Result, apply(M,F,A));
process_task({{M,F,A}, match, Result}) ->
    ?assertMatch(Result, apply(M,F,A));
process_task({{M,F,A}, same_elems, Result}) when is_list(Result) ->
    Actual = apply(M,F,A),
    true = is_list(Actual),
    ?assertEqual(lists:sort(Actual), lists:sort(Result)).

check_path(Dir) ->
    case string:rchr(Dir, $/) == length(Dir) of
    true -> Dir;
    _ -> Dir ++ "/"
    end.

do_config(TopDir, File) when is_atom(File) ->
    do_config(TopDir, atom_to_list(File));
do_config(TopDir, File) ->
    ConfigFile = TopDir ++ "config/" ++ File,
    {ok, Config} = file:consult(ConfigFile),
    Servers = proplists:get_value(servers, Config),
    {init_servers(Servers), Config}.

setup_teardown(Dir, Tasks0) ->
    Tasks = lists:foldl(fun
        ({setup, Setup}, Acc0) ->
            expand_tasks(Dir, "setup", Setup, Acc0);
        ({teardown, Teardown}, Acc0) ->
            expand_tasks(Dir, "teardown", Teardown, Acc0);
        (Task, Acc0) -> [Task | Acc0]
    end, [], Tasks0),
    {ok, lists:reverse(Tasks)}.

expand_tasks(Dir, Type, File, Acc0) ->
    Filename = io_lib:format("~s~s/~s", [Dir, Type, File]),
    {ok, [Tasks0]} = file:consult(Filename),
    lists:foldl(fun(T,Acc) -> [T|Acc] end, Acc0, Tasks0).
