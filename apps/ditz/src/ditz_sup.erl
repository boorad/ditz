-module(ditz_sup).
-author('brad@cloudant.com').

-behaviour(supervisor).

-export([start_link/1, init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Args) ->
    supervisor:start_link({local, ditz_sup}, ?MODULE, [Args]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init(_Args) ->
    Main =
        {ditz_main,
         {ditz_server, start_link, []},
         permanent,
         brutal_kill,
         worker,
         [ditz_main]},
    {ok, {{one_for_one, 10, 60},
          [Main]}}.


%%--------------------------------------------------------------------
%%  Internal functions
%%--------------------------------------------------------------------
