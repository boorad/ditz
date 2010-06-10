-module(ditz_server).
-author('brad@cloudant.com').

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, stop/1,
         nodelist/0, nodelist/1, nodelist/2,
         options/0, options/1, options/2,
         cmd/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).

-record(state, {nodelist=[], options=[]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

stop(Server) ->
    gen_server:cast({ditz_server, Server}, stop).

nodelist() ->
    nodelist(node()).

nodelist(Server) ->
    gen_server:call({ditz_server, Server}, nodelist).

nodelist(Server, NodeList) ->
    gen_server:call({ditz_server, Server}, {nodelist, NodeList}).

options() ->
    options(node()).

options(Server) ->
    gen_server:call({ditz_server, Server}, options).

options(Server, Options) ->
    gen_server:call({ditz_server, Server}, {options, Options}).

cmd(Server, Cmd) ->
    gen_server:call({ditz_server, Server}, {cmd, Cmd}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

% init
init([]) ->
    {ok, #state{}}.

% handle_call
handle_call(nodelist, _From, #state{nodelist=NodeList} = State) ->
    {reply, {ok, NodeList}, State};
handle_call({nodelist, NodeList}, _From, State) ->
    NewState = State#state{nodelist=NodeList},
    {reply, ok, NewState};
handle_call(options, _From, #state{options=Options} = State) ->
    {reply, {ok, Options}, State};
handle_call({options, Options}, _From, State) ->
    NewState = State#state{options=Options},
    {reply, ok, NewState};
handle_call({cmd, Cmd}, _From, State) ->
    Result = cmd_cleanup(Cmd),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

% handle_cast
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

% handle_info
handle_info(_Info, State) ->
    {noreply, State}.

% terminate
terminate(_Reason, _State) ->
    ok.

% code_change
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

cmd_cleanup(Cmd) ->
    case os:cmd(Cmd) of
    [] -> ok;
    Other -> maybe_ok(Other)
    end.

maybe_ok(Result) ->
    % some results that can also return ok
    OkWithExtra = string:str(Result,"ok:") > 0,
    RmNotFound = string:str(Result,"rm") > 0 andalso
        string:str(Result, "No such file or directory\n") > 0,
    KillNotFound = string:str(Result,"kill") > 0,

    % final if
    if
    OkWithExtra -> ok;
    RmNotFound -> ok;
    KillNotFound -> ok;
    true -> Result
    end.
