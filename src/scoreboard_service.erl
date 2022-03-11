%%%-------------------------------------------------------------------
%%% @author amirhosein
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(scoreboard_service).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(scoreboard_service_state, {}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #scoreboard_service_state{}}.

handle_call(_Request, _From, State = #scoreboard_service_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #scoreboard_service_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #scoreboard_service_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #scoreboard_service_state{}) ->
    ok.

code_change(_OldVsn, State = #scoreboard_service_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
