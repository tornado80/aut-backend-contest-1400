%%%-------------------------------------------------------------------
%%% @author amirhosein
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(submission_queue).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(submission_queue_state, {status, queue}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #submission_queue_state{}}.

handle_call(_Request, _From, State = #submission_queue_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #submission_queue_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #submission_queue_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #submission_queue_state{}) ->
    ok.

code_change(_OldVsn, State = #submission_queue_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
