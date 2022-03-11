%%%-------------------------------------------------------------------
%%% @author amirhosein
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(submission_queue).

-behaviour(gen_server).

-export([start_link/0, enqueue/1, insert_submission_done/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-include("records.hrl").

-record(submission_queue_state, {current, queue}).

%%%===================================================================
%%% Public functions
%%%===================================================================

enqueue(Submission) ->
    gen_server:cast(?MODULE, {new_submission, Submission}).

insert_submission_done(Submission) ->
    gen_server:cast(?MODULE, {insert_submission_done, Submission}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #submission_queue_state{queue = [], current = none}}.

handle_call(_Request, _From, State = #submission_queue_state{}) ->
    {reply, ok, State}.

handle_cast({insert_submission_done, Submission},
        State = #submission_queue_state{current = Current, queue = Queue}) ->

    {noreply, State};
handle_cast({new_submission, Submission},
        State = #submission_queue_state{current = Current, queue = Queue}) ->
    #submission{
        delivery_guid = Delivery,
        repository_clone_url = RepositoryCloneUrl,
        repository_name = RepositoryName,
        repository_full_name = RepositoryFullName,
        repository_owner_login = RepositoryOwnerLogin,
        head_commit_id = HeadCommitId,
        pushed_at_timestamp = PushedAtTimestamp,
        head_commit_message = HeadCommitMessage,
        head_commit_timestamp = HeadCommitTimestamp
    } = Submission,
    %{TeamId, TeamName, TeamTechnology} = cache_service:get_team_details(Submission),
    database_service:insert_submission(Submission),
    {noreply, State};
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
