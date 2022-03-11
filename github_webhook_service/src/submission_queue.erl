%%%-------------------------------------------------------------------
%%% @author amirhosein
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(submission_queue).

-behaviour(gen_server).

-export([start_link/0, enqueue/1, insert_submission_done/1, judge/1, reload_from_db/0]).
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

reload_from_db() ->
    gen_server:cast(?MODULE, reload_from_db).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #submission_queue_state{
        queue = [],
        current = none}}.

handle_call(_Request, _From, State = #submission_queue_state{}) ->
    {reply, ok, State}.

handle_cast({insert_submission_done, Submission},
        #submission_queue_state{
            current = Current,
            queue = Queue}) ->
    {NewCurrent, NewQueue} = case Current of
        none -> spawn(submission_queue, judge, [Submission]), {Submission, Queue};
        _Other -> {Current, Queue ++ [Submission]}
    end,
    NewState = #submission_queue_state{
        current = NewCurrent,
        queue = NewQueue
    },
    {noreply, NewState};
handle_cast({new_submission, Submission}, State) ->
    %NewSubmission = fill_submission_with_team_id(Submission),
    database_service:insert_submission(Submission),
    {noreply, State};
handle_cast({judge_done, Status, Submission},
        #submission_queue_state{
            current = _Current,
            queue = Queue}) ->
    database_service:update_score(Status, Submission),
    [NewCurrent | NewQueue] = Queue,
    NewState = #submission_queue_state{
        current = NewCurrent,
        queue = NewQueue
    },
    spawn(submission_queue, judge, [NewCurrent]),
    {noreply, NewState};
handle_cast(reload_from_db, State = #submission_queue_state{}) ->
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

fill_submission_with_team_id(Submission = #submission{repository = Repository}) ->
    {TeamId, TeamName, TeamTechnology} = cache_service:get_team_details(Submission),
    #submission{
        repository = Repository,
        team = #submission_team{
            team_id = TeamId,
            team_name = TeamName,
            team_technology = TeamTechnology
        },
        score = 0
    }.

judge(#submission{
    repository = Repository = #submission_repository{
        delivery_guid = Delivery,
        repository_clone_url = RepositoryCloneUrl,
        repository_name = RepositoryName,
        head_commit_id = HeadCommitId
    },
    team = Team = #submission_team{
        team_technology = Technology
    },
    score = Score}) ->
    Command = io_lib:format("conda run -n judge_environment python3 main.py ~p ~p ~p ~p ~p",
        [<<"django">>, Delivery, RepositoryCloneUrl, RepositoryName, HeadCommitId]),
    Result = os:cmd(Command),
    Score = maps:get(<<"score">>, jsx:decode(file:read_file("result.json"))),
    NewSubmission = #submission{
        repository = Repository,
        team = Team,
        score = Score
    },
    judge_done(Status, NewSubmission).

judge_done(Status, Submission) ->
    gen_server:cast(?MODULE, {judge_done, Status, Submission}).