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
    Submissions = database_service:get_submissions_in_queue(),
    lists:foreach(
        fun(Submission) ->
            gen_server:cast(?MODULE, {insert_submission_done, Submission})
        end,
        Submissions).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    filelib:ensure_dir("/home/amirhosein/judge/"),
    filelib:ensure_dir("/home/amirhosein/uploads/"),
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
        none ->
            spawn_judge(Submission),
            {Submission, Queue};
        _Other ->
            io:format("Inserting submission in queue~n"),
            {Current, Queue ++ [Submission]}
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
    io:format("Judge result received, sending results to database~n"),
    database_service:update_score(Status, Submission),
    [NewCurrent | NewQueue] = Queue,
    NewState = #submission_queue_state{
        current = NewCurrent,
        queue = NewQueue
    },
    spawn_judge(NewCurrent),
    {noreply, NewState};
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

spawn_judge(Submission) ->
    io:format("Spawning the judge process for respository ~p and submission ~p~n",
        [
            Submission#submission.repository#submission_repository.repository_full_name,
            Submission#submission.repository#submission_repository.delivery_guid
        ]),
    spawn(submission_queue, judge, [Submission]).

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

judge(
    #submission{
        repository = Repository = #submission_repository{
            delivery_guid = Delivery,
            repository_clone_url = RepositoryCloneUrl,
            repository_name = RepositoryName,
            head_commit_id = HeadCommitId
        },
        team = Team = #submission_team{
            team_technology = Technology
        },
        score = Score}
    ) ->
    Command = io_lib:format("./judge.sh ~s ~s ~s ~s ~s",
        [Technology, Delivery, RepositoryCloneUrl, RepositoryName, HeadCommitId]),
    file:write_file("result.json", jsx:encode(#{<<"score">> => 0})),
    Result = os:cmd(lists:flatten(Command)),
    {ok, Content} = file:read_file("result.json"),
    Score = maps:get(<<"score">>, jsx:decode(Content)),
    NewSubmission = #submission{
        repository = Repository,
        team = Team,
        score = Score
    },
    case Score < 0 of
        true -> judge_done(<<"failed">>, Result, NewSubmission);
        false -> judge_done(<<"judged">>, Result, NewSubmission)
    end.

judge_done(Status, Result, Submission) ->
    gen_server:cast(?MODULE, {judge_done, Status, Result, Submission}).
