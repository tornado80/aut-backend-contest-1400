%%%-------------------------------------------------------------------
%%% @author amirhosein
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(database_service).

-behaviour(gen_server).

-export([start_link/0, insert_submission/1, update_score/2, get_submissions_in_queue/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-include("records.hrl").

-record(database_service_state, {db_connection}).

insert_submission(Submission) ->
    gen_server:cast(?MODULE, {insert_submission, Submission}).

update_score(Status, Submission) ->
    gen_server:cast(?MODULE, {update_score, Status, Submission}).

get_submissions_in_queue() ->
    gen_server:call(?MODULE, get_submissions_in_queue).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, DBConnection} = utility:create_database_connection(),
    {ok, #database_service_state{db_connection = DBConnection}}.

handle_call(get_submissions_in_queue, _From, State = #database_service_state{db_connection = DBConnection}) ->
    {ok, _Columns, Rows} = utility:query(
        DBConnection,
        "SELECT delivery_guid, repository_full_name, repository_name, repository_clone_url,
            repository_owner_login, head_commit_id, head_commit_message, head_commit_timestamp,
            pushed_at_timestamp, technology
            FROM submission as s, team as t WHERE s.repository_full_name = t.repository_full_name AND status = $1",
        [<<"queue">>]
    ),
    Submissions = convert_rows_to_submissions(Rows, []),
    {reply, Submissions, State};
handle_call(_Request, _From, State = #database_service_state{}) ->
    {reply, ok, State}.

handle_cast({update_score, Status, Submission}, State = #database_service_state{db_connection = DBConnection}) ->
    #submission{
        repository = #submission_repository{
            delivery_guid = Delivery
        },
        score = Score
    } = Submission,
    utility:query(
        DBConnection,
        "UPDATE submission SET status = $1 AND score = $2 WHERE delivery_guid = $3",
        [Status, Score, Delivery]
    ),
    {noreply, State};
handle_cast({insert_submission, Submission}, State = #database_service_state{db_connection = DBConnection}) ->
    #submission{
        repository = #submission_repository{
            delivery_guid = Delivery,
            repository_clone_url = RepositoryCloneUrl,
            repository_name = RepositoryName,
            repository_full_name = RepositoryFullName,
            repository_owner_login = RepositoryOwnerLogin,
            head_commit_id = HeadCommitId,
            pushed_at_timestamp = PushedAtTimestamp,
            head_commit_message = HeadCommitMessage,
            head_commit_timestamp = HeadCommitTimestamp
        },
        team = #submission_team{
            team_id = TeamId
        }
    } = Submission,
    Result = utility:query(
        DBConnection,
        "INSERT INTO submission (
            delivery_guid,
            repository_full_name,
            repository_name,
            repository_clone_url,
            repository_owner_login,
            head_commit_id,
            head_commit_message,
            head_commit_timestamp,
            pushed_at_timestamp,
            status,
            team_id
        ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)",
        [
            Delivery,
            RepositoryFullName,
            RepositoryName,
            RepositoryCloneUrl,
            RepositoryOwnerLogin,
            HeadCommitId,
            HeadCommitMessage,
            HeadCommitTimestamp,
            PushedAtTimestamp,
            <<"queue">>,
            TeamId
        ]
    ),
    case Result of
        {ok, _Count} -> submission_queue:insert_submission_done(Submission);
        Other -> Other
    end,
    {noreply, State};
handle_cast(_Request, State = #database_service_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #database_service_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #database_service_state{}) ->
    ok.

code_change(_OldVsn, State = #database_service_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

convert_rows_to_submissions([], Result) -> Result;
convert_rows_to_submissions([Row | Tail], Result) ->
    {
        Delivery,
        RepositoryFullName,
        RepositoryName,
        RepositoryCloneUrl,
        RepositoryOwnerLogin,
        HeadCommitId,
        HeadCommitMessage,
        HeadCommitTimestamp,
        PushedAtTimestamp,
        Technology
    } = Row,
    Submission = #submission {
        repository = #submission_repository{
            delivery_guid = Delivery,
            repository_clone_url = RepositoryCloneUrl,
            repository_name = RepositoryName,
            repository_full_name = RepositoryFullName,
            repository_owner_login = RepositoryOwnerLogin,
            head_commit_id = HeadCommitId,
            pushed_at_timestamp = PushedAtTimestamp,
            head_commit_message = HeadCommitMessage,
            head_commit_timestamp = HeadCommitTimestamp
        },
        team = #submission_team{
            team_technology = Technology
        }
    },
    convert_rows_to_submissions(Tail, [Submission | Result]).