%%%-------------------------------------------------------------------
%%% @author amirhosein
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(database_service).

-behaviour(gen_server).

-export([start_link/0, insert_submission/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-include("records.hrl").

-record(database_service_state, {db_connection}).

insert_submission(Submission) ->
    gen_server:cast(?MODULE, {insert_submission, Submission}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, DBConnection} = utility:create_database_connection(),
    {ok, #database_service_state{db_connection = DBConnection}}.

handle_call(_Request, _From, State = #database_service_state{}) ->
    {reply, ok, State}.

handle_cast({insert_submission, Submission}, State = #database_service_state{db_connection = DBConnection}) ->
    #submission{
        delivery_guid = Delivery,
        repository_clone_url = RepositoryCloneUrl,
        repository_name = RepositoryName,
        repository_full_name = RepositoryFullName,
        repository_owner_login = RepositoryOwnerLogin,
        head_commit_id = HeadCommitId,
        pushed_at_timestamp = PushedAtTimestamp,
        head_commit_message = HeadCommitMessage,
        head_commit_timestamp = HeadCommitTimestamp,
        team_id = TeamId
    } = Submission,
    {ok, _Count} = utility:query(
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
    submission_queue:insert_submission_done(Submission),
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
