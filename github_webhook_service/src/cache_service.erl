%%%-------------------------------------------------------------------
%%% @author amirhosein
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cache_service).

-behaviour(gen_server).

-export([start_link/0, get_team_details/1, load_cache/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(cache_service_state, {table}).

get_team_details(RepositoryFullName) ->
    gen_server:call(?MODULE, {get_team_details, RepositoryFullName}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, Table} = load_cache(),
    {ok, #cache_service_state{table = Table}}.

handle_call({get_team_details, RepositoryFullName}, _From, State = #cache_service_state{table = Table}) ->
    [{_, TeamId, TeamName, TeamTechnology}] = ets:lookup(Table, RepositoryFullName),
    {reply, {TeamId, TeamName, TeamTechnology}, State};
handle_call(_Request, _From, State = #cache_service_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #cache_service_state{}) ->
    {noreply, State}.

handle_info(_Info, State = #cache_service_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #cache_service_state{}) ->
    ok.

code_change(_OldVsn, State = #cache_service_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_cache() ->
    {ok, DBConnection} = utility:create_database_connection(),
    {ok, _Columns, Rows} = utility:query(
        DBConnection,
        "SELECT repository_full_name, id, name, technology FROM team",
        []
    ),
    Table = ets:new(cache_service_table, []),
    case ets:insert(Table, Rows) of
        true -> {ok, Table};
        false -> lager:error("ETS table insertion failed"), error
    end.