%%%-------------------------------------------------------------------
%%% @author amirhosein
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2022 21:55
%%%-------------------------------------------------------------------
-module(utility).
-author("amirhosein").

%% API
-export([create_database_connection/0, query/3]).

create_database_connection() ->
    DBCredentialsFile = "database_credentials.config",
    {ok, DBCredentials} = file:consult(DBCredentialsFile),
    case epgsql:connect(maps:from_list(DBCredentials)) of
        {error, Reason} -> lager:error("Database connection error of ~p", [Reason]), error;
        Other -> Other
    end.

query(DBConnection, Query, Parameters) ->
    case epgsql:equery(DBConnection, Query, Parameters) of
        {error, Error} -> lager:error("Database query error: ~p when query was ~p", [Error, Query]), error;
        Other -> Other
    end.