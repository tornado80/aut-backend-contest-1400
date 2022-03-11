%%%-------------------------------------------------------------------
%% @doc github_webhook_service public API
%% @end
%%%-------------------------------------------------------------------

-module(github_webhook_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    start_cowboy(),
    github_webhook_service_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/payload", payload_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(
        github_webhook_payload_web_service,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ).