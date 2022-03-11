%%%-------------------------------------------------------------------
%%% @author amirhosein
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mar 2022 16:12
%%%-------------------------------------------------------------------
-module(payload_handler).
-author("amirhosein").

-include("records.hrl").

%% API
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    handle_payload/2
]).

%%%%%%%%%%%%%%%%%%%%%%%% Callbacks  %%%%%%%%%%%%%%%%%%%%%%%%

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, handle_payload}
    ], Req, State}.

% TODO: only allow GitHub to send requests from nginx
handle_payload(Req, State) ->
    Event = cowboy_req:header(<<"x-github-event">>, Req),
    Delivery = cowboy_req:header(<<"x-github-delivery">>, Req),
    check_event_type(Event, Delivery, Req, State).

%%%%%%%%%%%%%%%%%%%%%%%% Internal functions  %%%%%%%%%%%%%%%%%%%%%%%%

check_event_type(<<"push">>, Delivery, Req, State) ->
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    Json = jsx:decode(Body),
    Repository = maps:get(<<"repository">>, Json),
    RepositoryOwner = maps:get(<<"owner">>, Repository),
    HeadCommit = maps:get(<<"head_commit">>, Json),
    Submission = #submission{
        repository = #submission_repository{
            delivery_guid = Delivery,
            repository_clone_url = maps:get(<<"clone_url">>, Repository),
            repository_name = maps:get(<<"name">>, Repository),
            repository_full_name = maps:get(<<"full_name">>, Repository),
            repository_owner_login = maps:get(<<"login">>, RepositoryOwner),
            head_commit_id = maps:get(<<"id">>, HeadCommit),
            pushed_at_timestamp = maps:get(<<"pushed_at">>, Repository),
            head_commit_message = maps:get(<<"message">>, HeadCommit),
            head_commit_timestamp = maps:get(<<"timestamp">>, HeadCommit)
        }
    },
    submission_queue:enqueue(Submission),
    {true, Req, State};
check_event_type(_, _Delivery, Req, State) ->
    cowboy_req:reply(400, Req),
    {false, Req, State}.
