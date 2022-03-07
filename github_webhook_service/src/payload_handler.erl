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

handle_payload(Req, State) ->
    {Ip, Port} = cowboy_req:peer(Req),
    Headers = cowboy_req:headers(Req),
    Event = cowboy_req:header(<<"x-github-event">>, Req),
    Delivery = cowboy_req:header(<<"x-github-delivery">>, Req),
    {ok, Body, _Req} = cowboy_req:read_body(Req),
    Json = convert_to_json(Body),
    io:format("~nIP:Port: ~p:~p~nHeaders: ~p~nEvent: ~p~nDelivery: ~p~nBody: ~p~nJson: ~p~n", [Ip, Port, Headers, Event, Delivery, Body, Json]),
    {true, Req, State}.

convert_to_json(Body) ->
    case jsx:is_json(Body) of
        true -> jsx:decode(Body);
        false -> undefined
    end.