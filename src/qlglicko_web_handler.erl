-module(qlglicko_web_handler).

%% Cowboy REST Callback API
-export([allowed_methods/2,
         content_types_provided/2,
         get_json/2,
         init/3,
         terminate/3,
         validate_player/1]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

get_json(InReq, State) ->
    {Player, Req} = cowboy_req:binding(player, InReq),
    handle_player_json(Player, Req, State).

handle_player_json(Player, Req, State) ->
    {ok, Data} = qlg_db:player_stats(Player),
    {jsx:to_json(format_entries(
                   proplists:get_value(entries, Data),
                   proplists:get_value(streaks, Data)),
                 [space, {indent, 2}]),
     age(Req), State}.

terminate(_Reason, _Req, _State) ->
    ok.

validate_player(P) ->
  case re:run(P, <<"^[a-zA-Z0-9_]+$">>, [{capture, none}]) of
    nomatch -> false;
    match -> true
  end.

%% Internal players
%% -------------------------------

format_entries(Rank, Streak) ->
  Streaks = format_streaks(Streak),
  format_rank(Rank, Streaks).

format_rank(Entries, Streaks) ->
  R = sofs:relation([{Map, {T, R, RD}} || {T, Map, R, RD} <- Entries]),
  F = sofs:relation_to_family(R),
  [{Map, [
  	{<<"rank">>, order_ranks(Tourneys)},
  	{<<"streak">>, proplists:get_value(Map, Streaks, [])}]} || {Map, Tourneys} <- sofs:to_external(F)].

format_streaks(Ss) ->
  R = sofs:relation([{Map, {Res, Played}} || {Map, Res, Played} <- Ss]),
  F = sofs:relation_to_family(R),
  [{Map, order_streaks(Matches)} || {Map, Matches} <- sofs:to_external(F)].
  
order_ranks(Tourneys) ->
  [ [{<<"rank">>, R},
     {<<"rank_deviation">>, RD}] || {_, R, RD} <- lists:keysort(1, Tourneys) ].

order_streaks(Matches) ->
  [F || {F, _} <- lists:reverse(lists:keysort(2, Matches)) ].
  
age(Req) ->
    cowboy_req:set_resp_header(<<"Cache-Control">>,
                               <<"public, max-age=3600">>, Req).

