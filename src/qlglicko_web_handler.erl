-module(qlglicko_web_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(InReq, State) ->
    case cowboy_req:path_info(InReq) of
      {[P],  Req} when is_binary(P) ->
        handle_player(validate_player(P), Req, State);
      Other ->
        lager:debug("Got: ~p", [Other]),
        {ok, Req2} = cowboy_req:reply(404, [], <<"Not Found">>, InReq),
        {ok, Req2, State}
    end.

handle_player(invalid, Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<"Player not found">>, Req),
    {ok, Req2, State};
handle_player({valid, Player}, Req, State) ->
    {ok, Entries} = qlg_pgsql_srv:player_rank(Player),
    {ok, Streaks} = qlg_pgsql_srv:player_match_streak(Player),

    {ok, Req2} = cowboy_req:reply(200, [], jsx:to_json(format_entries(Entries, Streaks), [space, {indent, 2}]), Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
    
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
  
validate_player(P) ->
  case re:run(P, <<"^[a-zA-Z0-9_]+$">>, [{capture, none}]) of
    nomatch -> invalid;
    match -> {valid, P}
  end.
