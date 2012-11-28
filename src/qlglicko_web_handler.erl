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
    
    {ok, Req2} = cowboy_req:reply(200, [], jsx:to_json(format_entries(Entries), [space, {indent, 2}]), Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
    
%% Internal players
%% -------------------------------

format_entries([]) -> null;
format_entries([{Map, R, RD} | Next]) ->
  [{Map, [{<<"rank">>, round(R)}, {<<"rank_deviaton">>, round(RD)}]} | format_entries(Next)].

validate_player(P) ->
  case re:run(P, <<"^[a-zA-Z0-9_]+$">>, [{capture, none}]) of
    nomatch -> invalid;
    match -> {valid, P}
  end.
