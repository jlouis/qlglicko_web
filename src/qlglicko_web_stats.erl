-module(qlglicko_web_stats).

%% Cowboy REST Callback API
-export([allowed_methods/2,
         content_types_provided/2,
         get_csv/2,
         init/3,
         terminate/3,
         validate_tournament/1]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"csv">>, []}, get_csv}], Req, State}.

get_csv(InReq, State) ->
    {Tourney, Req} = cowboy_req:binding(tourney, InReq),
    {Tournament, OutReq} = cowboy_req:binding(tournament, Req),
    handle_stats(Tourney, Tournament, OutReq, State).

handle_stats(T, C, Req, State) ->
    {ok, Data} = qlg_db:tournament_stats(T, C),
    {["Player,Map,Rank,Rd,Sigma\n" | csv(Data)], age(Req), State}.

terminate(_Reason, _Req, _State) ->
    ok.

validate_tournament(T) ->
  case re:run(T, <<"^[a-zA-Z0-9_]+$">>, [{capture, none}]) of
    nomatch -> false;
    match -> true
  end.

%% Internal players
%% -------------------------------

csv([]) -> [];
csv([{P, M, R, Rd, Sigma} | Results]) ->
    [P, $,,
     M, $,,
     float_to_binary(R), $,,
     float_to_binary(Rd), $,,
     float_to_binary(Sigma),
     "\n" | csv(Results)].
  
age(Req) ->
    cowboy_req:set_resp_header(<<"Cache-Control">>,
                               <<"public, max-age=3600">>, Req).

