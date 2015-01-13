-module(qlglicko_web_stats).

%% Validators for this part
-export([validate_player/1, validate_map/1]).

%% Cowboy REST Callback API
-export([allowed_methods/2,
         content_types_provided/2,
         get_tsv/2,
         init/3,
         resource_exists/2,
         rest_init/2,
         terminate/3]).

-record(state, {
	q :: map_count | {rank, binary(), binary()},
	r = undefined :: any()
}).

%% Validators
validate_player(P) ->
  case re:run(P, <<"^[a-zA-Z0-9_]+$">>, [{capture, none}]) of
    nomatch -> false;
    match -> true
  end.
  
validate_map(<<"aerowalk">>) -> true;
validate_map(<<"bloodrun">>) -> true;
validate_map(<<"hektik">>) -> true;
validate_map(<<"battleforged">>) -> true;
validate_map(<<"campgrounds">>) -> true;
validate_map(<<"cure">>) -> true;
validate_map(<<"furiousheights">>) -> true;
validate_map(<<"houseofdecay">>) -> true;
validate_map(<<"lostworld">>) -> true;
validate_map(<<"sinister">>) -> true;
validate_map(<<"toxicity">>) -> true;
validate_map(<<"verticalvengeance">>) -> true;
validate_map(_) -> false.

%% Callbacks
init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {ok, Req, #state { q = undefined }};
rest_init(Req, [map_count]) ->
    {ok, Req, #state { q = map_count }};
rest_init(Req, [rank]) ->
    {Player, Req2} = cowboy_req:binding(player, Req),
    {Map, Req3} = cowboy_req:binding(map, Req2),
    {ok, Req3, #state { q = {rank, Player, Map} }}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"tab-separated-values">>, []}, get_tsv}], Req, State}.

resource_exists(Req, #state { q = map_count } = State) ->
    Data = qlg_db:duel_counts(),
    {true, Req, State#state { r = Data }};
resource_exists(Req, #state { q = {rank, P, M}} = State) ->
    case qlg_db:player_rankings(P, M) of
      not_found -> {false, Req, State};
      {ok, Data} -> {true, Req, State#state { r = Data }}
    end.

get_tsv(Req, #state { q = map_count, r = Data } = State) ->
    Output = [tsv:header(["Map", "Count"]) | tsv:data(Data)],
    {Output, age(Req), State};
get_tsv(Req, #state { q = {rank, _P, _M}, r = Data } = State) ->
    Output = [tsv:header(["Date", "Rank", "RankDeviation", "Sigma"]) | tsv:data(Data)],
    {Output, age(Req), State}.


terminate(_Reason, _Req, _State) ->
    ok.

%% Internal players
%% -------------------------------

age(Req) ->
    cowboy_req:set_resp_header(<<"Cache-Control">>,
                               <<"public, max-age=3600">>, Req).

