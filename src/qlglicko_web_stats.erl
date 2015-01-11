-module(qlglicko_web_stats).

%% Cowboy REST Callback API
-export([allowed_methods/2,
         content_types_provided/2,
         get_tsv/2,
         init/3,
         rest_init/2,
         terminate/3]).

-record(state, { query_type }).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, Opts) ->
    {ok, Req, state_init(Opts)}.
    
state_init([]) -> #state { query_type = undefined };
state_init([map_count]) -> #state { query_type = map_count }.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"tab-separated-values">>, []}, get_tsv}], Req, State}.

get_tsv(Req, #state { query_type = map_count } = State) ->
    Data = qlg_db:duel_counts(),
    Output = [tsv_header(["Map", "Count"]) | tsv_data(Data)],
    {Output, age(Req), State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% Internal players
%% -------------------------------

tsv_header([]) -> $\n;
tsv_header([E]) -> [E, $\n];
tsv_header([E|Es]) -> [E, $\t | tsv_header(Es)].

tsv_data([]) -> [];
tsv_data([T|Ts]) when is_tuple(T) ->
    Record = tsv_record(tuple_to_list(T)),[tsv_element(E) || E <- tuple_to_list(T)],
    [Record, $\n | tsv_data(Ts)].
    
tsv_record([]) -> [];
tsv_record([E]) -> [tsv_element(E)];
tsv_record([E|Es]) -> [tsv_element(E), $\t | tsv_record(Es)].

tsv_element(B) when is_binary(B) -> B;
tsv_element(F) when is_float(F) -> float_to_binary(F);
tsv_element(I) when is_integer(I) -> integer_to_binary(I).
  
age(Req) ->
    cowboy_req:set_resp_header(<<"Cache-Control">>,
                               <<"public, max-age=3600">>, Req).

