-module(qlglicko_cors_policy).

-export([policy_init/1]).
-export([allowed_origins/2, allow_credentials/2, exposed_headers/2, allowed_headers/2, allowed_methods/2]).

policy_init(Req) ->
    {ok, Req, undefined_state}.

allowed_origins(Req, State) ->
    {[<<"http://qlglicko.org">>, <<"http://www.qlglicko.org">>, <<"null">>], Req, State}.

allow_credentials(Req, State) ->
    {true, Req, State}.

exposed_headers(Req, State) ->
    {[<<"x-exposed">>], Req, State}.

allowed_headers(Req, State) ->
    {[<<"x-requested">>], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.