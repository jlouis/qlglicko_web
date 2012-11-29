-module(qlglicko_web_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = [{'_', [
        {[], cowboy_static,
          [{directory, {priv_dir, qlglicko_web, [<<"www">>]}},
           {mimetypes, [{<<".html">>, [<<"text/html">>]}]},
           {etag, default},
           {file, <<"index.html">>}]},
        {[<<"static">>, '...'], cowboy_static,
          [{directory, {priv_dir, qlglicko_web, [<<"www">>, <<"static">>]}},
           {mimetypes, [{<<".js">>, [<<"text/javascript">>]}]},
           {etag, default}]},
        {[<<"player">>, '...'], qlglicko_web_handler, []}]}],

    cowboy:start_http(qlglicko_cowboy_listener, 100,
        [{port, 8080}], [{dispatch, Dispatch}]),
    qlglicko_web_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(qlglicko_cowboy_listener),
    ok.
