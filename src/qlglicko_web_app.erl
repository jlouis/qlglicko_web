-module(qlglicko_web_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = [
        %% {URIHost, list({URIPath, Handler, Opts})}
        {'_', [{'_', qlglicko_web_handler, []}]}
    ],
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(qlglicko_cowboy_listener, 100,
        [{port, 8080}], [{dispatch, Dispatch}]).
    qlglicko_web_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(qlglicko_cowboy_listener),
    ok.
