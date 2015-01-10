-module(qlglicko_web_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    HostMatch = '_',
    RootMatch   = {"/", cowboy_static, {priv_file, qlglicko_web, "www/index.html"}},
    StaticMatch = {"/static/[...]", cowboy_static, {priv_dir, qlglicko_web, "www/static",
                    [{mimetypes, cow_mimetypes, all}]}},
    GlobalCounts = {"/stats/global/map_count", qlglicko_web_stats, [map_count]},
    StatsMatch  = {"/stats/tournament/[:tourney]/[:tournament]",
                   [{tourney, function,
                     fun qlglicko_web_stats:validate_tournament/1},
                    {tournament, int}],
                   qlglicko_web_stats, []},
    PlayerMatch = {"/player/[:player]", [{player, function,
                                          fun qlglicko_web_handler:validate_player/1}],
                                          qlglicko_web_handler, []},    
    Dispatch = cowboy_router:compile([{HostMatch, [RootMatch,
                                                   StaticMatch,
                                                   StatsMatch,
                                                   GlobalCounts,
                                                   PlayerMatch]}]),
    cowboy:start_http(qlglicko_http_listener, 100,
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]),
    qlglicko_web_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(qlglicko_cowboy_listener),
    ok.
