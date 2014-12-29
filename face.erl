-module(face).
-compile(export_all).

start() -> yaws:start_embedded(".", [{listen, {0,0,0,0}}, {port, 8080}], [{cache_refresh_secs, 0}]).

