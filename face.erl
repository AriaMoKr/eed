-module(face).
-compile(export_all).

start() -> yaws:start_embedded(".", [{port, 8080}], [{cache_refresh_secs, 0}]).

