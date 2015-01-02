-module(main).
-compile(export_all).

blocksize() -> 50.
gridcount() -> 10.

rect() -> 
  random:seed(now()),
  io_lib:format("{\"x\":~p,\"y\":~p,\"w\":~p,\"h\":~p}",
  [blocksize()*(random:uniform(gridcount())-1),
   blocksize()*(random:uniform(gridcount())-1),
   blocksize(), blocksize()]).

init() -> 
  io_lib:format("{\"blocksize\":~p,\"gridcount\":~p}",
                [blocksize(), gridcount()]).

start() ->
  yaws:start_embedded(".", [{listen, {0,0,0,0}}, {port, 8080}],
                      [{cache_refresh_secs, 0}]).
