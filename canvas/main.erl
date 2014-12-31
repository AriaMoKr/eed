-module(main).
-compile(export_all).

blocksize() -> 50.
gridcount() -> 10.

rects() -> io_lib:format("{\"x\":~p,\"y\":~p,\"w\":~p,\"h\":~p}",
  [blocksize()*(random:uniform(gridcount())-1),
   blocksize()*(random:uniform(gridcount())-1),
   blocksize(), blocksize()]).

initf() -> %"{\"blocksize\":20,\"gridcount\":20}".
  io_lib:format("{\"blocksize\":~p,\"gridcount\":~p}",
                [blocksize(), gridcount()]).
