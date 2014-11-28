% EEd - a minimal Erlang based Ed.
%
% Ed tutorial: http://linuxclues.blogspot.com/2012/09/ed-tutorial-line-editor-unix.html

-module(eed).

-compile([debug_info, export_all]).

fileed(Lines, Cursor) ->
  receive
    {From, {append, Line}} ->
      From ! {self(), ok},
      fileed(Lines ++ [Line], Cursor + 1);
    {From, {pop, _}} ->
      From ! {self(), ok},
      fileed(lists:sublist(Lines, length(Lines)-1), Cursor);
    {From, {list, _}} ->
      From ! {self(), Lines},
      fileed(Lines, Cursor);
    {From, {dump, _}} ->
      From ! {self(), {Lines, Cursor}},
      fileed(Lines, Cursor);
    {From, {print, _}} ->
      From ! {self(), Lines},
      fileed(Lines, Cursor);
  terminate ->
      ok
  end.

start() ->
  spawn(?MODULE, fileed, [[], 0]).

append(Pid, Line) ->
  Pid ! {self(), {append, Line}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.

append(Pid) ->
  L = io:get_line(""),
  case L of
    ".\n" -> quit;
    _ -> append(Pid, L),
         append(Pid)
  end.

pop(Pid) ->
  Pid ! {self(), {pop, undef}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.

list(Pid) ->
  Pid ! {self(), {list, undef}},
  receive
    {Pid, Msg} -> Msg
  after 3000 ->
    timeout
  end.

printall(Pid) -> io:format(list(Pid)).

print(Pid) ->
  Pid ! {self(), {print, undef}},
  receive
    {Pid, Msg} -> io:format(Msg)
  after 3000 ->
    timeout
  end.

test() ->
  E = start(),
  [] = list(E),
  ok.

getcmd(E) ->
  L = io:get_line("*"),
  case L of
    "q\n" -> {quit, E};
    "a\n" -> append(E),
             getcmd(E);
    ",p\n" -> printall(E),
              getcmd(E);
    "p\n" -> print(E),
             getcmd(E);
    _ -> io:format("?~n"),
         getcmd(E)
  end.

run() ->
  E = start(),
  getcmd(E).

