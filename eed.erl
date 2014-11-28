% EEd - a minimal Erlang based Ed.
%
% Ed tutorial: http://linuxclues.blogspot.com/2012/09/ed-tutorial-line-editor-unix.html

-module(eed).

-compile([debug_info, export_all]).

fileed(Lines, Cursor) ->
  receive
    {From, {append, Line}} ->
      From ! {self(), ok},
      {A, B} = lists:split(Cursor, Lines),
      fileed(A ++ [Line] ++ B, Cursor + 1);
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
      From ! {self(), lists:nth(Cursor, Lines)},
      fileed(Lines, Cursor);
    {From, {lineCount, _}} ->
      From ! {self(), length(Lines)},
      fileed(Lines, Cursor);
    {From, {setLineNumber, Num}} ->
      From ! {self(), ok},
      fileed(Lines, Num);
    terminate ->
      ok
  end.

start() ->
  spawn(?MODULE, fileed, [[], 0]).

append(Pid, Line) ->
  Pid ! {self(), {append, Line}},
  receive
    {Pid, Msg} -> Msg
  after 3000 -> timeout
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
  after 3000 -> timeout
  end.

list(Pid) ->
  Pid ! {self(), {list, undef}},
  receive
    {Pid, Msg} -> Msg
  after 3000 -> timeout
  end.

printall(Pid) -> io:format(list(Pid)).

print(Pid) ->
  Pid ! {self(), {print, undef}},
  receive
    {Pid, Msg} -> io:format(Msg)
  after 3000 -> timeout
  end.

lineCount(Pid) ->
  Pid ! {self(), {lineCount, undef}},
  receive
    {Pid, Msg} -> Msg
  after 3000 -> timeout
  end.

test() ->
  E = start(),
  [] = list(E),
  ok.

unknown() ->
  io:format("?~n").

prompt() ->
  "*".

chomp(String) ->
  re:replace(String, "(^\\s+)|(\\s+$)", "", [global, {return,list}]).

getnum(String) ->
  try erlang:list_to_integer(String) catch error:_Ex -> 0 end.

numValid(E, Num) ->
  (Num > 0) and (Num =< lineCount(E)).

setLineNum(Pid, Num) ->
  Pid ! {self(), {setLineNumber, Num}},
  receive
    {Pid, Msg} -> Msg
  after 3000 -> timeout
  end.

getcmd(E) ->
  L = chomp(io:get_line(prompt())),
  case L of
    "q" -> {quit, E};
    "a" -> append(E),
             getcmd(E);
    ",p" -> printall(E),
              getcmd(E);
    "p" -> print(E),
             getcmd(E);
    _ -> Num = getnum(L),
         NumValid = numValid(E, Num),
         if NumValid ->
              setLineNum(E, Num),
              getcmd(E);
            true ->
              unknown(),
              getcmd(E)
         end
  end.

run() ->
  E = start(),
  getcmd(E).

