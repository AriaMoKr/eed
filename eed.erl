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
    {From, {delete, _}} ->
      From ! {self(), ok},
      {A, [_|B]} = lists:split(Cursor-1, Lines),
      fileed(A ++ B, Cursor);
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

sendRecv(Pid, Cmd) ->
  sendRecv(Pid, Cmd, undef).
sendRecv(Pid, Cmd, Arg) ->
  Pid ! {self(), {Cmd, Arg}},
  receive {Pid, Msg} -> Msg
  after 2000 -> timeout
  end.

append(Pid) ->
  L = io:get_line(""),
  case L of
    ".\n" -> quit;
    _ -> sendRecv(Pid, append, L),
         append(Pid)
  end.

test() ->
  E = start(),
  [] = sendRecv(E, list),
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
  (Num > 0) and (Num =< sendRecv(E, lineCount)).

getcmd(E) ->
  L = chomp(io:get_line(prompt())),
  case L of
    "q" -> {quit, E};
    "a" -> append(E),
           getcmd(E);
    ",p" -> io:format("~s", [sendRecv(E, list)]),
            getcmd(E);
    "p" -> io:format("~s", [sendRecv(E, print)]),
           getcmd(E);
    "d" -> sendRecv(E, delete),
           getcmd(E);
    _ -> Num = getnum(L),
         NumValid = numValid(E, Num),
         if NumValid ->
              sendRecv(E, setLineNumber, Num),
              getcmd(E);
            true ->
              unknown(),
              getcmd(E)
         end
  end.

run() ->
  E = start(),
  getcmd(E).

