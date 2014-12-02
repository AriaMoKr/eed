-module(eed).
-compile([debug_info, export_all]).

fileed(Lines, Cursor) ->
  receive
    {From, {append, Line}} ->
      From ! {self(), ok},
      {A, B} = lists:split(Cursor, Lines),
      fileed(A ++ [Line] ++ B, Cursor + 1);
    {From, {delete, undef}} ->
      From ! {self(), ok},
      {A, [_|B]} = lists:split(Cursor-1, Lines),
      NewLines = A ++ B,
      NewCursor = min(Cursor, length(NewLines)),
      fileed(NewLines, NewCursor);
    {From, {list, undef}} ->
      From ! {self(), Lines},
      fileed(Lines, Cursor);
    {From, {print, undef}} ->
      From ! {self(), lists:nth(Cursor, Lines)},
      fileed(Lines, Cursor);
    {From, {lineCount, undef}} ->
      From ! {self(), length(Lines)},
      fileed(Lines, Cursor);
    {From, {setLineNumber, Num}} ->
      From ! {self(), ok},
      fileed(Lines, Num);
    {From, {getLineNumber, undef}} ->
      From ! {self(), Cursor},
      fileed(Lines, Cursor);
    terminate -> ok
  end.

start() ->
  spawn(?MODULE, fileed, [[], 0]).

sendRecv(Eed, Cmd) ->
  sendRecv(Eed, Cmd, undef).
sendRecv(Eed, Cmd, Arg) ->
  Eed ! {self(), {Cmd, Arg}},
  receive
    {Eed, Msg} -> Msg
    after 2000 -> timeout
  end.

%TODO move append functionality to runcmd with mode
append(Eed) ->
  L = io:get_line(""),
  case L of
    ".\n" -> finishedAppend;
    _ -> sendRecv(Eed, append, L),
         append(Eed)
  end.

unknown() -> "?\n".

prompt() -> "*".

chomp(String) ->
  re:replace(String, "(^\\s+)|(\\s+$)", "", [global, {return,list}]).

getnum(String) ->
  try erlang:list_to_integer(String)
    catch error:badarg -> 0
  end.

linenumValid(Eed, Num) ->
  (Num > 0) and (Num =< sendRecv(Eed, lineCount)).

setLineNumber(Eed, Num) ->
  NumValid = linenumValid(Eed, Num),
  if NumValid -> sendRecv(Eed, setLineNumber, Num),
                 {ok, sendRecv(Eed, print)};
     true -> {err, unknown()}
  end.

delete(Eed) ->
  NumValid = linenumValid(Eed, sendRecv(Eed, getLineNumber)),
  if NumValid -> sendRecv(Eed, delete), {ok, ""};
     true -> {err, unknown()}
  end.

printAll(Eed) ->
  NumValid = linenumValid(Eed, sendRecv(Eed, getLineNumber)),
  if NumValid -> {ok, sendRecv(Eed, list)};
     true -> {error, unknown()}
  end.

print(Eed, _Address) ->
  NumValid = linenumValid(Eed, sendRecv(Eed, getLineNumber)),
  if NumValid -> {ok, sendRecv(Eed, print)};
     true -> {error, unknown()}
  end.

% [address [,address]]command[parameters] - from GNU Ed Manual
runcmd(Eed, Command) ->
  {match,[[A,C,_E,O], _]} = re:run(Command,"([[:digit:]]*)([,]?)([[:digit:]]*)([[:alpha:]+-]?)",[global,{capture,all_but_first,list}]),

  % io:format("~p~n", [{A,C,O}]),

  case {A,C,O} of
    {_, _, "q"} -> {quit, ""};
    {A, _, "a"} -> append(Eed), {ok, ""};
    {A, ",", "p"} -> printAll(Eed);
    {A, _, "p"} -> print(Eed, getnum(A));
    {A, _, "d"} -> delete(Eed);
    {A, _, "+"} -> setLineNumber(Eed, sendRecv(Eed, getLineNumber) + 1);
    {A, _, "-"} -> setLineNumber(Eed, sendRecv(Eed, getLineNumber) - 1);
    {"", _, ""} -> setLineNumber(Eed, sendRecv(Eed, getLineNumber) + 1);
    {A, _, O} -> setLineNumber(Eed, getnum(A))
  end.

cmdloop(Eed) ->
  Cmd = chomp(io:get_line(prompt())),
  {R, Msg} = runcmd(Eed, Cmd),
  if R == quit -> quit;
    true -> io:format("~s", [Msg]), cmdloop(Eed)
  end.

run() ->
  Eed = start(),
  cmdloop(Eed).

test() ->
  Eed = start(),

  ok = sendRecv(Eed, append, "asdf"),
  {ok, "asdf"} = runcmd(Eed, "p"),
  {ok, ["asdf"]} = runcmd(Eed, ",p"),
  {ok, "asdf"} = runcmd(Eed, "1p"),

  ok = sendRecv(Eed, append, "1234"),
  {ok, "1234"} = runcmd(Eed, "p"),
  {ok, "1234"} = runcmd(Eed, "2p"),
  
  %{ok, "asdf"} = runcmd(Eed, "1p"),

  {ok, ["asdf", "1234"]} = runcmd(Eed, ",p"),
  %{ok, ["asdf", "1234"]} = runcmd(Eed, "1,2p"),

  {ok, "asdf"} = runcmd(Eed, "-"),
  {ok, "1234"} = runcmd(Eed, "+"),

  {ok, "asdf"} = runcmd(Eed, "1"),
  {ok, ""} = runcmd(Eed, "d"),

  {ok, "1234"} = runcmd(Eed, "p"),
  {ok, ["1234"]} = runcmd(Eed, ",p"),

  {ok, ""} = runcmd(Eed, "d"),

  ok.

