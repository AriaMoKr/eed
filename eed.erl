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

setLineNumber(Eed, {"", _C, _E}) ->
  setLineNumber(Eed, sendRecv(Eed, getLineNumber) + 1);
setLineNumber(Eed, {A, _C, _E}) ->
  setLineNumber(Eed, getnum(A));
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

print(Eed, {A, C, E}) ->
  print(Eed, A, C, E).
print(Eed, "", ",", "") ->
  printAll(Eed);
print(Eed, "", "", "") ->
  CurLine = sendRecv(Eed, getLineNumber),
  print(Eed, CurLine, CurLine);
print(Eed, Address, _C, _E) when is_list(Address) ->
  print(Eed, getnum(Address), "").
print(Eed, Address, _E) ->
  NumValid = linenumValid(Eed, Address),
  if NumValid -> 
       setLineNumber(Eed, Address),
       {ok, sendRecv(Eed, print)};
     true -> {error, unknown()}
  end.

% [address [,address]]command[parameters] - from GNU Ed Manual
runcmd(Eed, Command) ->
  {match,[[A,C,E,O], _]} = re:run(Command,"([[:digit:]]*)(,?)([[:digit:]]*)([[:alpha:]+-]?)",[global,{capture,all_but_first,list}]),

  Range = {A, C, E},

  case O of
    "q" -> {quit, ""};
    "p" -> print(Eed, Range);
    "a" -> append(Eed), {ok, ""};
    "d" -> delete(Eed);
    "+" -> setLineNumber(Eed, sendRecv(Eed, getLineNumber) + 1);
    "-" -> setLineNumber(Eed, sendRecv(Eed, getLineNumber) - 1);
    _ -> setLineNumber(Eed, Range)
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
  
  {ok, "asdf"} = runcmd(Eed, "1p"),

  {ok, ["asdf", "1234"]} = runcmd(Eed, ",p"),
  {ok, "1234"} = runcmd(Eed, "2"),
  %{ok, ["asdf", "1234"]} = runcmd(Eed, "1,2p"),
  
  {ok, "1234"} = runcmd(Eed, "2"),

  {ok, "asdf"} = runcmd(Eed, "-"),
  {ok, "1234"} = runcmd(Eed, "+"),

  {ok, "asdf"} = runcmd(Eed, "1"),
  {ok, ""} = runcmd(Eed, "d"),

  {ok, "1234"} = runcmd(Eed, "p"),
  {ok, ["1234"]} = runcmd(Eed, ",p"),

  {ok, ""} = runcmd(Eed, "d"),

  ok = sendRecv(Eed, append, "asdf"),
  ok = sendRecv(Eed, append, "1234"),
  ok = sendRecv(Eed, append, "zzzz"),
  %{ok, ["1234", "zzzz"]} = runcmd(Eed, "2,3p"),

  ok.

