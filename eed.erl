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

append(Eed, _Range, _RangeGiven) ->
  append(Eed).
append(Eed) ->
  L = io:get_line(""),
  case L of
    ".\n" -> {ok, ""};
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
                 {ok, [sendRecv(Eed, print)]};
     true -> {err, unknown()}
  end.

delete(Eed, _Range, _RangeGiven) ->
  delete(Eed).
delete(Eed) ->
  NumValid = linenumValid(Eed, sendRecv(Eed, getLineNumber)),
  if NumValid -> sendRecv(Eed, delete), {ok, ""};
     true -> {err, unknown()}
  end.

print(Eed, {A, E}, _RangeGiven) ->
  AValid = linenumValid(Eed, A),
  EValid = linenumValid(Eed, E),
  if AValid, EValid, A =< E -> print(Eed, A, E, validated);
     true -> {error, unknown()}
  end.

print(Eed, A, A, validated) -> 
  setLineNumber(Eed, A),
  {ok, [sendRecv(Eed, print)]};
print(Eed, A, E, validated) ->
  {ok, LinesA} = print(Eed, A, A, validated),
  {ok, LinesB} = print(Eed, A+1, E, validated),
  {ok, LinesA ++ LinesB}.

noCmd(Eed, Range, false) ->
  incLine(Eed, Range, false);
noCmd(Eed, {A, E}, RangeGiven) ->
  print(Eed, {A, E}, RangeGiven).

quit(_Eed, _Range, false) ->
  {quit, ""};
quit(_Eed, _Range, true) ->
  {error, unknown()}.

incLine(Eed, _Range, _RangeGiven) ->
  setLineNumber(Eed, sendRecv(Eed, getLineNumber) + 1).

decLine(Eed, _Range, _RangeGiven) ->
  setLineNumber(Eed, sendRecv(Eed, getLineNumber) - 1).

getLineRef(Eed, ".") ->
  sendRecv(Eed, getLineNumber);
getLineRef(Eed, "$") ->
  sendRecv(Eed, lineCount);
getLineRef(_Eed, Num) ->
  getnum(Num).

convertRange(Eed, {"", "", ""}) ->
  convertRange(Eed, {".", ",", "."});
convertRange(Eed, {"", ",", ""}) ->
  convertRange(Eed, {"1", ",", "$"});
convertRange(Eed, {A, "", ""}) ->
  convertRange(Eed, {A, ",", A});
convertRange(_Eed, {"", ",", _}) ->
  {error, unknown()};
convertRange(Eed, {A, ",", E}) ->
  {getLineRef(Eed, A), getLineRef(Eed, E)}.

commandSplit(Command) ->
% [address [,address]]command[parameters] - from GNU Ed Manual
  {match,R} = re:run(Command,"([[:digit:].$]*)(,?)([[:digit:].$]*)([[:alpha:]+-]?)",[global,{capture,all_but_first,list}]),
  [A,C,E,O] = hd(R),
  {A,C,E,O}.

runcmd(Eed, Command) ->
  {A,C,E,O} = commandSplit(Command),
  NewRange = convertRange(Eed, {A,C,E}),
  RangeGiven = {A,C,E} =/= {"","",""},

  io:format("~p~n", [{NewRange, RangeGiven}]),

  case O of
    "q" -> quit(Eed, NewRange, RangeGiven);
    "p" -> print(Eed, NewRange, RangeGiven);
    "a" -> append(Eed, NewRange, RangeGiven);
    "d" -> delete(Eed, NewRange, RangeGiven);
    "+" -> incLine(Eed, NewRange, RangeGiven);
    "-" -> decLine(Eed, NewRange, RangeGiven);
    _ -> noCmd(Eed, NewRange, RangeGiven)
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
  {ok, ["asdf"]} = runcmd(Eed, "p"),
  {ok, ["asdf"]} = runcmd(Eed, ",p"),
  {ok, ["asdf"]} = runcmd(Eed, "1p"),

  ok = sendRecv(Eed, append, "1234"),
  {ok, ["asdf"]} = runcmd(Eed, "1"),
  {ok, ["asdf"]} = runcmd(Eed, "p"),
  {ok, ["1234"]} = runcmd(Eed, ""),
  {ok, ["1234"]} = runcmd(Eed, "2p"),
  
  {ok, ["asdf"]} = runcmd(Eed, "1p"),

  {ok, ["asdf", "1234"]} = runcmd(Eed, ",p"),
  {ok, ["1234"]} = runcmd(Eed, "2"),
  {ok, ["asdf"]} = runcmd(Eed, "1,1p"),
  {ok, ["1234"]} = runcmd(Eed, "2,2p"),
  {ok, ["asdf", "1234"]} = runcmd(Eed, "1,2p"),
  
  {ok, ["1234"]} = runcmd(Eed, "2"),

  {ok, ["asdf"]} = runcmd(Eed, "-"),
  {ok, ["1234"]} = runcmd(Eed, "+"),
  {ok, ["asdf"]} = runcmd(Eed, "-"),

  {ok, ["asdf"]} = runcmd(Eed, "1"),
  {ok, ""} = runcmd(Eed, "d"),

  {ok, ["1234"]} = runcmd(Eed, "p"),
  {ok, ["1234"]} = runcmd(Eed, ",p"),
  {error, _} = runcmd(Eed, ",5p"),

  {ok, ""} = runcmd(Eed, "d"),

  ok = sendRecv(Eed, append, "asdf"),
  ok = sendRecv(Eed, append, "1234"),
  ok = sendRecv(Eed, append, "zzzz"),
  {ok, ["1234", "zzzz"]} = runcmd(Eed, "2,3p"),
  {ok, ["zzzz"]} = runcmd(Eed, ".p"),
  {ok, ["zzzz"]} = runcmd(Eed, ".,.p"),
  {ok, ["asdf"]} = runcmd(Eed, "1"),
  {ok, ["zzzz"]} = runcmd(Eed, "$p"),
  {ok, ["asdf"]} = runcmd(Eed, "1"),
  {ok, ["1234", "zzzz"]} = runcmd(Eed, "2,$p"),
  {error, "?\n"} = runcmd(Eed, "0"),
  {error, "?\n"} = runcmd(Eed, "4"),

  ok.

