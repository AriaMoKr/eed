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

init() -> [[], 0].

start() ->
  spawn(?MODULE, fileed, init()).

sendRecv(Pid, Cmd) ->
  sendRecv(Pid, Cmd, undef).
sendRecv(Pid, Cmd, Arg) ->
  Pid ! {self(), {Cmd, Arg}},
  receive
    {Pid, Msg} -> Msg
    after 2000 -> timeout
  end.

%TODO move append functionality to runcmd with mode
append(Pid) ->
  L = io:get_line(""),
  case L of
    ".\n" -> finishedAppend;
    _ -> sendRecv(Pid, append, L),
         append(Pid)
  end.

unknown() -> io:format("?~n").

prompt() -> "*".

chomp(String) ->
  re:replace(String, "(^\\s+)|(\\s+$)", "", [global, {return,list}]).

getnum(String) ->
  try erlang:list_to_integer(String)
    catch error:_Ex -> 0
  end.

numValid(E, Num) ->
  (Num > 0) and (Num =< sendRecv(E, lineCount)).

runcmd(E, L) ->
  case L of
    "q" -> {quit, ""};
    "a" -> append(E), {ok, ""};
    ",p" -> {ok, sendRecv(E, list)};
    "p" -> {ok, sendRecv(E, print)};
    "d" -> sendRecv(E, delete), {ok, ""};
    "+" -> N = sendRecv(E, getLineNumber),
           sendRecv(E, setLineNumber, N + 1),
           {ok, sendRecv(E, print)};
    "-" -> N = sendRecv(E, getLineNumber),
           sendRecv(E, setLineNumber, N - 1),
           {ok, sendRecv(E, print)};
    _ -> Num = getnum(L),
         NumValid = numValid(E, Num),
         if NumValid -> sendRecv(E, setLineNumber, Num),
                        {ok, sendRecv(E, print)};
            true -> {error, unknown()}
         end
  end.

cmdloop(E) ->
  Cmd = chomp(io:get_line(prompt())),
  {R, Msg} = runcmd(E, Cmd),
  if R == quit -> quit;
    true -> io:format("~s", [Msg]), cmdloop(E)
  end.

run() ->
  E = start(),
  cmdloop(E).

test() ->
  E = start(),
  %[] = sendRecv(E, list),
  {ok, ""} = runcmd(E, ",p"),

  %{ok, ["asdf"]} = runcmd(E, "a"),
  %would need to handle input programmatically, maybe mode in runcmd or use an iobuffer
  %use sendRecv instead
  ok = sendRecv(E, append, "asdf"),
  {ok, "asdf"} = runcmd(E, "p"),
  {ok, ["asdf"]} = runcmd(E, ",p"),

  ok = sendRecv(E, append, "1234"),
  {ok, "1234"} = runcmd(E, "p"),
  {ok, ["asdf", "1234"]} = runcmd(E, ",p"),

  {ok, "asdf"} = runcmd(E, "-"),
  {ok, "1234"} = runcmd(E, "+"),

  {ok, "asdf"} = runcmd(E, "1"),
  {ok, ""} = runcmd(E, "d"),

  {ok, "1234"} = runcmd(E, "p"),
  {ok, ["1234"]} = runcmd(E, ",p"),

  {ok, ""} = runcmd(E, "d"),

  ok.

