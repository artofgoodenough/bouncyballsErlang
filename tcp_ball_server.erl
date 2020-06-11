-module(tcp_ball_server).
-export([server/1]).
-import(axb_ball, [createBallProcs/6]).
-import(axb_collide, [createCollisionProc/3]).

server(Port) ->
  {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 0}, {reuseaddr, true}, {active, once}]),
  spawn(fun() -> connect(Listen) end).

% Listener for TCP Connections
connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> connect(Listen) end),
  serveClient(Socket).

% Service Client on Socket
%   Initialize for set of Balls
serveClient(Socket) ->
  receive
    {tcp, Socket, Bin} ->
      <<
          Balls:32/native-signed-integer,
          Fps:32/native-signed-integer,
          Speed:32/native-signed-integer,
          WinWidth:32/native-signed-integer,
          TexWidth:32/native-signed-integer,
          WinHeight:32/native-signed-integer,
          TexHeight:32/native-signed-integer
          >> = Bin,
      io:format("Balls: ~p, FPS: ~p~n", [Balls, Fps]),
      CPid = createCollisionProc(Balls, TexWidth, TexHeight),
      Pids = createBallProcs(Balls, Speed / Fps, WinWidth - TexWidth,
                              WinHeight - TexHeight, CPid, []),
      startBalls(Socket, Balls, CPid, Pids, 1000 div Fps);
    {tcp_closed, Socket}  ->
      io:format("Socket Closed from Client w/o init.~n")
  end.

% The Loop Function
%   Timeout for Latest Ball Positions
%   Send out Ball Positions on Socket
startBalls(Socket, Balls, CPid, Pids, Timeout) ->
  MyPid = self(),
  lists:map(fun(Pid) -> spawn(fun() -> Pid ! {move, MyPid} end) end, Pids),
  Payload = receiveStates(Balls, []),
  sendPacket(Socket, Payload),
  receive
    {tcp_closed, Socket}  ->
      io:format("Socket Closed from Client.~n"),
      lists:map(fun(Pid) -> spawn(fun() -> Pid ! {quit, MyPid} end) end, Pids),
      CPid ! {quit, MyPid}
  after Timeout ->
    startBalls(Socket, Balls, CPid, Pids, Timeout)
  end.

% Send out Ball Position Packets
sendPacket(Socket, <<>>) ->
  self() ! {tcp_closed, Socket};
sendPacket(Socket, Payload) ->
  gen_tcp:send(Socket, Payload),
  inet:setopts(Socket, [{active, once}]).

% Reeive all Ball Positions per time unit
receiveStates(0, Positions) ->
  list_to_binary(Positions);
receiveStates(Balls, Positions) ->
  receive
    {ball, Bin} ->
      receiveStates(Balls - 1, [Bin | Positions]);
    {tcp_closed, _}  ->
      <<>>
  end.
