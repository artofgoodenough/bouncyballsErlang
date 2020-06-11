-module(axb_ball).
-export([createBallProcs/6]).

% Create one Process per Ball
createBallProcs(0, _, _, _, _, Pids) ->
  Pids;
createBallProcs(Balls, V, Xlim, Ylim, CPid, Pids) ->
  X = rand:uniform() * Xlim,
  Y = rand:uniform() * Ylim,
  Vx = rand:uniform() * V,
  Vy = rand:uniform() * V,
  Pid = spawn(fun() -> ball({X, Y, Vx, Vy}, Xlim, Ylim, CPid) end),
  createBallProcs(Balls - 1, V, Xlim, Ylim, CPid, [Pid | Pids]).

% Ball Process
ball({X, Y, Vx, Vy}, Xlim, Ylim, CPid) ->
  receive
    {move, Pid} ->
      NewLoc = moveBall({X, Y, Vx, Vy}, Xlim, Ylim),
      {X1, Y1, _, _} = NewLoc,
      Bin = <<
              X1:32/native-signed-float,
              Y1:32/native-signed-float
            >>,
      Pid ! {ball, Bin},
      CPid ! {ball, self(), NewLoc},
      ball(NewLoc, Xlim, Ylim, CPid);
    {swap, {NVx, NVy}}  ->
      ball({X - Vx + NVx, Y - Vy + NVy, NVx, NVy}, Xlim, Ylim, CPid);
    {quit, _} ->
      quit
  end.

% Movement within walls of Window
moveBall({X, Y, Vx, Vy}, Xlim, Ylim) when X < 0 ->
  moveBall({0, Y, -Vx, Vy}, Xlim, Ylim);
moveBall({X, Y, Vx, Vy}, Xlim, Ylim) when X > Xlim ->
  moveBall({Xlim, Y, -Vx, Vy}, Xlim, Ylim);
moveBall({X, Y, Vx, Vy}, Xlim, Ylim) when Y < 0 ->
  moveBall({X, 0, Vx, -Vy}, Xlim, Ylim);
moveBall({X, Y, Vx, Vy}, Xlim, Ylim) when Y > Ylim ->
  moveBall({X, Ylim, Vx, -Vy}, Xlim, Ylim);
moveBall({X, Y, Vx, Vy}, _, _) ->
  {X + Vx, Y + Vy, Vx, Vy}.
