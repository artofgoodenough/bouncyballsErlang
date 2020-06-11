-module(axb_collide).
-export([createCollisionProc/3]).

createCollisionProc(Balls, TexWidth, TexHeight)  ->
  spawn(fun() -> collisionProc(0, Balls, TexWidth, TexHeight, [], true) end).

% Receive Every Ball Positions then start receiving every ball position again
collisionProc(Balls, Balls, TexWidth, TexHeight, PosList, Choice) ->
  spawn(fun() -> resolveCollisions(PosList, TexWidth, TexHeight, Choice) end),
  collisionProc(0, Balls, TexWidth, TexHeight, [],  not Choice);
collisionProc(Balls, TBalls, TexWidth, TexHeight, PosList, Choice) ->
  receive
    {ball, BallPid, Pos}  ->
      collisionProc(Balls + 1, TBalls, TexWidth, TexHeight, [{BallPid, Pos} | PosList], Choice);
    {quit, _} ->
      done
  end.

% Sort on X Azis or Y Axis, caller alternates true and false
resolveCollisions(PosList, TexWidth, TexHeight, true) ->
  Sorted = lists:sort(fun({_, {AX, _, _, _}}, {_, {BX, _, _, _}}) -> AX =< BX end, PosList),
  messageBall(Sorted, TexWidth, TexHeight);
resolveCollisions(PosList, TexWidth, TexHeight, false) ->
  Sorted = lists:sort(fun({_, {_, AY, _, _}}, {_, {_, BY, _, _}}) -> AY =< BY end, PosList),
  messageBall(Sorted, TexWidth, TexHeight).

% Pick Next 2 Balls to see if they collide
messageBall([H, P | T], TexWidth, TexHeight) ->
  {_, {HX, HY, _, _}} = H,
  {_, {PX, PY, _, _}} = P,
  Collided = sendMessage(abs(HX - PX) < TexWidth, abs(HY - PY) < TexHeight, H, P),
  nextPair(Collided, T, P, TexWidth, TexHeight);
messageBall(_, _, _) ->
  done.

% Message Pair of Balls that Collide
sendMessage(true, true, {HPid, {_, _, HVx, HVy}}, {PPid, {_, _, PVx, PVy}}) ->
  HPid ! {swap, {PVx, PVy}},
  PPid ! {swap, {HVx, HVy}},
  true;
sendMessage(_, _, _, _) ->
  false.

% Put Second Ball back into List if no collision happened
nextPair(true, T, _, TexWidth, TexHeight) ->
  messageBall(T, TexWidth, TexHeight);
nextPair(false, T, P, TexWidth, TexHeight) ->
  messageBall([P | T], TexWidth, TexHeight).
