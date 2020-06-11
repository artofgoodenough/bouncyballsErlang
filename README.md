# Simple Bouncy Balls with Erlang
## Purpose
Learning Erlang. I am currently reading 'Programming Erlang 2nd Edition by Joe Armstrong'. Started at the beginning, jumped about chapters in parts 2, 3 and 4, trying out some of the great features. Got the book maybe 5 or 7 years ago, but got busy and forgot what I learnt before, so this time I'm documenting things.

## This bouncyballsErlang Project
The files tcp_ball_server.erl, axb_ball.erl and axb_collide.erl make up a simple server that accepts TCP connections with a request for bouncing balls positions. All balls are the same size to simplify calculations. Collision is modeled simply by checking for intersection of squares of any 2 balls, so no sqrt even, anywhere. This server responds by sending out balls positions continuously based on frames per second requested. Multiple clients may connect and disconnect, each requesting their own set of balls.

To run the server, you'll need Erlang. On a Debian Linux System, that's installed using,
```
sudo apt-get install erlang
```

To run the server, run 'erl' from the directory that contains the abovementioned .erl files, then compile each file and run the server on port 63000, because the client is built to make requests to port 63000.
After running 'erl' here's how it looks on my command terminal,
```
Erlang/OTP 22 [erts-10.6.4] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1]

Eshell V10.6.4  (abort with ^G)
1> c(axb_ball).
{ok,axb_ball}
2> c(axb_collide).
{ok,axb_collide}
3> c(tcp_ball_server).
{ok,tcp_ball_server}
4> tcp_ball_server:server(63000).
<0.95.0>
5>
```
### The Client
The client is an SDL GUI C++ Application that's located in the sdlClient directory. However, you must have SDL2 to build.

To install SDL2 on Debian Linux Systems,
```
sudo apt-get install libsdl2-dev
sudo apt-get install libsdl2-image-dev
```   

To build go into the sdlClient directory, and just run make from the command line. The executable should be created in the bin directory under the sdlClient directory. To run, go into the bin diretory, and run the executable.
```
./dispBalls.exe
```

Of course the Erlang Balls Server should already be running, or it will not find a server and quit.

## Please Note
This is a simple 'just good enough' demo of a bunch of elastic balls bouncing around. Not exact, but just good enough, so you will see the occasional clump of quivering balls, and I only handle collision pairs. The Erlang code could be simpler to just do this, but it's the way it is so I could learn to use features, and it's my documentation of how to use those features. You might also notice that there are race conditions, but I have not seen them manifest themselves when running the code yet, using multiple clients on my refurbished HP 840 from a year and a half ago.

This is mainly documentation for myself, but it's public just in case anyone else finds it useful while learning Erlang, maybe even SDL, C++, or Sockets programming from Erlang and C.
