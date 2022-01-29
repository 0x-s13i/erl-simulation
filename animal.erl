-module(animal).
-export([start_link / 2, move/2, sleep/2, stop/1]).

start_link(Name, {X, Y}) -> ok.
move(Name, Move) -> ok.
sleep(Name, Time) -> ok.
stop(Name) -> ok.
