-module(world).

-export([start_link/1, move/2, get/1, delete/1]).

start_link(Filename) ->
    {ok,[T]} = file:consult(Filename),
    T.

move(AnimalName, {X, Y}) ->
    ok.

get(AnimalName) ->
    ok.

delete(AnimalName) ->
    ok.