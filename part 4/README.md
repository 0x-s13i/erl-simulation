# erl-simulation
Erlang implementation of a simulated world where animals roam around virtual worlds.

This part of the exercise creates a server (implemented in the `world.erl` module) which, upon starting, loads a configuration of the grid. It stores a list of obstacles together with the position of the animals.

Before moving, animals will ask the server if they can move to a particular slot. If there is no obstacle or animal, and the animal does not risk falling off the face of the *flat* earth, the animal moves and the server stores the new position of the animal

Compile with

    > c(animal).
    {ok,animal}
    > c(world).
    {ok,world}

An example with a turtle, and introducing a frog:

    > world:start_link('world.config').
    turtle: Started in position {4,3}
    {ok,<0.92.0>}
    > world:move(turtle, {2,2}).
    turtle: Moved to position {2,2}
    ok
    > world:move(frog, {4,3}).
    frog: Started in position {4,3}
    > world:get(turtle).
    turtle is in location 2,2