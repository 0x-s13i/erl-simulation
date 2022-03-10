# erl-simulation
We use supervision trees to implement our fault-tolerant strategy. To get a suitable supervision structure, we need to have different supervisors at different levels capable of handling static and dynamic children.

## Animal Supervisor
This supervisor starts and monitors the animals.

## Top Level Supervisor
This starts the world server and the animal supervisor.

Compile with

    > c(animal).
    {ok,animal}
    > c(world).
    {ok,world}
    > c(animal_sup).
    {ok,animal_sup}
    > c(world_sup).
    {ok,world_sup}

An example with a turtle, and introducing a frog:

    > world_sup:start_link().
    {ok,<0.92.0>}
    > animal_sup:add_animal(animal_sup, turtle, {4,3}).
    turtle: Started in position {4,3}
    {ok, turtle}
    > animal_sup:add_animal(animal_sup, frog, {2,2}).
    frog: Started in position {2,2}
    {ok, frog}
    > world:move(frog, {2,3}).
    frog: Moved to {2,3}
    > world:get(frog).
    frog: is in location {2,3}