# erl-simulation
Erlang implementation of a simulated world where animals roam around virtual worlds.

This part of the exercise creates an animal process which roams around the world, not worrying about obstacles, other animals, or the edge of the earth. It uses `io:format/2` to provide feedback on the animals location.

Compile with
    
    > c(animal).
    {ok,animal}

An example with a turtle:

    > animal:start_link(turtle, {4,3}).
    turtle: Started in position {4,3}
    {ok,<0.86.0>}
    > animal:move(turtle, up).
    turtle: Moved to position {4,4}
    ok
    > animal:move(turtle, up), animal:move(turtle, up), animal:move(turtle, up).
    turtle: Moved to position {4,5}
    turtle: Moved to position {4,6}
    turtle: Moved to position {4,7}
    ok
    > animal:move(turtle, right), animal:move(turtle, right).
    turtle: Moved to position {5,7}
    turtle: Moved to position {6,7}
    ok
