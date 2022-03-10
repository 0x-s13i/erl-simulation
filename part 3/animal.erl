-module(animal).

-export([start_link/2, move_coords/2]).
-export([init/3]).

-record(animal, {name, xPosition, yPosition}).

start_link(Name, {X, Y}) ->
    Pid = spawn_link(?MODULE, init, [Name, X, Y]),
    register(Name, Pid),
    {ok, Pid}.

init(Name, X, Y) ->
    ets:insert(worldAnimalDb, #animal{name = Name, xPosition = X, yPosition = Y}),
    io:format("~p: Started in position {~p,~p}~n", [Name, X, Y]),
    loop().

loop() ->
    receive
        {move_coords, Name, X, Y} ->
            ets:insert(worldAnimalDb, #animal{name = Name, xPosition = X, yPosition = Y}),
            io:format("~p: Moved to {~p,~p}~n", [Name, X, Y]),
            loop()
    end.

move_coords(Name, {X, Y}) ->
    Name ! {move_coords, Name, X, Y}.
