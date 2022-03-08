-module(animal).
-compile(export_all).

-record(animal, {name, xPosition, yPosition}).

start_link(Name, {X, Y}) ->
    register(Name, spawn(?MODULE, init, [Name, X, Y])).

init(Name, X, Y) ->
    io:format("~p: Started in position {~p,~p}~n", [Name, X, Y]),
    ets:insert(worldAnimalDb, #animal{name = Name, xPosition = X, yPosition = Y}).

move_coords(Name, Coords, AnimalDb) ->
    {X, Y} = Coords,
    ets:insert(worldAnimalDb, #animal{name = Name, xPosition = X, yPosition = Y}),
    AnimalDb.

delete(Name, AnimalDb) ->
    io:format("Stopping ~p~n", [Name]),
    ets:delete(worldAnimalDb, Name),
    AnimalDb.
