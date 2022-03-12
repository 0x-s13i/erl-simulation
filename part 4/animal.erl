-module(animal).

-export([start_link/3, move_coords/3]).
-export([init/4]).

-record(animal, {name, xPosition, yPosition, nodeName}).

start_link(Name, {X, Y}, NodeName) ->
    Pid = spawn_link(?MODULE, init, [Name, X, Y, NodeName]),
    register(Name, Pid),
    {ok, Pid}.

init(Name, X, Y, NodeName) ->
    ets:insert(worldAnimalDb, #animal{name = Name, xPosition = X, yPosition = Y, nodeName = NodeName}),
    io:format("~p: Started in position ~p {~p,~p}~n", [Name, NodeName, X, Y]),
    loop().

loop() ->
    receive
        {move_coords, Name, X, Y, NodeName} ->
            ets:insert(worldAnimalDb, #animal{name = Name, xPosition = X, yPosition = Y, nodeName = NodeName}),
            io:format("~p: Moved to ~p {~p,~p}~n", [NodeName, Name, X, Y]),
            loop()
    end.

move_coords(Name, {X, Y}, NodeName) ->
    Name ! {move_coords, Name, X, Y, NodeName}.
