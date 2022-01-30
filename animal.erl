-module(animal).

-export([start_link / 2, move/2, sleep/2, stop/1]).
-export([init/3]).

-record(animal, {name, xPosition, yPosition}).

start_link(Name, {X, Y}) ->
    Pid = spawn(?MODULE, init, [Name, X, Y]),
    register(Name, Pid),
    {ok, Pid}.

init(Name, X, Y) ->
    io:format("~p: Started in position {~p,~p}~n", [Name, X, Y]),
    ets:new(animalDb, [named_table, set, {keypos, #animal.name}]),
    ets:insert(animalDb, #animal{name = Name, xPosition = X, yPosition = Y}),
    loop(animalDb).

loop(AnimalDB) ->
    receive
        {move, Name, up} ->
            ets:update_element(AnimalDB, Name, {4, lookupY(AnimalDB, Name) + 1}),
            [{_,_,X,Y}] = ets:lookup(AnimalDB, Name),
            io:format("~p: Moved to position {~p,~p}~n", [Name,X,Y]),
            loop(AnimalDB);
        {move, Name, down} ->
            ets:update_element(AnimalDB, Name, {4, lookupY(AnimalDB, Name) - 1}),
            [{_,_,X,Y}] = ets:lookup(AnimalDB, Name),
            io:format("~p: Moved to position {~p,~p}~n", [Name,X,Y]),
            loop(AnimalDB);
        {move, Name, left} ->
            ets:update_element(AnimalDB, Name, {3, lookupX(AnimalDB, Name) - 1}),
            [{_,_,X,Y}] = ets:lookup(AnimalDB, Name),
            io:format("~p: Moved to position {~p,~p}~n", [Name,X,Y]),
            loop(AnimalDB);
        {move, Name, right} ->
            ets:update_element(AnimalDB, Name, {3, lookupX(AnimalDB, Name) + 1}),
            [{_,_,X,Y}] = ets:lookup(AnimalDB, Name),
            io:format("~p: Moved to position {~p,~p}~n", [Name,X,Y]),
            loop(AnimalDB);
        stop ->
            ok
    end.

move(Name, Move) ->
    Name ! {move, Name, Move},
    ok.

sleep(Name, Time) ->
    ok.

stop(Name) ->
    ok.

lookupY(Db, Key) ->
    [{_,_,_,Y}] = ets:lookup(Db, Key),
    Y.

lookupX(Db, Key) ->
    [{_,_,X,_}] = ets:lookup(Db, Key),
    X.