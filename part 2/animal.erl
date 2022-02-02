-module(animal).

-export([start_link / 2, move/2, sleep/2, stop/1, move_coords/2]).
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
        {move, From, Name, Direction} ->
            case Direction of
                up ->    ets:update_element(AnimalDB, Name, {4, lookup(AnimalDB, Name, y) + 1}),
                         From ! {reply, positionIo(AnimalDB, Name)},
                         loop(AnimalDB);
                down ->  ets:update_element(AnimalDB, Name, {4, lookup(AnimalDB, Name, y) - 1}),
                         From ! {reply, positionIo(AnimalDB, Name)},
                         loop(AnimalDB);
                left ->  ets:update_element(AnimalDB, Name, {3, lookup(AnimalDB, Name, x) - 1}),
                         From ! {reply, positionIo(AnimalDB, Name)},
                         loop(AnimalDB);
                right -> ets:update_element(AnimalDB, Name, {3, lookup(AnimalDB, Name, x) + 1}),
                         From ! {reply, positionIo(AnimalDB, Name)},
                         loop(AnimalDB)
            end;
        {stop, Name} ->
            terminate(Name)
    end.

move(Name, Move) ->
    Name ! {move, self(), Name, Move},
    receive
        {reply, Reply} -> Reply
    end.

move_coords(Name, Coords) ->
    %%% TODO -> Implement the functionality
    %%% to move animal through the loop
    io:format("~p moved to ~p~n", [Name, Coords]).
    
sleep(_Name, Time) ->
    receive
        after Time -> ok
    end.

stop(Name) ->
    io:format("Stopping ~p~n", [Name]),
    Name ! {stop, self(), Name},
    ok.

terminate(Name) ->
    exit(whereis(Name), kill).

positionIo(Db, Key) ->
    [{_,_,X,Y}] = ets:lookup(Db, Key),
    io:format("~p: Moved to position {~p,~p}~n", [Key,X,Y]).

lookup(Db, Key, XorY) ->
    case XorY of
        y -> [{_,_,_,Y}] = ets:lookup(Db, Key), Y;
        x -> [{_,_,X,_}] = ets:lookup(Db, Key), X
    end.
