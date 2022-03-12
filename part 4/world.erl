-module(world).
-compile(export_all).

-behaviour(gen_server).

-record(gridSize, {widthX, widthY}).
-record(animal, {name, xPosition, yPosition, nodeName}).
-record(node, {name, x, y, destX, destY}).

% start_link() -> start_link('world.config').

start_link(Filename) ->
    {ok, [Config]} = file:consult(Filename),
    {GridSize, Teleporters, Obstacles} = Config,
    grid_into_db(GridSize),
    obstacles_into_db(Obstacles),
    teleporters_into_db(Teleporters),
    ets:new(worldAnimalDb, [named_table, set, {keypos, #animal.name}, public]),
    {ok, _Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, self()}.

grid_into_db(Grid) ->
    {WidthX, WidthY} = Grid,
    ets:new(gridDb, [named_table, set]),
    ets:insert(gridDb, #gridSize{widthX = WidthX, widthY = WidthY}).

obstacles_into_db(Obstacles) ->
    [Palm, Rock, Water] = Obstacles,
    ets:new(obstacleDb, [named_table, set]),
    ets:insert(obstacleDb, Palm),
    ets:insert(obstacleDb, Rock),
    ets:insert(obstacleDb, Water),
    ok.

teleporters_into_db(Teleporters) ->
    [{Node, Entry, Destination}] = Teleporters,
    {X, Y} = Entry,
    {DestX, DestY} = Destination,
    ets:new(teleporterDb, [named_table, set]),
    ets:insert(teleporterDb, #node{name = Node, x = X, y = Y, destX = DestX, destY = DestY}),
    ok.

move(AnimalName, {X, Y}) ->
    [GridDimensions] = ets:lookup(gridDb, gridSize),
    {_, Xmax, Ymax} = GridDimensions,
    if
        (X > Xmax) or (Y > Ymax) ->
            throw("Outside of grid range. You'll fall off the earth");
        true ->
            ok
    end,
    % Check there are no obstacles
    check_obstacles({X, Y}, palm),
    check_obstacles({X, Y}, rock),
    check_obstacles({X, Y}, water),
    % Check there are no other animals...
    check_animals(X, Y),

    % Which node are we currently in &
    [{_,DestNodeName,_,_,_,_}] = ets:lookup(teleporterDb, node),
    if
        DestNodeName ==  mars ->
            ThisNodeName = earth;
        DestNodeName == earth ->
            ThisNodeName = mars
    end,
    % Only use this if animal lands on teleporter
    % TODO -> FINISH
    % check_teleporters(X, Y),

    %%% Think about new animals, and storing positions permanently
    AnimalLookup = check_animal_exists(AnimalName),
    case AnimalLookup of
        [_] ->  gen_server:cast(?MODULE, {move_coords, AnimalName, {X, Y}, ThisNodeName});
        [] ->   animal:start_link(AnimalName, {X, Y}, ThisNodeName)
    end.

% TODO -> FINISH
% MAKE THIS THE MAIN POINT TO CHECK
% check_teleporters(X, Y) ->
%     [{_,DestNodeName,TeleX,TeleY,_,_}] = ets:lookup(teleporterDb, node),
%     if
%         (X == TeleX) and (Y == TeleY) ->
%             if
%                 DestNodeName ==  mars ->
%                     ThisNodeName = earth;
%                 DestNodeName == earth ->
%                     ThisNodeName = mars
%             end
%         true ->
            
%     ok.

check_animal_exists(AnimalName) ->
    AnimalLookup = ets:lookup(worldAnimalDb, AnimalName),
    AnimalLookup.

check_animals(X, Y) ->
    AnimalCheck = lists:flatten(ets:match(worldAnimalDb, #animal{name='$1', xPosition=X, yPosition=Y})),
    check_animal_clash(AnimalCheck).

check_animal_clash([]) ->
    ok;
check_animal_clash([_]) ->
    throw("There is an animal in the area you're trying to move to").

check_obstacles(Coords, Obstacle) ->
    [ObstacleRow] = ets:lookup(obstacleDb, Obstacle),
    {_, ObstacleCoords} = ObstacleRow,
    check_coordinates_clash(Coords, ObstacleCoords).

check_coordinates_clash(_AnimalCoords, []) -> ok;
check_coordinates_clash(AnimalCoords, [H|T]) ->
    if 
        H == AnimalCoords ->
            throw("There is an obstacle in the area you're trying to move to");
        true ->
            check_coordinates_clash(AnimalCoords, T)
    end,
    ok.

get(AnimalName) ->
    [AnimalLocation] = ets:lookup(worldAnimalDb, AnimalName),
    {_,_,X,Y,NodeName} = AnimalLocation,
    io:format("~p: is in location ~p {~p,~p}~n", [AnimalName, NodeName, X, Y]),
    ok.

delete(AnimalName) ->
    ets:delete(worldAnimalDb, AnimalName),
    AnimalName ! exit(whereis(AnimalName), kill),
    ok.

handle_cast({move_coords, AnimalName, {X, Y}, NodeName}, AnimalDb) ->
    animal:move_coords(AnimalName, {X, Y}, NodeName),
    {noreply, AnimalDb}.
