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
    {DestNodeName, ThisNodeName} = check_teleporters(X, Y),
    AnimalInfo = check_animal_in_right_world(AnimalName, DestNodeName),

    case AnimalInfo of
        [_] ->
            check_grid_size(X, Y),
            
            check_obstacles({X, Y}, palm),
            check_obstacles({X, Y}, rock),
            check_obstacles({X, Y}, water),
            
            check_animals(X, Y),

            gen_server:cast(?MODULE, {move_coords, AnimalName, {X, Y}, ThisNodeName});
        [] ->
            animal:start_link(AnimalName, {X, Y}, ThisNodeName)
    end.

check_grid_size(X, Y) ->
    [GridDimensions] = ets:lookup(gridDb, gridSize),
    {_, Xmax, Ymax} = GridDimensions,
    if
        (X > Xmax) or (Y > Ymax) ->
            throw("Outside of grid range. You'll fall off the planet");
        true ->
            ok
    end.

check_animal_in_right_world(AnimalName, DestNodeName) ->
    AnimalInfo = ets:lookup(worldAnimalDb, AnimalName),
    case AnimalInfo of
        [_] ->
            [{_,_,_,_,World}] = AnimalInfo,
            if
                World == DestNodeName -> throw("That animal does not exist on this world");
                true -> ok
            end;
        [] -> ok
    end,
    AnimalInfo.

check_teleporters(X, Y) ->
    [{_,DestNodeName,TeleX,TeleY,_,_}] = ets:lookup(teleporterDb, node),
    if
        (X == TeleX) and (Y == TeleY) ->
            ThisNodeName = DestNodeName;
        true ->
            case DestNodeName of
                mars -> ThisNodeName = earth;
                earth -> ThisNodeName = mars
            end
    end,
    {DestNodeName, ThisNodeName}.

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
