-module(world).

-export([start_link/1, move/2, get/1, delete/1]).
-export([init/1, handle_call/3]).

-behaviour(gen_sever).

-record(gridSize, {widthX, widthY}).
-record(animal, {name, positionX, positionY}).

start_link(Filename) ->
    {ok, [Config]} = file:consult(Filename),
    {GridSize, _Teleporters, Obstacles} = Config,
    grid_into_db(GridSize),
    obstacles_into_db(Obstacles),
    gen_server:start_link({local, ?MODULE},?MODULE,[],[]).

init(_) ->
    %%% Should I be initialising the turtle here? %%%
    %%% Seeems strange not to give user interface %%%
    {ok, animal:start_link(turtle, {4, 3})}.

grid_into_db(Grid) ->
    {WidthX, WidthY} = Grid,
    ets:new(gridDb, [named_table, set]),
    ets:insert(gridDb, 
        #gridSize{widthX = WidthX, widthY = WidthY}).

obstacles_into_db(Obstacles) ->
    [Palm, Rock, Water] = Obstacles,
    ets:new(obstacleDb, [named_table, set]),
    ets:insert(obstacleDb, Palm),
    ets:insert(obstacleDb, Rock),
    ets:insert(obstacleDb, Water),
    ok.

move(AnimalName, {X, Y}) ->
    %%% TODO -> implement the lookup in the grid and
    %%% obstacle db to make sure that the move is valid.
    gen_server:call(?MODULE, {move_coords, AnimalName, {X, Y}}).
    %%% Think about new animals, and storing positions permanently

get(AnimalName) ->
    ok.

delete(AnimalName) ->
    ok.

handle_cast() ->
    ok.

handle_call({move_coords, AnimalName, {X, Y}}, _From, AnimalDB) ->
    {reply, animal:move_coords(AnimalName, {X, Y}), AnimalDB}.
