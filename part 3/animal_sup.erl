-module(animal_sup).

-export([start_link/0, add_animal/3, remove_animal/2]).
-export([init/1]).

-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok, {{one_for_one, 20, 1000000},[]}}.

add_animal(SupPid, AnimalName, {X, Y}) ->
    AnimalChild = {AnimalName, {animal, start_link, [AnimalName, {X, Y}]},
                   permanent, infinity, worker, [animal]},
    supervisor:start_child(SupPid, AnimalChild),
    {ok, AnimalName}.

remove_animal(SupPid, AnimalName) ->
    ets:delete(worldAnimalDb, AnimalName),
    supervisor:terminate_child(SupPid, AnimalName),
    supervisor:delete_child(SupPid, AnimalName),
    ok.
