-module(animal_sup).
-compile(export_all).

-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    world:start_link(),
    {ok, {{one_for_one, 3, 60},[]}}.

add_animal(SupPid, AnimalName, {X, Y}) ->
    AnimalSpec = {animal, {animal, start_link, [AnimalName, {X, Y}],
                  permanent, 2000, worker, [animal]}},
    supervisor:start_child(SupPid, AnimalSpec),
    {ok, AnimalName}.

remove_animal(SupPid, AnimalName) ->
    ok.
