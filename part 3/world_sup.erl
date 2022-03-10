-module(world_sup).

-export([start_link/0]).
-export([init/1]).

-behaviour(supervisor).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok,{
        {one_for_one, 5, 10000}, 
        [
            {
                world,
                {world, start_link, []},
                permanent, 10000, worker, [world]
            },
            {
                animal_sup,
                {animal_sup, start_link, []},
                permanent, 10000, supervisor, [animal_sup]
            }
        ]
    }}.
