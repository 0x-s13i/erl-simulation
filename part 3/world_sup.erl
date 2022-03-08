-module(world_sup).
-compile(export_all).

-behaviour(supervisor).

start_link(ChildList) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, ChildList).

init(_) ->
    {ok,{
        {one_for_one,2,3600}, 
        [
            {
                world,
                {world, start_link},
                permanent, 10000, worker, [world]
            },
            {
                animal_sup,
                {animal_sup, start_link},
                permanent, 10000, supervisor, [animal_sup]
            }
        ]
    }}.
