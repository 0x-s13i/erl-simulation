# erl-simulation
Implementing the backend of a simulated world where animals roam around virtual worlds. In doing so, they have to avoid water, obstacles and other animals. Many (distributed) worlds will exist in parallel, and animals will be able to move in between these worlds whenever they stumble upon a teleporter (yes, they do exist!).

The animal process will consist of a functional interface used by intelligent agents. Actions agents can perform on the animals include:

* Join and leave a world
* Move up, down, left and right
* Pause in between moves

The world will consist of a two dimensional, square grid inhabited by animals moving from slot to slot. Ahead of every move, they will be told by a server if the slot they want to enter free, if it contains a teleporter, an obstacle or other animals. This being a flat earth, the server will also be responsible to ensure animals do not fall off the edge.
The server will:
* Load the configuration of a virtual world from file
* Inform animals if the move they want to make is valid
* Teleport animals to a different world
* Handle the dynamic creation and deletion of animals

The exercise will be broken up into smaller parts that are easier to develop and test. By the end they will be combined into a supervision tree and distributed across multiple worlds.