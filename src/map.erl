-module(map).
-export([new/0,update/3,reachable/2,all_nodes/1]).

% Public functions %

% Returns an empty map
new() ->
    [].

