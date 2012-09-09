-module(map).
-export([new/0,update/3]).

% Public functions %

% Returns an empty map
new() ->
    [].

% Updates the map to reflect that Node has directional links to all nodes in the list Links
update(Node, Links, Map) ->
    NewMap = lists:keydelete(Node,1,Map),
    lists:append(NewMap, {Node, Links}).
