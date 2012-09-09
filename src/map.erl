-module(map).
-export([new/0,update/3, reachable/2, all_nodes/1]).

% Public functions %

% Returns an empty map
new() ->
    [].

% Updates the map to reflect that Node has directional
% links to all nodes in the list Links
update(Node, Links, Map) ->
    NewMap = lists:keydelete(Node,1,Map),
    [{Node, Links} | NewMap].

% Returns the list of nodes directly reachable from Node
reachable(Node, Map) ->
    case lists:keyfind(Node,1,Map) of
	false -> [];
	{_, ListNodes} -> ListNodes
    end.

% Returns all the nodes of the Map
all_nodes(Map) ->
    NewMap = lists:flatmap(fun({Node,Links}) -> [Node|Links] end, Map),
    lists:usort(NewMap).
    
    
