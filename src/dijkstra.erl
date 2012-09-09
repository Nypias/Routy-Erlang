-module(dijkstra).
-export([entry/2, replace/4, update/4]).

% Public functions

% Returns the length of the shortest path to the node
% 0 if the node is not found
entry(Node,Sorted) ->
    case lists:keyfind(Node,1,Sorted) of
	{_,Length,_} ->
	    Length;
	false -> 0
    end.

% Replace the entry for Node in sorted with a new entry
replace(Node, N, Gateway, Sorted) ->
    NewMap = lists:keydelete(Node,1,Sorted),
    lists:keysort(2,[{Node, N, Gateway} | NewMap]).

% Update the list Sorted
update(Node, N, Gateway, Sorted) ->
    Length = entry(Node, Sorted),
    if 
	N < Length ->
	    replace(Node,N,Gateway,Sorted);
	true -> Sorted
    end.
