-module(dijkstra).
-export([entry/2, replace/4, update/4]).

% Public functions

% Returns the length of the shortest path to the node
% 0 if the node is not found
entry(Node,Sorted) ->
    lists:foldl(fun({NameNode, Length, _},Acc) -> 
			if 
			    % Check if we are searching in a good node
			    NameNode == Node -> 
				if
				    Acc == 0 -> Length;
				    true -> erlang:min(Length,Acc)
				end;

			    true -> Acc
			end
		end,			 
	  0, Sorted).

% Replace the entry for Node in sorted with a new entry
replace(Node, N, Gateway, Sorted) ->
    NewMap = lists:keydelete(Node,1,Sorted),
    lists:sort([{Node, N, Gateway} | NewMap]).

% Update the list Sorted
update(Node, N, Gateway, Sorted) ->
    Length = entry(Node, Sorted),
    if 
	N < Length ->
	    replace(Node,N,Gateway,Sorted);
	true -> Sorted
    end.
