-module(dijkstra).
-export([entry/2]).

% Public functions

% Returns the length of the shortest path to the node
% 0 if the node is not found
entry(Node,Sorted) ->
    lists:foldl(fun({NameNode, Length, Gateway},Acc) -> 
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
