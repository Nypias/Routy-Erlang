-module(dijkstra).
-export([table/2,route/2]).

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

% Main algorithm for Dijkstra
iterate(Sorted, Map, Table) ->
    case Sorted of
	[] ->
	    Table;
	[{_,inf,_} | _] ->
	    Table;
	[Entry | Tail] ->
	    {NodeName,Length,Gateway} = Entry, 
	    case lists:keyfind(NodeName,1,Map) of
		{_,Reachables} ->
		    NewList = lists:foldl(fun(E1,SortedList) ->
						  update(E1,Length+1,Gateway,SortedList) end, 
					  Tail, Reachables);
		false ->
		    NewList = Tail
	    end,
	    iterate(NewList,Map,[{NodeName,Gateway} | Table])
    end.

% Produce a routing table
table(Gateways, Map) ->
    AllNodes = map:all_nodes(Map),
    InitSortedList = lists:keysort(2, lists:map(fun(Node) ->
							case lists:member(Node, Gateways) of
							    true ->
								{Node, 0, Node};
							    false ->
								{Node, inf, unknown}
							end
						end, AllNodes)),
    iterate(InitSortedList, Map, []).


% Search the routing table and return the gateway suitable to route messages to a node
route(Node, Table) ->
    case lists:keyfind(Node,1,Table) of
	{Dest, Gateway} ->
	    {ok, Gateway};
	false ->
	    notfound
    end.
