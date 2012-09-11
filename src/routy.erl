-module(routy).
-export([start/1, stop/1, init/1, router/6]).

start(Name) ->
    register(Name, spawn(fun() -> init(Name) end)).

stop(Node) ->
    Node ! stop,
    unregister(Node).

init(Name) ->
    Intf = interfaces:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map) ->
    receive
	{add, Node, Pid} ->
	    Ref = erlang:monitor(process,Pid),
	    Intf1 = interfaces:add(Node, Ref, Pid, Intf),
	    router(Name, N, Hist, Intf1, Table, Map);
	{remove, Node} ->
	    {ok, Ref} = interfaces:ref(Node, Intf),
	    erlang:demonitor(Ref),
	    Intf1 = interfaces:remove(Node, Intf),
	    router(Name, N, Hist, Intf1, Table, Map);
	{'DOWN', Ref, process, _, _} ->
	    {ok, Down} = interfaces:name(Ref, Intf),
	    io:format("~w: exit recived from ~w~n", [Name, Down]),
	    Intf1 = interfaces:remove(Down, Intf),
	    router(Name, N, Hist, Intf1, Table, Map);
	{status, From} ->
	    From ! {status, {Name, N, Hist, Intf, Table, Map}},
	    router(Name, N, Hist, Intf, Table, Map);
	{links, Node, R, Links} ->
	    case hist:update(Node, R, Hist) of
		{new, Hist1} ->
		    interfaces:broadcast({links, Node, R, Links}, Intf),
		    Map1 = map:update(Node, Links, Map),
		    router(Name, N, Hist1, Intf, Table, Map1);
		old ->
		    router(Name, N, Hist, Intf, Table, Map)
	    end;
	update ->
	    Table1 = dijkstra:table(intf:list(Intf), Map),
	    router(Name, N, Hist, Intf, Table1, Map);
	broadcast ->
	    Message = {links, Name, N, intf:list(Intf)},
	    intf:broadcast(Message, Intf),
	    router(Name, N+1, Hist, Intf, Table, Map);

	{route, Name, From, Message} ->
	    io:format("~w: received message ~w ~n", [Name, Message]),
	    router(Name, N, Hist, Intf, Table, Map);
	
	{route, To, From, Message} ->
	    io:format("~w: routing message (~w)", [Name, Message]),
	    case dijkstra:route(To, Table) of
		{ok, Gw} ->
		    case intf:lookup(Gw, Intf) of
			{ok, Pid} ->
			    Pid ! {route, To, From, Message};
			notfound ->
			    ok
		    end;
		notfound ->
		    ok
	    end,
	    router(Name, N, Hist, Intf, Table, Map);

	{send, To, Message} ->
	    self() ! {route, To, Name, Message},
	    router(Name, N, Hist, Intf, Table, Map);


	stop ->
	    ok
    end.
