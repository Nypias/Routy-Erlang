-module(interfaces).
-export([new/0, add/4, remove/2, lookup/2, ref/2]).

new() ->
    [].

add(Name, Ref, Pid, Intf) ->
    case lists:member({Name, Ref, Pid}, Intf) of
	true ->
	    Intf;
	false ->
	    [{Name, Ref, Pid} | Intf]
    end.

remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
    case lists:keyfind(Name,1,Intf) of
	{Name,_,Pid} ->
	    {ok, Pid};
	false ->
	    notfound
    end.

ref(Name, Intf) ->
    case lists:keyfind(Name,1,Intf) of
	{Name, Ref,_} ->
	    {ok, Ref};
	false ->
	    notfound
    end.
