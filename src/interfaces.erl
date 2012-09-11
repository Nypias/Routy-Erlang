-module(interfaces).
-export([new/0, add/4, remove/2]).

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
