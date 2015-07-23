-module(bk_letter).
-compile(export_all).

last(List)->
	atom_to_list(lists:last(List)).


init(List)->
	lists:sublist(List,length(List)-1).


next(Char)->
	
	
	[Letter]=atom_to_list(erlang:list_to_atom(Char)),
	case Letter == $z of
		true -> 
			[$a];
		false ->
			[Letter + 1]
	end.

previous(Char)->
		[Letter]=atom_to_list(erlang:list_to_atom(Char)),
	case Letter == $a of
		true -> 
			[$z];
		false ->
			[Letter - 1]
	end.



ifCondition(Cond, Do)->
	case Cond of
		true-> Do;
		false -> false
	end.
