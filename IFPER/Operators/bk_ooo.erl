-module(bk_ooo).
-compile(export_all).




mymapfoldl(Func,Acc,List)->
	{NL,NA}=lists:mapfoldl(Func,Acc,List),
	NL.
  
  
func_gen(X,Y)->
	{X,Y}.



max(List)->
	
	case erlang:is_list(List) of
		true ->
			case erlang:is_integer(lists:nth(1, List)) of
				true->
					Elem=lists:max(List),
					NumElem=lists:foldl(fun(X,Acc)-> case X == Elem of true -> Acc+1; false -> Acc end end, 0, List),
					case NumElem>1 of
						false ->
							string:str(List, [Elem]);
						true ->
							tie
					end;
				false ->
					false
			end;
		false ->
			false
	end.
	

dif(List)->
	
	case erlang:is_list(List) of
		true ->
			case erlang:is_integer(lists:nth(1, List)) of
				true->
					Unique=lists:foldl(fun(X,Acc)-> case lists:member(X, List--[X]) of true-> Acc; false -> string:str(List, [X]) end end,0, List),
					case Unique =/= 0 of
						true->
							Unique;
						false ->
							tie
					end;
				
				false ->
					false
			end;
		false ->
			false
	end.

sim_simple(Elem,List)->
	
	{lists:foldl(fun(X,Acc)->case Elem == X of true -> Acc;false -> Acc+1 end end, 0, List),List}.



sim_Hamming(Elem,List)->
	
	{lists:foldl(fun(X,Acc)-> Acc + hamming(Elem,X) end, 0, List),List}.



hamming(Elem,Xl)->
	
 	E=gb_sets:from_list(Elem),
	X=gb_sets:from_list(Xl),
	gb_sets:size(E)-gb_sets:size(gb_sets:intersection(E, X)).
	
	

prueba()->
	L=[a,a,b,a,a],
	
	mymapfoldl({bk_ooo,sim_simple},L,L).


sim_difElem(Elems, List)->

	{lists:foldl(fun(X,Acc)-> Acc+is1(X) end, 0, Elems),List}.
	
	
	


is1(Elem)->
	case lists:nth(2,erlang:atom_to_list(Elem))==49 of 
		true ->
			1;
		false ->
			0
end.

	
