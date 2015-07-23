-module(clientWeka).

-compile(export_all).

client_DM_Gen() ->

	{boxDM, erlang:list_to_atom(lists:append("serverDM@",net_adm:localhost()))} ! {self(), "Model"},
	receive
		
		{ok, Var} ->
			io:format("Modelo Generado~n",[]),
			Var;	
		
		_Else ->
			io:format("Modelo NO Generado~n",[]),
				_Else
	
	after 15000 ->
			io:format("Time out Generate Model~n",[])
	end.


client_DM_Clas(Opt, MedRul, MedProg, RulRat, ProgRat, Rule, Size, Arity, Act, Prev1, Prev2, Prev3, Cob, CobNeg, NumPrevOps,Vars,Cons,Funcs,Strucs,Rec) ->
	
	%io:format("Dentro Client-> Opt: ~p, MedRules: ~p, MedProgs: ~p, RuleRatio: ~p, ProgRatio: ~p ~n",[Opt,MedRul,MedProg,RulRat,ProgRat]),
	
	{boxDM, erlang:list_to_atom(lists:append("serverDM@",net_adm:localhost()))} ! {self(), "Classify",Opt, MedRul, MedProg, RulRat, ProgRat, Rule, 
																				   Size, Arity, Act,  Prev1, Prev2, Prev3, Cob, CobNeg, NumPrevOps, Vars, Cons, Funcs, Strucs, Rec},
	receive
		
		{ok, Class} ->
			Class;	
		
		_Else ->
				_Else
	
	after 15000 ->
			io:format("Time out Classify ~n",[])
	end.






			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
	
	
