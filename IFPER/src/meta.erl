-module(meta).
-compile(export_all).
-type suc() :: integer().
-spec succ(Num :: suc()) -> suc().


arity(Table)->
	[{Key,{SelecRule,Rew,PrevActs}}]=ets:lookup(Table, 1),
	Rule=lists:append(SelecRule,"."),
	P1 = smerl:new(rule),
	{ok, P2} = smerl:add_func(P1, Rule),	
	smerl:compile(P2),
 	%smerl:for_module(P2).
	[{function,_,_,Arity,_}|_]=smerl:get_forms(P2),
	Arity.

s(Int)->
	Int+1.


suma(X,Y) ->
	X+Y.


succ(X)-> X+1.

cobertura(Rule1,Rule2)->
	 P1 = smerl:new(rule),
	{ok, P2} = smerl:add_func(P1, Rule1),	
	Forms1=smerl:get_forms(P2),
	 
	 P3 = smerl:new(rule),
	{ok, P4} = smerl:add_func(P3, Rule2),	
	Forms2=smerl:get_forms(P4),
	 
	 io:format("R1: ~p~n",[Forms1]),
	  io:format("R2: ~p~n",[Forms2]),
	 
	  io:format("Iguales: ~p~n",[Forms2==Forms1]).


cob(Rule1,Ex)->
	
			P1 = smerl:new(rule),
			{ok, P2} = smerl:add_func(P1, Rule1),	
			
			
			Ej=string:sub_word(Ex,1,$-),
			Rhs=string:sub_word(Ex,2,$>),			
			Code=lists:append("try ",Ej),
			Code1=lists:append(Code, " of Val -> case Val=="),
			Code2=lists:append(Code1,Rhs),
			Code3=lists:append(Code2," of true -> true; false -> false end catch _:_ -> false end."),
			
			CodeFin=lists:append("cubre()->",Code3),
			io:format("Code: ~p~n",[CodeFin]),
			{ok, P3} = smerl:add_func(P2,CodeFin),
			smerl:compile(P3),
			Sol=rule:cubre(),
			
			io:format("SOL= ~w~n",[Sol]),
			case Sol of
				false ->
					io:format("Entro en FALSE~n",[]);
					
				Else ->
					io:format("Entro en ELSE~n",[])
					
			end.
	


t([X,a],Y)-> X.


atom2var(Regla,Pos)->
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,atom2varPatterns(Patterns,[],Pos,Arity),Guards,Body}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	New3=string:sub_word(New2,1,$.).

atom2varPatterns(_,NewPat,_,0)->
	NewPat;

atom2varPatterns(Patterns,NewPat,Pos,Index)->
	
	case Index =/= Pos of
		true ->
			atom2varPatterns(Patterns,[lists:nth(Index,Patterns)]++NewPat,Pos,Index-1);
		false ->
			{ok,NewVar,1}=erl_scan:string([65+Pos-1]),
			atom2varPatterns(Patterns,NewVar++NewPat,Pos,Index-1)
	end.
	

oper()->
	{ok,OperatorsModule}=compile:file("prueba.erl"),
	OperatorsModule:oper_1("play(a,b,c)->true").
	

