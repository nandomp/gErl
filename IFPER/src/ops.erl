-module(ops).
-compile(export_all).






%%% SI ARGUMENTO Y RHS (solo un termino) COINCIDEN, GENERALIZA %%%%


patternAnd1Body2var(Regla)->
		
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	io:format("Pats: ~p~n",[Patterns]),
	EtsVars = ets:new('VarsUsed',[bag] ),
	


	%NewVar=freeVar(Patterns,0),
	%io:format("Nueva var: ~p~n",[NewVar]),
	%{NewPatterns,NewBody}=patternAnd1Body2varExps(Patterns,Body,[],Body,NewVar),
	
	
	{NewPatterns,NewBody}=patternAnd1Body2var2(Patterns,Body,EtsVars,[],[],1),
	io:format("New Patt-> ~p~n",[NewPatterns]),
	io:format("New Body-> ~p~n",[NewBody]),
	
	Return=printrules(Forms,NewPatterns,NewBody),
	Return.
	%%New = erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,lists:nth(1,NewPatterns),Guards,lists:nth(1,NewBody)}]}])),
	%Neww = erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,lists:nth(2,NewPatterns),Guards,lists:nth(2,NewBody)}]}])),
	
	
	%New2=re:replace(New, " ", "",[global,{return,list}]),
	%New3=re:replace(Neww, " ", "",[global,{return,list}]),
	%[string:sub_word(New2,1,$.),string:sub_word(New3,1,$.)].


patternAnd1Body2var2(Patterns,Body,EtsVars,NP,NB,Index)->
	
	case Index>length(Body) of
		false -> 
			NewVar=freeVar(Patterns,EtsVars),
			io:format("Nueva var: ~p~n",[NewVar]),
			{NewPatterns,NewBody}=patternAnd1Body2varExps(Patterns,[lists:nth(Index,Body)],[],Body,NewVar),
			NewBody2=consBody(Body,NewBody,Index),
			io:format("Body2 ~p~n",[NewBody2]),
			patternAnd1Body2var2(Patterns,Body,EtsVars,NP++[NewPatterns],NB++[NewBody2],Index+1);
		true ->
			{NP,NB}
	end.
	
%%% Devolver Reglas %%%

printrules(Forms,NewPatterns,NewBody)->
	
	printrules2(Forms,NewPatterns,NewBody,[],length(NewPatterns)).
  

printrules2(_,_,_,Return,0)->
		Return;

printrules2(Forms,NewPatterns,NewBody,Return,Index) ->
	
	io:format("Index.......~p~n",[Index]),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	New = erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,lists:nth(Index,NewPatterns),Guards,lists:nth(Index,NewBody)}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	printrules2(Forms,NewPatterns,NewBody,[string:sub_word(New2,1,$.)]++Return,Index-1).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%% 


consBody(Body,NewBody,Index)->
	io:format("Body: ~p~n",[Body]),
	io:format("NewBody: ~p~n",[NewBody]),
	io:format("Index: ~p~n",[Index]),
	consbody2(Body,NewBody,[],Index,length(Body)).
	
consbody2(_,_,Return,_,0)->
	io:format("Return: ~p~n",[Return]),
	Return;

consbody2(Body,NewBody,Return,Index,I)->
	
	case Index=/=I of
		true ->
			consbody2(Body,NewBody,Return++[lists:nth(I,Body)],Index,I-1);
		false ->
			consbody2(Body,NewBody,Return++[lists:nth(1,NewBody)],Index,I-1)
	end.



patternAnd1Body2varExps([Exp|Exps],Body,NP,NB,NewVar) ->	
			
			{NP2,NB2}=patternAnd1Body2varExp(Exp,Body,NP,NB,NewVar),
			patternAnd1Body2varExps(Exps,Body,NP2,NB2,NewVar);


patternAnd1Body2varExps(Other,Body,NP,NB,_) ->
		{NP,NB}.


%%%%%%%%%%%%%
%%igualdada de atomos

patternAnd1Body2varExp({atom,LINE,Atom},[{atom,LINE2,Atom}],NP,NB,NewVar) ->
	
					
				{NP++NewVar,NewVar};

patternAnd1Body2varExp({atom,LINE,Atom},Body,NP,NB,NewVar) ->
				
			
			{NP++[{atom,1,Atom}],NB};



%%Igualdad Integers

patternAnd1Body2varExp({integer,LINE,Int},[{integer,LINE2,Int}],NP,NB,NewVar) ->
	
					
				{NP++NewVar,NewVar};

patternAnd1Body2varExp({integer,LINE,Int},Body,NP,NB,NewVar) ->
				
			
			{NP++[{integer,LINE,Int}],NB};



%%Igualdad de listas

patternAnd1Body2varExp({cons,LINE,E1,E2},[{cons,LINE2,E1,E2}],NP,NB,NewVar) ->
	
					
			{NP++NewVar,NewVar};


patternAnd1Body2varExp({cons,LINE,E1,E2},Body,NP,NB,NewVar)->
	
	
	io:format("ENtro cons ~n",[]),
	{NP2,NB2}=patternAnd1Body2varExps([E1],Body,[],NB,NewVar),
	
	io:format("NP2 ~p~n",[NP2]),
	io:format("NB2 ~p~n",[NB2]),
	{NP3,NB3}=patternAnd1Body2varExps([E2],Body,[],NB2,NewVar),
	io:format("NP3 ~p~n",[NP3]),
	io:format("NB3 ~p~n",[NB3]),
	{NP++[{cons,1,lists:nth(1,NP2),lists:nth(1,NP3)}],NB3};
	
%%Igualdad de tuplas

patternAnd1Body2varExp({tuple,LINE,Exps},[{tuple,LINE2,Exps}],NP,NB,NewVar) ->
	
					
			{NP++NewVar,NewVar};


patternAnd1Body2varExp({tuple,LINE,Exps},Body,NP,NB,NewVar)->
	
	
	io:format("Entro tup ~n",[]),
	
	{NP2,NB2}=patternAnd1Body2varExps(Exps,Body,[],NB,NewVar),
	
	
	{NP++[{tuple,1,NP2}],NB2};


%% Otros

patternAnd1Body2varExp(Other,Body,NP,NB,NewVar) ->
		
	{NP++[Other],NB}.



%%%% EXTRACCION DE VARIABLES DE LOS PATRONES %%%


extractVars([Exp|ExpsPatterns], Vars)->
	
	extractVars2(Exp,Vars),extractVars(ExpsPatterns, Vars);

extractVars(Other,Vars)->
	ok.

	
extractVars2({var,LINE,Var},Vars)->

	%Vars ++ [{var,LINE,Var}];
	ets:insert(Vars,{var,LINE,Var});

extractVars2({cons,LINE,E1,E2},Vars) ->
	
	extractVars2(E1,Vars),extractVars2(E2,Vars);
	
extractVars2({tuple,LINE,Exps},Vars) ->
	
	extractVars(Exps,Vars);

extractVars2(Other, Vars)->
	ok.


freeVar(Patterns,EtsVars)->
	
	extractVars(Patterns,EtsVars),
	%io:format("VarsUsed ~p~n",[VarsUsed]),
	
	freeVar2(EtsVars,0).

	%{ok,[NewVar],_}=erl_scan:string([65+Index]),
	%case lists:keyfind(element(3,NewVar),3,VarsUsed) of
	%	false -> 
	%		[NewVar];
	%		
	%	_Else ->
	%		freeVar(Patterns,Index+1)
	%end.
			

freeVar2(EtsVars,Index)->
	
		{ok,[NewVar],_}=erl_scan:string([65+Index]),
		{var,LINE,Var}=NewVar,
		case ets:match(EtsVars,{'_','_',Var}) of 
			[] -> 
				ets:insert(EtsVars,{var,LINE,Var}),
				[NewVar];
			_Else ->
				freeVar2(EtsVars,Index+1)
		end.

