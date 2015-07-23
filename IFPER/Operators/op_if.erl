-module(op_if).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1]).
%-export([oper_5/1]).
oper_1(Regla)->
	atom2var(Regla,1).

oper_2(Regla)->
	atom2var(Regla,2).

oper_3(Regla)->
	atom2var(Regla,3).

oper_4(Regla)->
	patternAnd1Body2var(Regla).

%oper_5(Regla)->
%	crg:crg(Regla).

%oper_6(Regla)->
%	atom2var(Regla,6).

%oper_7(Regla)->
%	atom2var(Regla,7).

atom2var(Regla,Pos)->
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,atom2varPatterns(Patterns,[],Pos,Arity),Guards,Body}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	[string:sub_word(New2,1,$.)].

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


oneterm_rhs2var(Regla)->
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	%io:format("Forms: ~p",[Forms]),
	%io:format("rhs: ~p",[Body]).
	Variables=atom2varBody(Patterns,[],Arity),
	%io:format("New: ~p~n",[Variables]),
	NewRules=genrhsrules(Forms, Variables),
	case length(NewRules)==0 of
		true -> 
			[Regla];
		false ->
			NewRules
	end.
		

%io:format("NewRules ~p~n",[NewRules]).

atom2varBody(_,NewBodies,0)->
	NewBodies;

atom2varBody(Patterns,NewBodies,Index)->
	
	case lists:nth(Index, Patterns)   of
		{var,1,Var} ->
			atom2varBody(Patterns,[{var,1,Var}]++NewBodies,Index-1);			
		_Else ->
			atom2varBody(Patterns,NewBodies,Index-1)
	end.
	
genrhsrules(Forms, Variables )->
	
	
	genrhsrules2(Forms, Variables,[], length(Variables)).


genrhsrules2(_,_, NewRules, 0)->	
	NewRules;

genrhsrules2(Forms, Variables,NewRules, Index)->
	
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	New = erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,[lists:nth(Index,Variables)]}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	New3=string:sub_word(New2,1,$.),
	genrhsrules2(Forms, Variables, [New3]++NewRules, Index-1).







%%% SI ARGUMENTO Y RHS (solo un termino) COINCIDEN, GENERALIZA %%%%


patternAnd1Body2var(Regla)->
		
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	io:format("Pats: ~p~n",[Patterns]),
	NewVar=freeVar(Patterns,0),
	io:format("Nueva var: ~p~n",[NewVar]),
	{NewPatterns,NewBody}=patternAnd1Body2varExps(Patterns,Body,[],Body,NewVar),
	New = erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPatterns,Guards,NewBody}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	[string:sub_word(New2,1,$.)].


patternAnd1Body2varExps([Exp|Exps],Body,NP,NB,NewVar) ->	
	
		
			{NP2,NB2}=patternAnd1Body2varExp(Exp,Body,NP,NB,NewVar),
			patternAnd1Body2varExps(Exps,Body,NP2,NB2,NewVar);


patternAnd1Body2varExps(Other,Body,NP,NB,_) ->
		{NP,NB}.




patternAnd1Body2varExp({atom,1,Atom},Body,NP,NB,NewVar) ->
	
	case {atom,1,Atom} == lists:nth(1,Body) of
			true->				
				{NP++NewVar,NewVar};
				
			false ->
				{NP++[{atom,1,Atom}],NB}
	end;

patternAnd1Body2varExp({cons,LINE,E1,E2},Body,NP,NB,NewVar)->
	

	io:format("ENtro cons ~n",[]),
	{NP2,NB2}=patternAnd1Body2varExps([E1],Body,[],NB,NewVar),
	
	io:format("NP2 ~p~n",[NP2]),
	io:format("NB2 ~p~n",[NB2]),
	{NP3,NB3}=patternAnd1Body2varExps([E2],Body,[],NB,NewVar),
	
	{NP++[{cons,1,lists:nth(1,NP2),lists:nth(1,NP3)}],NB3};
	

patternAnd1Body2varExp(Other,Body,NP,NB,NewVar) ->
		
	{NP++[Other],NB}.



%%%% EXTRACCION DE VARIABLES DE LOS PATRONES %%%


extractVars([Exp|ExpsPatterns], Vars)->
	
	extractVars2(Exp,Vars)++extractVars(ExpsPatterns, Vars);

extractVars(Other,Vars)->
	Vars.

	
extractVars2({var,1,Var},Vars)->

	Vars ++ [{var,1,Var}];

extractVars2({cons,LINE,E1,E2},Vars) ->
	
	extractVars2(E1,Vars) ++ extractVars2(E2,Vars);
	

extractVars2(Other, Vars)->
	Vars.


freeVar(Patterns,Index)->
	VarsUsed=extractVars(Patterns,[]),
	io:format("VarsUsed ~p~n",[VarsUsed]),
	{ok,[NewVar],1}=erl_scan:string([65+Index]),
	case lists:member(NewVar,VarsUsed) of
		true-> 
			freeVar(Patterns,Index+1);
		false ->
			[NewVar]
	end.
			




