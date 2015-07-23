-module(op_open).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1,oper_5/1,oper_6/1,oper_7/1,oper_8/1,oper_9/1,oper_10/1,oper_11/1,oper_12/1,oper_13/1,oper_14/1,oper_15/1]).
%-compile(export_all).

%oper_1(Regla)->
%	atom2var(Regla,1).

oper_1(Regla)->
	bk_1(Regla).

oper_2(Regla)->
	insertKeyConditions(Regla,abloy).

oper_3(Regla)->
	insertKeyConditions(Regla,chubb).

oper_4(Regla)->
	insertKeyConditions(Regla,rubo).

oper_5(Regla)->
	insertKeyConditions(Regla,yale).

oper_6(Regla)->
	insertKeyConditions(Regla,short).

oper_7(Regla)->
	insertKeyConditions(Regla,medium).

oper_8(Regla)->
	insertKeyConditions(Regla,long).

oper_9(Regla)->
	insertKeyConditions(Regla,narrow).

oper_10(Regla)->
	insertKeyConditions(Regla,normal).

oper_11(Regla)->
	insertKeyConditions(Regla,broad).

oper_12(Regla)->
	insertKeyConditions(Regla,1).

oper_13(Regla)->
	insertKeyConditions(Regla,2).

oper_14(Regla)->
	insertKeyConditions(Regla,3).

oper_15(Regla)->
	insertKeyConditions(Regla,4).


%oper_4(Regla)->
%	atom2var(Regla,4).

%oper_5(Regla)->
%	atom2var(Regla,4).


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
			{ok,NewVar,1}=erl_scan:string([65+(Pos-1)]),
			atom2varPatterns(Patterns,NewVar++NewPat,Pos,Index-1)
	end.

%%%%%%%%%%%
givemeFreshVar()->
	{MegaSecs, Secs, Microsecs}=erlang:now(),
	%{Year, Month, Day}=erlang:date(),
	%{Hour, Min, Sec}=erlang:time(),	
	%{ok,NewVar,1}=erl_scan:string("VAR"++erlang:integer_to_list(Year)++erlang:integer_to_list(Month)++erlang:integer_to_list(Day)++erlang:integer_to_list(Hour)++erlang:integer_to_list(Min)++erlang:integer_to_list(Sec)),
	{ok,NewVar,1}=erl_scan:string("VAR"++erlang:integer_to_list(Microsecs)),
	%io:format("Var ~p~n",[NewVar]).
	NewVar.



bk_1(Regla)->
	
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	NewVar=givemeFreshVar(),
	
	case Body of
		
		[{call,LINE,{remote,LINE2,{atom,LINE3,MOD},{atom,LINE4,FUNC}},ARGS}] ->
			
			NewBody=Body,
			NewPatts=Patterns;
	
		_Else ->
			
			{ok,BKModule} = compile:file("bk_open.erl"),
			{ok,Meta_model} = smerl:for_module(BKModule),
			Op=smerl:get_exports(Meta_model),
			Func=element(1,lists:nth(1,Op)),
			%Arity=element(2,lists:nth(1,Op)),
			[Bunch]=Patterns,			
			[NewVar2]=NewVar,
			NewBody=[{call,1,{remote,1,{atom,1,BKModule},{atom,1,Func}},[{nil,0},NewVar2]}],
			NewPatts=NewVar
			%io:format("Varrrrrrr: ~p~n",[NewVar])
	end,
	
	New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPatts,Guards,NewBody}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	New3=re:replace(New2, "\n", "",[global,{return,list}]),
	New4=re:replace(New3, "\t", "",[global,{return,list}]),
	[string:sub_word(New4,1,$.)].









insertKeyConditions(Rule,Condition)->
	
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body of
		
		[{call,LINE,{remote,LINE2,{atom,LINE3,MOD},{atom,LINE4,FUNC}},[OldCond,Bunch]}] ->
			
			NewBody=[{call,LINE,{remote,LINE2,{atom,LINE3,MOD},{atom,LINE4,FUNC}},[addcond(OldCond,Condition),Bunch]}],
			Cond=erl_parse:normalise(OldCond);
	
		_Else ->

			Cond=[],
			NewBody=Body
			%{ok,BKModule} = compile:file("C:/Users/Nando/workspace/IFPER/Operators/bk_open.erl"),
			%{ok,Meta_model} = smerl:for_module(BKModule),
			%Op=smerl:get_exports(Meta_model),
			%Func=element(1,lists:nth(1,Op)),
			%%Arity=element(2,lists:nth(1,Op)),
			%[Bunch]=Patterns,			
			%NewBody=[{call,1,{remote,1,{atom,1,BKModule},{atom,1,Func}},[{nil,0},Bunch]}]
			
	end,
	
	%io:format("Memeber: Condition: ~p, Cons:~p~n",[Condition,Cond]),
	case lists:member(Condition, Cond) of
		false ->
			%io:format("NewPrev: ~p~n",[erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])]),
			New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
			%io:format("New: ~p~n",[New]),
			New2=re:replace(New, " ", "",[global,{return,list}]),
			%io:format("New2: ~p~n",[New2]),
			New3=re:replace(New2, "\n", "",[global,{return,list}]),
			%io:format("New3: ~p~n",[New3]),
			New4=re:replace(New3, "\t", "",[global,{return,list}]),
			[string:sub_word(New4,1,$.)];
		
		true ->

			[Rule]
	end.

addcond({nil,Line},Condition)->	
	
	case erlang:is_number(Condition) of 
		
		true ->
			{cons,0,{integer,0,Condition},{nil,0}};
	
		false ->
			erl_parse:abstract([Condition])
	end;

addcond({cons,Line,E1,E2},Condition)->	
	
	{cons,Line,E1,addcond(E2,Condition)}.













