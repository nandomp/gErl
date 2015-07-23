-module(op_web).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1,oper_5/1,oper_6/1,oper_7/1,oper_8/1,oper_9/1,oper_10/1,oper_11/1,oper_12/1,oper_13/1,oper_14/1,oper_15/1,oper_16/1,oper_17/1,oper_18/1,oper_19/1,oper_20/1,oper_21/1,oper_22/1,oper_23/1,oper_24/1,oper_25/1,oper_26/1,oper_27/1,oper_28/1,oper_29/1,oper_30/1,oper_31/1,oper_32/1,oper_33/1,oper_34/1,oper_35/1,oper_36/1,oper_37/1,oper_38/1,oper_39/1,oper_40/1,oper_41/1,oper_42/1,oper_43/1,oper_44/1,oper_45/1,oper_46/1,oper_47/1,oper_48/1,oper_49/1,oper_50/1]).
%-compile(export_all).

%oper_1(Regla)->
%	atom2var(Regla,1).

oper_1(Regla)->
	atom2var(Regla,1).

oper_2(Regla)->
	atom2var(Regla,2).

oper_3(Regla)->
	atom2var(Regla,3).

oper_4(Regla)->
	bk_1(Regla,2).

oper_5(Regla)->
	insertKeyConditions(Regla,olympics).

oper_6(Regla)->
	insertKeyConditions(Regla,held).

oper_7(Regla)->
	insertKeyConditions(Regla,summer).

oper_8(Regla)->
	insertKeyConditions(Regla,athens).

oper_9(Regla)->
	insertKeyConditions(Regla,football).

oper_10(Regla)->
	insertKeyConditions(Regla,champion).

oper_11(Regla)->
	insertKeyConditions(Regla,europe).

oper_12(Regla)->
	insertKeyConditions(Regla,10).

oper_13(Regla)->
	insertKeyConditions(Regla,20).

oper_14(Regla)->
	insertKeyConditions(Regla,30).

oper_15(Regla)->
	insertKeyConditions(Regla,40).

oper_16(Regla)->
	bk_2(Regla,1).

oper_17(Regla)->
	insertKeyConditionsGraph(Regla,1,olympics).

oper_18(Regla)->
	insertKeyConditionsGraph(Regla,1,games).

oper_19(Regla)->
	insertKeyConditionsGraph(Regla,1,swim).

oper_20(Regla)->
	insertKeyConditionsGraph(Regla,1,win).

oper_21(Regla)->
	insertKeyConditionsGraph(Regla,1,boxing).

oper_22(Regla)->
	insertKeyConditionsGraph(Regla,1,medal).

oper_23(Regla)->
	insertKeyConditionsGraph(Regla,1,europe).

oper_24(Regla)->
	insertKeyConditionsGraph(Regla,1,final).

oper_25(Regla)->
	insertKeyConditionsGraph(Regla,1,best).

oper_26(Regla)->
	insertKeyConditionsGraph(Regla,1,player).

oper_27(Regla)->
	insertKeyConditionsGraph(Regla,1,football).

oper_28(Regla)->
	insertKeyConditionsGraph(Regla,1,match).

oper_29(Regla)->
	insertKeyConditionsGraph(Regla,1,team).

oper_30(Regla)->
	insertKeyConditionsGraph(Regla,1,players).

oper_31(Regla)->
	insertKeyConditionsGraph(Regla,1,scores).

oper_32(Regla)->
	insertKeyConditionsGraph(Regla,1,results).

oper_33(Regla)->
	insertKeyConditionsGraph(Regla,1,referees).

oper_34(Regla)->
	insertKeyConditionsGraph(Regla,2,olympics).

oper_35(Regla)->
	insertKeyConditionsGraph(Regla,2,games).

oper_36(Regla)->
	insertKeyConditionsGraph(Regla,2,swim).

oper_37(Regla)->
	insertKeyConditionsGraph(Regla,2,win).

oper_38(Regla)->
	insertKeyConditionsGraph(Regla,2,boxing).

oper_39(Regla)->
	insertKeyConditionsGraph(Regla,2,medal).

oper_40(Regla)->
	insertKeyConditionsGraph(Regla,2,europe).

oper_41(Regla)->
	insertKeyConditionsGraph(Regla,2,final).

oper_42(Regla)->
	insertKeyConditionsGraph(Regla,2,best).

oper_43(Regla)->
	insertKeyConditionsGraph(Regla,2,player).

oper_44(Regla)->
	insertKeyConditionsGraph(Regla,2,football).

oper_45(Regla)->
	insertKeyConditionsGraph(Regla,2,match).

oper_46(Regla)->
	insertKeyConditionsGraph(Regla,2,team).

oper_47(Regla)->
	insertKeyConditionsGraph(Regla,2,players).

oper_48(Regla)->
	insertKeyConditionsGraph(Regla,2,scores).

oper_49(Regla)->
	insertKeyConditionsGraph(Regla,2,results).

oper_50(Regla)->
	insertKeyConditionsGraph(Regla,2,referees).





%%% SIEMPRE QUE NO HAYA UNA VARIABLE YA %%%%
atom2var(Regla,Pos)->
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,atom2varPatterns(Patterns,[],Pos,Arity),Guards,Body}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	New3=re:replace(New2, "\n", "",[global,{return,list}]),
	New4=re:replace(New3, "\t", "",[global,{return,list}]),
	[string:sub_word(New4,1,$.)].


atom2varPatterns(_,NewPat,_,0)->
	NewPat;

atom2varPatterns(Patterns,NewPat,Pos,Index)->
	

	case Index =/= Pos of
		true ->
			atom2varPatterns(Patterns,[lists:nth(Index,Patterns)]++NewPat,Pos,Index-1);
		false ->
			case lists:nth(Index,Patterns)  of
				
				{var,_,_} ->
					atom2varPatterns(Patterns,[lists:nth(Index,Patterns)]++NewPat,Pos,Index-1);
				
				_Else ->
					
					
					atom2varPatterns(Patterns,givemeFreshVar()++NewPat,Pos,Index-1)
			end
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

atom2varPatternsWithVar(_,NewPat,_,_,0)->
	NewPat;

atom2varPatternsWithVar(Patterns,NewPat,Var,Pos,Index)->
	

	case Index =/= Pos of
		true ->
			atom2varPatternsWithVar(Patterns,[lists:nth(Index,Patterns)]++NewPat,Var,Pos,Index-1);
		false ->			
			atom2varPatternsWithVar(Patterns,Var++NewPat,Var,Pos,Index-1)
			
	end.


bk_1(Regla,PatternPos)->
	[R2]=oper_1(Regla),
	[R3]=oper_2(R2),
	[R4]=oper_3(R3),
	bk_1After(R4,PatternPos).


bk_1After(Regla,PatternPos)->
	
	
	
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
			
			{ok,BKModule} = compile:file("../Operators/bk_web.erl"),
			{ok,Meta_model} = smerl:for_module(BKModule),
			Op=smerl:get_exports(Meta_model),
			Func=element(1,lists:nth(1,Op)),
			%Arity=element(2,lists:nth(1,Op)),
			%[Bunch]=Patterns,			
			[NewVar2]=NewVar,
			NewBody=[{call,1,{remote,1,{atom,1,BKModule},{atom,1,Func}},[{nil,0},NewVar2]}],
			NewPatts=atom2varPatternsWithVar(Patterns,[],NewVar,PatternPos,Arity)
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
		
		[{call,LINE,{remote,LINE2,{atom,LINE3,MOD},{atom,LINE4,bk_1}},[OldCond,Bunch]}] ->
			Test=1,RuleNew=Rule,
			NewBody=[{call,LINE,{remote,LINE2,{atom,LINE3,MOD},{atom,LINE4,bk_1}},[addcond(OldCond,Condition),Bunch]}],
			Cond=erl_parse:normalise(OldCond);
	
		[{call,LINE,{remote,LINE2,{atom,LINE3,MOD},{atom,LINE4,FUNC}},[OldCond,Bunch]}] ->
			Test=2,RuleNew=Rule,
			NewBody=[],Cond=[];
			
		_Else ->
			NewBody=[],Cond=[],
			Test=3,
			[RuleNew]=bk_1(Rule,2)
			%{ok,BKModule} = compile:file("C:/Users/Nando/workspace/IFPER/Operators/bk_open.erl"),
			%{ok,Meta_model} = smerl:for_module(BKModule),
			%Op=smerl:get_exports(Meta_model),
			%Func=element(1,lists:nth(1,Op)),
			%%Arity=element(2,lists:nth(1,Op)),
			%[Bunch]=Patterns,			
			%NewBody=[{call,1,{remote,1,{atom,1,BKModule},{atom,1,Func}},[{nil,0},Bunch]}]
			
	end,
	
	%io:format("Memeber: Condition: ~p, Cons:~p~n",[Condition,Cond]),
	case Test of
		1 ->
	
			case lists:member(Condition, Cond)  of
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
			end;
		2 ->
			[Rule];
		3 ->
			insertKeyConditions(RuleNew,Condition)
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


bk_2(Regla,PatternPos)->
	[R2]=oper_1(Regla),
	[R3]=oper_2(R2),
	[R4]=oper_3(R3),
	bk_2After(R4,PatternPos).

bk_2After(Regla,PatternPos)->
	
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
			
			{ok,BKModule} = compile:file("../Operators/bk_web.erl"),
			{ok,Meta_model} = smerl:for_module(BKModule),
			Op=smerl:get_exports(Meta_model),
			Func=element(1,lists:nth(2,Op)),
			%Arity=element(2,lists:nth(1,Op)),
			%[Bunch]=Patterns,			
			[NewVar2]=NewVar,
			NewBody=[{call,1,{remote,1,{atom,1,BKModule},{atom,1,Func}},[{tuple,0,[{nil,0},{nil,0}]},NewVar2]}],
			NewPatts=atom2varPatternsWithVar(Patterns,[],NewVar,PatternPos,Arity)
			%io:format("Varrrrrrr: ~p~n",[NewVar])
	end,
	
	New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPatts,Guards,NewBody}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	New3=re:replace(New2, "\n", "",[global,{return,list}]),
	New4=re:replace(New3, "\t", "",[global,{return,list}]),
	[string:sub_word(New4,1,$.)].


insertKeyConditionsGraph(Rule,Pos,Condition)->
	
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body of
		
		[{call,LINE,{remote,LINE2,{atom,LINE3,MOD},{atom,LINE4,bk_2}},[Nodo,Bunch]}] ->
			
			NewBody=[{call,LINE,{remote,LINE2,{atom,LINE3,MOD},{atom,LINE4,bk_2}},[addcondGraph(Nodo,Pos,Condition),Bunch]}],
			%Cond=erl_parse:normalise(OldCond);
			New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
			%io:format("New: ~p~n",[New]),
			New2=re:replace(New, " ", "",[global,{return,list}]),
			%io:format("New2: ~p~n",[New2]),
			New3=re:replace(New2, "\n", "",[global,{return,list}]),
			%io:format("New3: ~p~n",[New3]),
			New4=re:replace(New3, "\t", "",[global,{return,list}]),
			[string:sub_word(New4,1,$.)];	
	
		[{call,LINE,{remote,LINE2,{atom,LINE3,MOD},{atom,LINE4,FUN}},[Nodo,Bunch]}] ->
			
			[Rule];
		
		_Else ->
			
			[RuleNew]=bk_2(Rule,1),
			insertKeyConditionsGraph(RuleNew,Pos,Condition)
			%Cond=[],
			
			%{ok,BKModule} = compile:file("C:/Users/Nando/workspace/IFPER/Operators/bk_open.erl"),
			%{ok,Meta_model} = smerl:for_module(BKModule),
			%Op=smerl:get_exports(Meta_model),
			%Func=element(1,lists:nth(1,Op)),
			%%Arity=element(2,lists:nth(1,Op)),
			%[Bunch]=Patterns,			
			%NewBody=[{call,1,{remote,1,{atom,1,BKModule},{atom,1,Func}},[{nil,0},Bunch]}]
			
	end.
	

		
	



addcondGraph(Nodo,Pos,Condition)->
	
	{A,B}=erl_parse:normalise(Nodo),
	
	case Pos of
		
		1 ->
				case lists:member(Condition, A) of
						
					true->
							Nodo;
					
					false ->
							A1=A++[Condition],
							erl_parse:abstract({A1,B})
				end;
		2->
				
				case lists:member(Condition, B) of
						
					true->
							Nodo;
					
					false ->
							B1=B++[Condition],
							erl_parse:abstract({A,B1})
				end
	end.

							
					
	
	
