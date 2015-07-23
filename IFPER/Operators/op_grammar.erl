-module(op_grammar).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1]).%,oper_5/1]).


oper_1(Rule)->
	sintaxis(Rule, np,2).

oper_2(Rule)->
	sintaxis(Rule, np,3).
	
oper_3(Rule)->
	sintaxis(Rule, vp,1).

oper_4(Rule)->
	sintaxis(Rule, vp,2).

%oper_5(Rule)->
%	sintaxis(Rule, conj,1).



sintaxis(Rule, Cons, Num)->

	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,Body}]}=Forms,
	
	
	[List]= Patterns,
	LastVar=operatorsSet:look4VarList(lists:nth(1,Patterns)),
	{NewList,NewVars}= generaliseList(List, LastVar, Num,[],1),
	case length(NewVars)==Num of
		true ->
			
			%io:format("New List: ~p, NewVars: ~p~n",[NewList,NewVars]),
			
	
			[B]=Body,		
			NewBody= [{op,LINE2,'and',B,{call,LINE2,{remote,LINE2,{atom,LINE2,bk_grammar},{atom,LINE2,Cons}},NewVars}}],
	
			%io:format("New Body: ~p~n",[NewBody]),
			New2=erl_prettypr:format(erl_syntax:form_list([{function,LINE1,Name,Arity,[{clause,LINE2,[NewList],Guards,NewBody}]}])),
			New3=re:replace(New2, "\n", "",[global,{return,list}]),
			New4=re:replace(New3, "\r", "",[global,{return,list}]),
			[string:sub_word(New4,1,$.)];
		false ->
			[Rule]
	end.
	


generaliseList(List, LastVar, Num,Vars, Index)->
	
	case Index==Num+1 of
		false ->
			{NewList,Var}=operatorsSet:lookforNthList(List,LastVar+Index),
			%io:format("NewVar: ~p, NEwList: ~p~n",[Var,NewList]),
			generaliseList(NewList, LastVar, Num,Vars++Var, Index+1);
		true ->
			{List,Vars}
	end.





		