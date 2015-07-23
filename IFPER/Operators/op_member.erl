-module(op_member).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1,oper_5/1,oper_6/1]).


oper_1(Regla)->
	operatorsSet:head2varLists(Regla, 1).

oper_2(Regla)->
	operatorsSet:tail2varLists(Regla, 1).

oper_3(Regla)->
	att2Head(Regla, 1,2).
	
oper_4(Regla)->
	operatorsSet:att2var(Regla,2).

oper_5(Regla)->
	operatorsSet:recAddHead(Regla, 1).

oper_6(Regla)->
	operatorsSet:recAddTail(Regla, 1).



%oper_6(Regla)->
%	att2Tail(Regla, 1,2).

	
att2Head(Rule,AttList,Att2Change)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	{cons,LINE,E1,E2}=lists:nth(AttList, Patterns),
	{NewPatterns,Nths}= lists:mapfoldl(fun (X,Nth) -> case Nth == Att2Change of true -> {E1,Nth+1}; false -> {X,Nth+1} end end, 1, Patterns),
	
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPatterns,Guards,Body}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].

	
att2Tail(Rule,AttList,Att2Change)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	{cons,LINE,E1,E2}=lists:nth(AttList, Patterns),
	{NewPatterns,Nths}= lists:mapfoldl(fun (X,Nth) -> case Nth == Att2Change of true -> {E2,Nth+1}; false -> {X,Nth+1} end end, 1, Patterns),
	
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPatterns,Guards,Body}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].	
	
	
	
	
	
	
	
	
	
	
	
