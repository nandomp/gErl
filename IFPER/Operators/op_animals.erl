-module(op_animals).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1,oper_5/1,oper_6/1,oper_7/1,oper_8/1,oper_9/1,oper_10/1,oper_11/1,oper_12/1,oper_13/1,oper_14/1]).

oper_1(Regla)->
	animals_chars(Regla,has_covering,hair).

oper_2(Regla)->
	animals_chars(Regla,has_covering,none).

oper_3(Regla)->
	animals_chars(Regla,has_covering,scales).

oper_4(Regla)->
	animals_chars(Regla,has_covering,feathers).

oper_5(Regla)->
	animals_chars(Regla,has_legs,4).

oper_6(Regla)->
	animals_chars(Regla,has_legs,0).

oper_7(Regla)->
	animals_chars(Regla,has_legs,2).

oper_8(Regla)->
	animals_chars(Regla,has_milk,false).

oper_9(Regla)->
	animals_chars(Regla,homeothermic,false).

oper_10(Regla)->
	animals_chars(Regla,habitat,land).

oper_11(Regla)->
	animals_chars(Regla,habitat,water).

oper_12(Regla)->
	animals_chars(Regla,habitat,air).

oper_13(Regla)->
	animals_chars(Regla,habitat,caves).

oper_14(Regla)->
	animals_chars(Regla,has_eggs,false).

oper_15(Regla)->
	animals_chars(Regla,has_gills,false).






animals_chars(Regla, Argument, Other)->

	
	
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	%Generalización att1
	{ok,[NewVar],1}=erl_scan:string("X"),
	{NewPatterns,Nths}= lists:mapfoldl(fun (X,Nth) -> case Nth == 1 of true -> {NewVar,Nth+1}; false-> {X,Nth+1} end end, 1, Patterns),
	
	
	[B]=Body,
	case Other of
		false ->
			NewBody= [{op,1,'and',B,{call,1,{remote,1,{atom,1,bk_animals},{atom,1,Argument}},[{var,1,'X'}]}}];
		_else ->
			{ok,[Other2],_} = erl_scan:string(lists:flatten(io_lib:format("~p", [Other]))),
			NewBody= [{op,1,'and',B,{call,1,{remote,1,{atom,1,bk_animals},{atom,1,Argument}},[{var,1,'X'},Other2]}}]
	end,

	
	
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPatterns,Guards,NewBody}]}])),
	New3=re:replace(New2, "\n", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].
