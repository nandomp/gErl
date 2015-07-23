-module(op_ooo).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1,oper_5/1]).%,oper_6/1]).

oper_1(Rule)->
	operatorsSet:att2var(Rule, 1).


oper_2(Rule)->
	ins_mapfold(Rule).

oper_3(Rule)->
	ins_dif(Rule).

oper_4(Rule)->
	ins_simFuncX(Rule,sim_Hamming).

oper_5(Rule)->
	ins_simFuncX(Rule,sim_difElem).

%oper_6(Rule)->
%	ins_simFuncX(Rule,sim_simple).


ins_mapfold(Rule)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body  of
		[{integer,LINE,_}]->%%Generic mymapfoldr function
			[Pat]=Patterns,
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,mymapfoldl}},[{tuple,LINE,[{atom,LINE,bk_ooo},{atom,LINE,func_gen}]},{integer,LINE,0},Pat]}];
			
		%[{cons,LINE,_,_}]->
		%	[Pat]=Patterns,
		%	NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,mymapfoldl}},[{tuple,LINE,[{atom,LINE,bk_ooo},{atom,LINE,func_gen}]},{integer,LINE,0},Pat]}];
			
		_Else ->
			NewBody=Body
			
	end,
	
	
	New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE,Name,Arity,[{clause,LINE,Patterns,Guards,NewBody}]}])),
	New31=re:replace(New21, "\n", "",[global,{return,list}]),
	New41=re:replace(New31, "\r", "",[global,{return,list}]),
	New51=re:replace(New41, "\t", " ",[global,{return,list}]),
	[string:sub_word(New51,1,$.)].


ins_dif(Rule)->
	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body  of
		[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,mymapfoldl}},_}]->
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,dif}},Body}];
			
		[{cons,_,_,_}]->
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,dif}},Body}];
			
		_Else ->			
			NewBody=Body
	end,
	
	
	New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE,Name,Arity,[{clause,LINE,Patterns,Guards,NewBody}]}])),
	New31=re:replace(New21, "\n", "",[global,{return,list}]),
	New41=re:replace(New31, "\r", "",[global,{return,list}]),
	New51=re:replace(New41, "\t", " ",[global,{return,list}]),
	[string:sub_word(New51,1,$.)].



ins_simFunc(Rule)->

	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body  of
		
	
		[{call,_,{remote,_,{atom,_,bk_ooo},{atom,_,mymapfoldl}},[_,_,List]}]->
			
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,mymapfoldl}},[{tuple,LINE,[{atom,LINE,bk_ooo},{atom,LINE,sim_simple}]},List,List]}];
		
		[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,max}},[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,mymapfoldl}},[_,_,List]}]}]->
																							
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,max}},
					  [{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,mymapfoldl}},[{tuple,LINE,[{atom,LINE,bk_ooo},{atom,LINE,sim_simple}]},List,List]}]}];
		
		
			
		_Else ->			
			NewBody=Body
	end,
	
	
	New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE,Name,Arity,[{clause,LINE,Patterns,Guards,NewBody}]}])),
	New31=re:replace(New21, "\n", "",[global,{return,list}]),
	New41=re:replace(New31, "\r", "",[global,{return,list}]),
	New51=re:replace(New41, "\t", " ",[global,{return,list}]),
	[string:sub_word(New51,1,$.)].



ins_simFuncX(Rule,Func)->

	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body  of
		
		[{call,_,{remote,_,{atom,_,bk_ooo},{atom,_,mymapfoldl}},[{tuple,_,[{atom,_,bk_ooo},{atom,_,Func}]},List,List]}]->
			
			NewBody=Body;
		
		[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,mymapfoldl}},[_,_,List]}]->
			
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,mymapfoldl}},[{tuple,LINE,[{atom,LINE,bk_ooo},{atom,LINE,Func}]},List,List]}];
		
		[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,max}},[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,mymapfoldl}},[_,_,List]}]}]->
																							
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,max}},
					  [{call,LINE,{remote,LINE,{atom,LINE,bk_ooo},{atom,LINE,mymapfoldl}},[{tuple,LINE,[{atom,LINE,bk_ooo},{atom,LINE,Func}]},List,List]}]}];
			
		_Else ->			
			NewBody=Body
	end,
	
	
	New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE,Name,Arity,[{clause,LINE,Patterns,Guards,NewBody}]}])),
	New31=re:replace(New21, "\n", "",[global,{return,list}]),
	New41=re:replace(New31, "\r", "",[global,{return,list}]),
	New51=re:replace(New41, "\t", " ",[global,{return,list}]),
	[string:sub_word(New51,1,$.)].