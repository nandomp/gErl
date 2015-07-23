-module(op_letter_2).
%compile(export_all).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1,oper_5/1,oper_6/1]).



oper_1(Rule)->
	insInit(Rule).

oper_2(Rule)->
	insLast(Rule).

oper_3(Rule)->
	insNext(Rule).

oper_4(Rule)->
	insPrev(Rule).

%oper_5(Rule)->
	%ins_if(Rule, {op,1,'==',{call,1,{remote,1,{atom,1,bk_letter},{atom,1,last}},[{var,1,'List'}]},{string,1,"a"}}).

%oper_6(Rule)->
	%ins_if(Rule, {op,1,'=/=',{call,1,{remote,1,{atom,1,bk_letter},{atom,1,last}},[{var,1,'List'}]},{string,1,"a"}}).

oper_5(Rule)->
	ins_if(Rule, {op,1,'==',{op,1,'rem',{call,1,{atom,1,length},[{var,1,'List'}]},{integer,1,3}},{integer,1,0}}).

oper_6(Rule)->
	ins_if(Rule, {op,1,'=/=',{op,1,'rem',{call,1,{atom,1,length},[{var,1,'List'}]},{integer,1,3}},{integer,1,0}}).



caseArg_remotes(Arg,[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,init}},Temp}])->
	case Arg of
		
		[{var,_,V}] ->
				[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,init}},[{var,LINE,V}]}];			
		[{call,_,{remote,_,BK2,{atom,_,Func}},Arg2}]->
				[{call,LINE,{remote,LINE,BK2,{atom,LINE,Func}},caseArg_remotes(Arg2,[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,init}},Arg2}])}];

		_Else ->
			Arg
	end;

caseArg_remotes(Arg,[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,last}},Temp}])->
	case Arg of
		
		[{var,_,V}] ->
				Arg;			
		[{call,_,{remote,_,BK2,{atom,_,init}},Arg2}]->
				[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,last}},[{call,LINE,{remote,LINE,BK2,{atom,LINE,init}},Arg2}]}];

		[{call,_,{remote,_,BK2,{atom,_,last}},Arg2}]->
				Arg;

		[{call,_,{remote,_,BK2,{atom,_,Func}},Arg2}]->
				[{call,LINE,{remote,LINE,BK2,{atom,LINE,Func}},caseArg_remotes(Arg2,[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,last}},Arg2}])}];

		_Else ->
				Arg
	end;

caseArg_remotes(Arg,[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,next}},Temp}])->
	%io:format("~p~n",[Arg]),
	case Arg of
		
		[{var,_,V}] ->
				%io:format("Entro por Var",[]),
				Arg;			
		[{call,_,{remote,_,BK2,{atom,_,last}},Arg2}]->
				%io:format("Entro por Last",[]),
				[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,next}},[{call,LINE,{remote,LINE,BK2,{atom,LINE,last}},Arg2}]}];				
		[{call,_,{remote,_,BK2,{atom,_,Func}},Arg2}]->
				%io:format("Entro por Func",[]),
				[{call,LINE,{remote,LINE,BK2,{atom,LINE,Func}},caseArg_remotes(Arg2,[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,next}},Arg2}])}];	
		_Else ->
				%io:format("Entro por Else",[]),
				Arg
	end;
			
caseArg_remotes(Arg,[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,previous}},Temp}])->
	case Arg of
		
		[{var,_,V}] ->
				Arg;			
		[{call,_,{remote,_,BK2,{atom,_,last}},Arg2}]->
				[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,previous}},[{call,LINE,{remote,LINE,BK2,{atom,LINE,last}},Arg2}]}];				
		[{call,_,{remote,_,BK2,{atom,_,Func}},Arg2}]->
				[{call,LINE,{remote,LINE,BK2,{atom,LINE,Func}},caseArg_remotes(Arg2,[{call,LINE,{remote,LINE,{atom,LINE,BK},{atom,LINE,previous}},Arg2}])}];	
		_Else ->
				Arg
	end.



insInit(Rule)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body  of
		[{string,_,_}]->
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_letter},{atom,LINE,init}},[{var,LINE,'List'}]}],
			NewPatterns=[{var,LINE,'List'}];
		[{call,LINE,{remote,LINE,BK,{atom,LINE,Fun}},Args}]->
			NewPatterns=Patterns,
			NewBody=[{call,LINE,{remote,LINE,BK,{atom,LINE,Fun}},caseArg_remotes(Args,[{call,LINE,{remote,LINE,{atom,LINE,bk_letter},{atom,LINE,init}},[{var,LINE,'List'}]}])}];
		_Else ->
			NewPatterns=Patterns,
			NewBody=Body
	end,
	
	
	New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE,Name,Arity,[{clause,LINE,NewPatterns,Guards,NewBody}]}])),
	New31=re:replace(New21, "\n", "",[global,{return,list}]),
	New41=re:replace(New31, "\r", "",[global,{return,list}]),
	New51=re:replace(New41, "\t", " ",[global,{return,list}]),
	[string:sub_word(New51,1,$.)].




insLast(Rule)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body  of
		[{string,_,_}]->
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_letter},{atom,LINE,last}},[{var,LINE,'List'}]}],
			NewPatterns=[{var,LINE,'List'}];
		[{call,LINE,{remote,LINE,BK,{atom,LINE,init}},Args}] ->
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_letter},{atom,LINE,last}},[{call,LINE,{remote,LINE,BK,{atom,LINE,init}},Args}]}],
			NewPatterns=Patterns;
		[{call,LINE,{remote,LINE,BK,{atom,LINE,Fun}},Args}]->
			NewPatterns=Patterns,
			NewBody=[{call,LINE,{remote,LINE,BK,{atom,LINE,Fun}},caseArg_remotes(Args,[{call,LINE,{remote,LINE,{atom,LINE,bk_letter},{atom,LINE,last}},[{var,LINE,'List'}]}])}];
		_Else ->
			NewPatterns=Patterns,
			NewBody=Body
	end,
	
	
	New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE,Name,Arity,[{clause,LINE,NewPatterns,Guards,NewBody}]}])),
	New31=re:replace(New21, "\n", "",[global,{return,list}]),
	New41=re:replace(New31, "\r", "",[global,{return,list}]),
	New51=re:replace(New41, "\t", " ",[global,{return,list}]),
	[string:sub_word(New51,1,$.)].





insNext(Rule)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body  of
		
		[{call,LINE,{remote,LINE,BK,{atom,LINE,last}},Args}]->
			NewPatterns=Patterns,
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_letter},{atom,LINE,next}},[{call,LINE,{remote,LINE,BK,{atom,LINE,last}},Args}]}];
		
		[{call,LINE,{remote,LINE,BK,{atom,LINE,Fun}},Args}]->
			NewPatterns=Patterns,
			NewBody=[{call,LINE,{remote,LINE,BK,{atom,LINE,Fun}},caseArg_remotes(Args,[{call,LINE,{remote,LINE,{atom,LINE,bk_letter},{atom,LINE,next}},[{var,LINE,'List'}]}])}];
	
		_Else ->
			NewPatterns=Patterns,
			NewBody=Body
	end,
	
	
	New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE,Name,Arity,[{clause,LINE,NewPatterns,Guards,NewBody}]}])),
	New31=re:replace(New21, "\n", "",[global,{return,list}]),
	New41=re:replace(New31, "\r", "",[global,{return,list}]),
	New51=re:replace(New41, "\t", " ",[global,{return,list}]),
	[string:sub_word(New51,1,$.)].


insPrev(Rule)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body  of
		
		[{call,LINE,{remote,LINE,BK,{atom,LINE,last}},Args}]->
			NewPatterns=Patterns,
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_letter},{atom,LINE,previous}},[{call,LINE,{remote,LINE,BK,{atom,LINE,last}},Args}]}];
		
		[{call,LINE,{remote,LINE,BK,{atom,LINE,Fun}},Args}]->
			NewPatterns=Patterns,
			NewBody=[{call,LINE,{remote,LINE,BK,{atom,LINE,Fun}},caseArg_remotes(Args,[{call,LINE,{remote,LINE,{atom,LINE,bk_letter},{atom,LINE,previous}},[{var,LINE,'List'}]}])}];
		_Else ->
			NewPatterns=Patterns,
			NewBody=Body
	end,
	
	
	New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE,Name,Arity,[{clause,LINE,NewPatterns,Guards,NewBody}]}])),
	New31=re:replace(New21, "\n", "",[global,{return,list}]),
	New41=re:replace(New31, "\r", "",[global,{return,list}]),
	New51=re:replace(New41, "\t", " ",[global,{return,list}]),
	[string:sub_word(New51,1,$.)].


ins_if(Rule,Cond)->
	
	SelectedRule=Rule++".",
	{ok,String,LINE}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	%%COnd== {op,1,'==',{call,1,{remote,1,{atom,1,bk_letter},{atom,1,last}},[{var,1,'List'}]},{string,1,"a"}}
	
	case Body  of
		
		[{call,_,{remote,_,{atom,_,bk_letter},{atom,_,ifCondition}},[_,Else]}] ->
			NewPatterns=[{var,LINE,'List'}],
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_letter},{atom,LINE,ifCondition}},[Cond,Else]}];
		[{call,_,Remote,Args}]->
			NewPatterns=[{var,LINE,'List'}],
			NewBody=[{call,LINE,{remote,LINE,{atom,LINE,bk_letter},{atom,LINE,ifCondition}},[Cond,{call,LINE,Remote,Args}]}];
		_Else ->
			NewPatterns=Patterns,
			NewBody=Body
					
	end,
	
	
	New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE,Name,Arity,[{clause,LINE,NewPatterns,Guards,NewBody}]}])),
	New31=re:replace(New21, "\n", "",[global,{return,list}]),
	New41=re:replace(New31, "\r", "",[global,{return,list}]),
	New51=re:replace(New41, "\t", " ",[global,{return,list}]),
	[string:sub_word(New51,1,$.)].