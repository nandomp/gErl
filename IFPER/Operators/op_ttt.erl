-module(op_ttt).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1]).
%-export([oper_1/1]).

oper_1(Regla)->
	
	atom2var(Regla,1).

oper_2(Regla)->
	
	atom2var(Regla,2).

oper_3(Regla)->
	
	atom2var(Regla,3).

oper_4(Regla)->
	
	atom2var(Regla,4).

oper_5(Regla)->
	
	atom2var(Regla,5).

oper_6(Regla)->
	
	atom2var(Regla,6).

oper_7(Regla)->
	
	atom2var(Regla,7).

oper_8(Regla)->
	
	atom2var(Regla,8).

oper_9(Regla)->
	
	atom2var(Regla,9).

%oper_1(Regla)->	
%	crg:crg(Regla).
%	atom2var(Regla,5).

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
			{ok,NewVar,1}=erl_scan:string([65+1-1]),
			atom2varPatterns(Patterns,NewVar++NewPat,Pos,Index-1)
	end.