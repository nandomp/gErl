-module(op_last).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1,oper_5/1,oper_6/1]).

oper_1(Regla)->	
	head2varLists(Regla).

oper_2(Regla)->	
	tail2varLists(Regla).

oper_3(Regla)->	
	recAddHead(Regla).

oper_4(Regla)->	
	recAddTail(Regla).

oper_5(Regla)->
	[R2]=head2varLists(Regla),
 	head2rhs(R2).

oper_6(Regla)->
	[R2]=tail2varLists(Regla),
 	tail2rhs(Regla).	


head2rhs(Regla)->
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	NewBody= head2rhs2(Patterns),
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].


head2rhs2([Exp|Exps])->
	[head2rhs2(Exp)|head2rhs2(Exps)];

head2rhs2({cons,LINE,E1,E2})->
	E1;
	
head2rhs2(Other)->
	Other.
	
tail2rhs(Regla)->
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	NewBody= tail2rhs2(Patterns),
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].


tail2rhs2([Exp|Exps])->
	[tail2rhs2(Exp)|tail2rhs2(Exps)];

tail2rhs2({cons,LINE,E1,E2})->
	E2;
	
tail2rhs2(Other)->
	Other.	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


head2varLists(Regla)->
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	%[{cons,LINE,E1,E2}]=Patterns,
	%{ok,[NewVar],1}=erl_scan:string([65]),
	
	NewPat= head2varList2(Patterns),
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPat,Guards,Body}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].


head2varList2([{cons,LINE,E1,E2}])->
	{ok,[NewVar],1}=erl_scan:string([65]),	
	[{cons,LINE,NewVar,E2}];

head2varList2([{nil,1}])->
	[{nil,1}].
	

tail2varLists(Regla)->
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	%[{cons,LINE,E1,E2}]=Patterns,
	%{ok,[NewVar],1}=erl_scan:string([66]),
	NewPat= tail2varList2(Patterns),	
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPat,Guards,Body}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].

tail2varList2([{cons,LINE,E1,{nil,1}}])->
	%{ok,[NewVar],1}=erl_scan:string([66]),	
	[{cons,LINE,E1,{nil,1}}];

tail2varList2([{cons,LINE,E1,E2}])->
	{ok,[NewVar],1}=erl_scan:string([66]),	
	[{cons,LINE,E1,NewVar}];

tail2varList2([{nil,1}])->
	[{nil,1}].


oneterm_rhs2var(Regla)->
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,LINE,Patterns,Guards,Body}]}=Forms,
	%io:format("Forms: ~p",[Forms]),
	%io:format("rhs: ~p",[Body]),
	Variables=atom2varBody(Patterns,[]),
	%io:format("New: ~p~n",[Variables]),
	
	NewRules=genrhsrules(Forms, Variables),
	case length(NewRules)==0 of
		true -> 
			[Regla];
		false ->
			NewRules
	end.
		

%io:format("NewRules ~p~n",[NewRules]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Recursive function with Head of list (pattern) %%%
recAddHead(Rule)->
	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	NewBody= recBodyHead(Patterns,Name),	
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].
	




recBodyHead([Exp|Exps],Name)->
	recBodyHead2(Exp,Name)++recBodyHead2(Exps,Name).

recBodyHead2({cons,LINE,E1,E2},Name)->
	[{call,LINE,{atom,LINE,Name},[E1]}];
	
recBodyHead2(Other,_)->
	Other.
	
	
recAddTail(Rule)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	NewBody= recBodyTail(Patterns,Name),	
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].	


recBodyTail([Exp|Exps],Name)->
	recBodyTail2(Exp,Name)++recBodyTail2(Exps,Name).

recBodyTail2({cons,LINE,E1,E2},Name)->
	[{call,LINE,{atom,LINE,Name},[E2]}];
	
recBodyTail2(Other,_)->
	Other.
	


atom2varBody([Exp|Exps],NewPat)->
	
	%io:format("Exp: ~p~n",[Exp]),
	%io:format("Exps: ~p~n",[Exps]),
	%io:format("NewPat -> ~p~n",[NewPat]),
	atom2varBody2(Exp,NewPat)++atom2varBody(Exps,NewPat);

atom2varBody(Other,NewPat)->
	
	NewPat.
	
	

atom2varBody2({var,LINE,V},NewPat)->
	
	[{var,LINE,V}]++NewPat;

atom2varBody2({cons,LINE,E1,E2},NewPat)->
	
	atom2varBody([E1],NewPat)++atom2varBody([E2],NewPat);

atom2varBody2({atom,LINE,A},NewPat)->
	
	NewPat;

atom2varBody2(_,NewPat)->
	
	NewPat.

	


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



