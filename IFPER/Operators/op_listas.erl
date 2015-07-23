-module(op_listas).
-export([oper_1/1,oper_2/1,oper_3/1]).

oper_1(Regla)->	
	operatorsSet:att2var(Regla,1).

oper_2(Regla)->	
	addHead(Regla).

oper_3(Regla)->	
	addLast(Regla).


addHead(Regla)->
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	NewB=newBody(Body,Patterns),
	
	%io:format("NewB: ~p~n",[NewB]),
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewB}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	New4=re:replace(New3, "\n", "",[global,{return,list}]),
	New5=re:replace(New4, "\t", "",[global,{return,list}]),
	[string:sub_word(New5,1,$.)].


newBody([Exp|Exps],New)->
	[newBody2(Exp,New)|newBody(Exps,New)];

newBody(Other,New)->
	Other.
	
newBody2({cons,LINE,{call,LINE,E1,E2},{nil,1}},New)->
	{cons,LINE,{call,LINE,E1,E2},{cons,LINE,{call,1,{remote,1,{atom,1,lists},{atom,1,nth}},[{integer,1,1}]++New},{nil,1}}};

newBody2({cons,LINE,{call,LINE,E1,E2},EE},New)->
	{cons,LINE,{call,LINE,E1,E2},newBody2(EE,New)};

newBody2({cons,LINE,E1,{nil,1}},New)->
	{cons,LINE,{call,1,{remote,1,{atom,1,lists},{atom,1,nth}},[{integer,1,1}]++New},{nil,1}};


newBody2({cons,LINE,E1,E2},New)->
	newBody2(E2,New);

newBody2(Other,New)->
	{cons,1,{call,1,{remote,1,{atom,1,lists},{atom,1,nth}},[{integer,1,1}]++New},{nil,1}}.
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



addLast(Regla)->
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	NewB=newBodyLast(Body,Patterns),
	
	%io:format("NewB: ~p~n",[NewB]),
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewB}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	New4=re:replace(New3, "\n", "",[global,{return,list}]),
	New5=re:replace(New4, "\t", "",[global,{return,list}]),
	[string:sub_word(New5,1,$.)].


newBodyLast([Exp|Exps],New)->
	[newBodyLast2(Exp,New)|newBodyLast(Exps,New)];

newBodyLast(Other,New)->
	Other.
	


newBodyLast2({cons,LINE,{call,LINE,E1,E2},{nil,1}},New)->
	{cons,LINE,{call,LINE,E1,E2},{cons,LINE,{call,1,{remote,1,{atom,1,lists},{atom,1,last}},New},{nil,1}}};

newBodyLast2({cons,LINE,E1,{nil,1}},New)->
	{cons,LINE,{call,1,{remote,1,{atom,1,lists},{atom,1,last}},New},{nil,1}};

newBodyLast2({cons,LINE,{call,LINE,E1,E2},EE},New)->
	{cons,LINE,{call,LINE,E1,E2},newBodyLast2(EE,New)};

newBodyLast2({cons,LINE,E1,E2},New)->
	newBody2(E2,New);

newBodyLast2(Other,New)->
	{cons,1,{call,1,{remote,1,{atom,1,lists},{atom,1,last}},New},{nil,1}}.

























head2rhs(Regla)->
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	[B]= head2rhs1(Patterns),
	NewBody=newBody(Body,B),
	%io:format("B: ~p, NewBody: ~p ~n",[B,NewBody]),
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].



	

head2rhs1([Exp|Exps])->
	[head2rhs2(Exp)|head2rhs1(Exps)];

head2rhs1(Other)->
	Other.


head2rhs2({cons,LINE,E1,E2})->
	E1;


head2rhs2(Other)->
	Other.	










tail2rhs(Regla)->
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	[B]= tail2rhs1(Patterns),
	NewBody=newBody(Body,B),
	%io:format("B: ~p, NewBody: ~p ~n",[B,NewBody]),
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].


tail2rhs1([Exp|Exps])->
	[tail2rhs2(Exp)|tail2rhs1(Exps)];

tail2rhs1(Other)->
	Other.

tail2rhs2({cons,LINE,E1,{nil,1}})->
	E1;	

tail2rhs2({cons,LINE,E1,E2})->
	tail2rhs2(E2);



tail2rhs2(Other)->
	Other.	


