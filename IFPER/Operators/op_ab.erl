-module(op_ab).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1]).

oper_1(Regla)->
	atom2var(Regla,1).

%oper_3(Regla)->
%	pattern2Body(Regla).

oper_2(Regla)->
	applymapGeneric(Regla).

oper_3(Regla)->
	bk_1(Regla).

oper_4(Regla)->
	bk_2(Regla).


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
			{ok,NewVar,1}=erl_scan:string([65+Pos-1]),
			atom2varPatterns(Patterns,NewVar++NewPat,Pos,Index-1)
	end.



pattern2Body(Regla)->
	
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,Patterns}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	[string:sub_word(New2,1,$.)].


%Body == lista
applymapGeneric(Regla)->
	
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body of 
		
		[{call,LINE,{remote,LINE2,{atom,LINE3,lists},{atom,LINE4,map}},[{tuple,LINE5,[{atom,LINE6,MOD},{atom,LINE7,FUNC}]},ARGS]}] ->
			
						
			%{ok,BKModule} = compile:file("C:/Users/Nando/workspace/IFPER/Operators/bk_ab.erl"),
			%{ok,Meta_model} = smerl:for_module(BKModule),
			%Op=smerl:get_exports(Meta_model),
			%Func=element(1,lists:nth(3,Op)),	
			NewBody=[{call,LINE,{remote,LINE2,{atom,LINE3,lists},{atom,LINE4,map}},[{tuple,LINE5,[{atom,LINE6,bk_ab},{atom,LINE7,bk_3}]},ARGS]}];
																					
		
		_Else ->
			
			NewBody=[{call,1,{remote,1,{atom,1,lists},{atom,1,map}},[{tuple,1,[{atom,1,bk_ab},{atom,1,bk_3}]},lists:nth(1,Patterns)]}]
	
	end,
	
	
	New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	[string:sub_word(New2,1,$.)].


bk_1(Regla)->
	
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body of
		
		[{call,LINE,{remote,LINE2,{atom,LINE3,lists},{atom,LINE4,map}},REMOTE}] ->
			
			case REMOTE of 
				
				[{tuple,LINE5,[{atom,LINE6,MOD},{atom,LINE7,FUNC}]},ARGS]->
					
					{ok,BKModule} = compile:file("../Operators/bk_ab.erl"),
					{ok,Meta_model} = smerl:for_module(BKModule),
					Op=smerl:get_exports(Meta_model),
					Func=element(1,lists:nth(1,Op)),
					%Arity=element(2,lists:nth(1,Op)),
					NewBody=[{call,LINE,{remote,LINE2,{atom,LINE3,lists},{atom,LINE4,map}},[{tuple,LINE5,[{atom,LINE6,BKModule},{atom,LINE7,Func}]},ARGS]}];					
				_Else ->
					
					NewBody=Body
			end;
		
		_Else ->
			
			NewBody=Body
	end,
	
	New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	[string:sub_word(New2,1,$.)].



bk_2(Regla)->
	
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body of
		
		[{call,LINE,{remote,LINE2,{atom,LINE3,lists},{atom,LINE4,map}},REMOTE}] ->
			
			case REMOTE of 
				
				[{tuple,LINE5,[{atom,LINE6,MOD},{atom,LINE7,FUNC}]},ARGS] ->
					
					{ok,BKModule} = compile:file("../Operators/bk_ab.erl"),
					{ok,Meta_model} = smerl:for_module(BKModule),
					Op=smerl:get_exports(Meta_model),
					Func=element(1,lists:nth(2,Op)),
					%Arity=element(2,lists:nth(1,Op)),
					NewBody=[{call,LINE,{remote,LINE2,{atom,LINE3,lists},{atom,LINE4,map}},[{tuple,LINE5,[{atom,LINE6,BKModule},{atom,LINE7,Func}]},ARGS]}];					
					
				_Else ->
					
					NewBody=Body
			end;
		
		_Else ->
			
			NewBody=Body
	end,
	
	New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	[string:sub_word(New2,1,$.)].