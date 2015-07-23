-module(op_raven2).
%-compile(export_all).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1,oper_5/1,oper_6/1,oper_7/1,oper_8/1]).



oper_1(Rule)->
	[R]=operatorsSet:att2var(Rule, 1),
	insert2obj(R,{bk_raven,identity_obj}).

oper_2(Rule)->
	[R]=operatorsSet:att2var(Rule, 1),
	insert2obj(R,{bk_raven,missing_obj}).

oper_3(Rule)->
	[R]=operatorsSet:att2var(Rule, 1),
	insert2obj(R,{bk_raven,or_figs}).

oper_4(Rule)->
	[R]=operatorsSet:att2var(Rule, 1),
	insert2obj(R,{bk_raven,xor_figs}).

oper_5(Rule)->
	[R]=operatorsSet:att2var(Rule, 1),
	insert2obj(R,{bk_raven,inter_figs}).

oper_6(Rule)->
	[R]=operatorsSet:att2var(Rule, 1),
	insert2att(R,{bk_raven,identity_att}).

oper_7(Rule)->
	[R]=operatorsSet:att2var(Rule, 1),
	insert2att(R,{bk_raven,missing_att}).

oper_8(Rule)->
	[R]=operatorsSet:att2var(Rule, 1),
	insert2att(R,{bk_raven,incr_att}).

%oper_9(Rule)->
%	operatorsSet:att2var(Rule, 1).


insert2obj(Rule,{Bk,Op})->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body  of
		
		[{cons,_,{call,_,_,Args},Tail2}]->
			NewBody=	[{cons,LINE,{call,LINE,{remote,LINE,{atom,LINE,Bk},{atom,LINE,Op}},Args},Tail2}],
			NewPatterns= Patterns;
		[{cons,_,{cons,_,{atom,_,_},Tail1},Tail2}]->
			NewBody=	[{cons,LINE,{call,LINE,{remote,LINE,{atom,LINE,Bk},{atom,LINE,Op}},Patterns},Tail2}],
			NewPatterns= Patterns;
		_Else ->
			NewPatterns=Patterns,
			NewBody=Body
	end,
	
	
	New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE,Name,Arity,[{clause,LINE,NewPatterns,Guards,NewBody}]}])),
	New31=re:replace(New21, "\n", "",[global,{return,list}]),
	New41=re:replace(New31, "\r", "",[global,{return,list}]),
	New51=re:replace(New41, "\t", " ",[global,{return,list}]),
	[string:sub_word(New51,1,$.)].




newatt_body({cons,LINE,{cons,_,{atom,_,_},Tail1},Tail2},Patterns,{Bk,Op},Id_att)->
	
	{cons,LINE,{call,LINE,{remote,LINE,{atom,LINE,Bk},{atom,LINE,Op}},Patterns++[{integer,LINE,Id_att}]},Tail2};
	
newatt_body({cons,LINE,{cons,_,{integer,_,_},Tail1},Tail2},Patterns,{Bk,Op},Id_att)->
	
	{cons,LINE,{call,LINE,{remote,LINE,{atom,LINE,Bk},{atom,LINE,Op}},Patterns++[{integer,LINE,Id_att}]},Tail2};

newatt_body({cons,LINE,{cons,_,{float,_,_},Tail1},Tail2},Patterns,{Bk,Op},Id_att)->
	
	{cons,LINE,{call,LINE,{remote,LINE,{atom,LINE,Bk},{atom,LINE,Op}},Patterns++[{integer,LINE,Id_att}]},Tail2};

newatt_body({cons,LINE,{call,_,Remote,Arg},Tail},Patterns,{Bk,Op},Id_att)->
	
	{cons,LINE,{call,LINE,Remote,Arg},newatt_body(Tail,Patterns,{Bk,Op},Id_att+1)};

newatt_body({nil,LINE},_,_,_)->
	
	{nil,LINE}.

	

insert2att(Rule,{Bk,Op})->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Body  of
		%[{cons,1,Head,{cons,1,{cons,1,{integer,1,2},{nil,1}},{nil,1}}}]
		  
		%[{cons,1,Head,{cons,1,{call,1,{remote,1,{atom,1,bk_raven},{atom,1,identity_att}},[{var,1,'L'}]},{nil,1}}}]
		
		[{cons,_,Head, Tail }]->
			
			NewBody=[{cons,LINE,Head, newatt_body(Tail,Patterns,{Bk,Op},1) }],
			
				%[{cons,LINE,Head,{cons,LINE,{call,LINE,{remote,LINE,{atom,LINE,Bk},{atom,LINE,Op}},Arg},{nil,1}}}],
			NewPatterns= Patterns;
		
		%[{cons,1,{call,1,{remote,1,{atom,1,bk},{atom,1,a}},[{var,1,'L'}]},   %%HEAD
				%{cons,1,{call,1,{remote,1,{atom,1,bk},{atom,1,b}},[{var,1,'L'}]},   %%TAIL 1 (HEAD)
						%{cons,1,{call,1,{remote,1,{atom,1,bk},{atom,1,c}},[{var,1,'L'}]},  %%TAIL 2 (HEAD)
                 			 %{nil,1}  %%TAIL3
						%}
				%}
		%}]
		
		%[{cons,_,Head,{cons,_,{call,_,Remote,Arg},Tail2}}]->
			
		%	Temp=newatt_body(Tail2),
		%	NewBody=[{cons,LINE,Head,{cons,LINE,{call,LINE,Remote,Arg},Tail2}}],
		%	NewPatterns= Patterns;
		
		
		
		
		%[{cons,_,Head,{cons,_,{cons,_,{atom,_,_},Tail1},Tail2}}]->
		%	NewBody=[{cons,LINE,Head,{cons,LINE,{call,LINE,{remote,LINE,{atom,LINE,Bk},{atom,LINE,Op}},Patterns},{nil,LINE}}}],
		%	NewPatterns= Patterns;

		%[{cons,_,Head,{cons,_,{cons,_,{integer,_,_},Tail1},Tail2}}]->
		%	NewBody=[{cons,LINE,Head,{cons,LINE,{call,LINE,{remote,LINE,{atom,LINE,Bk},{atom,LINE,Op}},Patterns},{nil,LINE}}}],
		%%	NewPatterns= Patterns;

		%[{cons,_,Head,{cons,_,{cons,_,{float,_,_},Tail1},Tail2}}]->
		%	NewBody=[{cons,LINE,Head,{cons,LINE,{call,LINE,{remote,LINE,{atom,LINE,Bk},{atom,LINE,Op}},Patterns},{nil,LINE}}}],
		%	NewPatterns= Patterns;
		
		
		
		_Else ->
		
			NewPatterns=Patterns,
			NewBody=Body
	end,

	

	New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE,Name,Arity,[{clause,LINE,NewPatterns,Guards,NewBody}]}])),
	New31=re:replace(New21, "\n", "",[global,{return,list}]),
	New41=re:replace(New31, "\r", "",[global,{return,list}]),
	New51=re:replace(New41, "\t", " ",[global,{return,list}]),
	[string:sub_word(New51,1,$.)].
