-module(operatorsSet).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                    %%%
%%%           0. Generalise Att in Patterns            %%%
%%%                                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

att2var(Regla,Arg)->
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	{NewPatterns,Nths}= lists:mapfoldl(fun (X,Nth) -> case Nth == Arg of true -> case element(1,X) == var of false ->[NewVar]=givemeFreshVar(),{NewVar,Nth+1}; true-> {X,Nth+1} end; false -> {X,Nth+1} end end, 1, Patterns),
	
	New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPatterns,Guards,Body}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	[string:sub_word(New2,1,$.)].


%%% VAR between [] %%%
givemeFreshVar()->
	{MegaSecs, Secs, Microsecs}=erlang:now(),
	%{Year, Month, Day}=erlang:date(),
	%{Hour, Min, Sec}=erlang:time(),	
	%{ok,NewVar,1}=erl_scan:string("VAR"++erlang:integer_to_list(Year)++erlang:integer_to_list(Month)++erlang:integer_to_list(Day)++erlang:integer_to_list(Hour)++erlang:integer_to_list(Min)++erlang:integer_to_list(Sec)),
	{ok,NewVar,1}=erl_scan:string("VAR"++erlang:integer_to_list(Microsecs)),
	%io:format("Var ~p~n",[NewVar]).
	NewVar.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                  %%%
%%%  1. Head of a List (in att number, Att) to RHS   %%%
%%%                                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

head2rhs(Regla,Arg)->
	
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case length(Patterns)>= Arg of
		true->
			List= lists:nth(Arg, Patterns),
			case head2rhs2(List) of
				'error' ->
					NewBody=Body;
				NewBody->
					NewBody
			end;
		false ->
			NewBody=Body
	end,	
	
	
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].



head2rhs2({cons,LINE,E1,E2})->
	[E1];
	
head2rhs2(Other)->
	error.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                  %%%
%%%  2. Tail of a List (in att number, Att) to RHS   %%%
%%%                                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tail2rhs(Regla,Arg)->
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case length(Patterns)>= Arg of
		true->
			List= lists:nth(Arg, Patterns),
			case tail2rhs2(List) of
				'error' ->
					NewBody=Body;
				NewBody->
					NewBody
			end;
		false ->
			NewBody=Body
	end,	
	

	
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].



tail2rhs2({cons,LINE,E1,E2})->
	[E2];
	
tail2rhs2(Other)->
	error.	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                    %%%
%%% 3.Head of a List (in att number, Att) To a Varible %%%
%%%                                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

head2varLists(Regla,Att)->
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	%[{cons,LINE,E1,E2}]=Patterns,
	%{ok,[NewVar],1}=erl_scan:string([65]),
	
	
	{NewPatterns,Nths}= lists:mapfoldl(fun (X,Nth) -> case Nth == Att of true -> {head2varList2(X),Nth+1}; false -> {X,Nth+1} end end, 1, Patterns),
	%NewPat= head2varList2(Patterns),
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPatterns,Guards,Body}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].


head2varList2({cons,LINE,E1,E2})->
	{ok,[NewVar],1}=erl_scan:string([65]),	
	{cons,LINE,NewVar,E2};

head2varList2({nil,1})->
	{nil,1};

head2varList2(Other)->
	Other.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                    %%%
%%% 4.Tail of a List (in att number, Att) to a Varible %%%
%%%                                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tail2varLists(Regla,Att)->
	%io:format("atom2var(~p,~p)~n",[Regla,Pos]),
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	%[{cons,LINE,E1,E2}]=Patterns,
	%{ok,[NewVar],1}=erl_scan:string([66]),
	{NewPatterns,Nths}= lists:mapfoldl(fun (X,Nth) -> case Nth == Att of true -> {tail2varList2(X),Nth+1}; false -> {X,Nth+1} end end, 1, Patterns),
	
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPatterns,Guards,Body}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].

tail2varList2({cons,LINE,E1,{nil,1}})->
	%{ok,[NewVar],1}=erl_scan:string([66]),	
	{cons,LINE,E1,{nil,1}};

tail2varList2({cons,LINE,E1,E2})->
	{ok,[NewVar],1}=erl_scan:string([66]),	
	{cons,LINE,E1,NewVar};

tail2varList2({nil,1})->
	{nil,1};

tail2varList2(Other)->
	Other.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                    %%%
%%%     5.Recursive call with HEAD of a List (arg)     %%%
%%%                                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recAddHead(Rule,Arg)->
	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,Line,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case length(Patterns) >= Arg of
		true ->
			case recBodyHead(lists:nth(Arg,Patterns)) of
				'error' -> 
					NewBody=Body;
				_else ->
					{NewArgs,Nths}= lists:mapfoldl(fun (X,Nth) -> case Nth == Arg of true -> {recBodyHead(X),Nth+1}; false -> {X,Nth+1} end end, 1, Patterns),
					NewBody= [{call,Line,{atom,Line,Name},NewArgs}]
			end;
		false ->
			NewBody=Body
	end,
			
	
	
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].
	

recBodyHead({cons,LINE,E1,E2})->
	E1;
	
recBodyHead(Other)->
	error.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                    %%%
%%%     6.Recursive call with TAIL of a List (arg)     %%%
%%%                                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recAddTail(Rule,Arg)->
	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,Line,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case length(Patterns) >= Arg of
		true ->
			case recBodyTail(lists:nth(Arg,Patterns)) of
				'error' -> 
					NewBody=Body;
				_else ->
					{NewArgs,Nths}= lists:mapfoldl(fun (X,Nth) -> case Nth == Arg of true -> {recBodyTail(X),Nth+1}; false -> {X,Nth+1} end end, 1, Patterns),
					NewBody= [{call,Line,{atom,Line,Name},NewArgs}]
			end;
		false ->
			NewBody=Body
	end,
	
	
	
	New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
	New3=re:replace(New2, " ", "",[global,{return,list}]),
	[string:sub_word(New3,1,$.)].
	


recBodyTail({cons,LINE,E1,E2})->
	E2;
	
recBodyTail(Other)->
	error.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                    %%%
%%%         7.  Give nth attribute of lhs              %%%
%%%				Returns [Rule, attribute] 			   %%%
%%%			8.  Substitute rhr by Att pased			   %%%
%%%			9.  Add Att to rhs (last)       		   %%%
%%%			10. Add Att to rhs (first)                 %%%
%%%                                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% operatorsSet:att2rhs_add_last("f(a,b,c)->v",operatorsSet:giveNatt_lhs("f(a,b,c)->v",3)). ----> ["f(a,b,c)->v,c"]

giveNatt_lhs(Rule,Nth)->
	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,Line,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	case (length(Patterns) >= Nth) of
		true ->
			lists:nth(Nth,Patterns);
		false ->
			error
	end.


att2rhs_del(Rule,Att)->
	
	case Att of 
		'error'->
			[Rule];
		_else ->
			SelectedRule=Rule++".",
			{ok,String,_}=erl_scan:string(SelectedRule),
			{ok,Forms}=erl_parse:parse_form(String),
			{function,Line,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
			New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,[Att]}]}])),
			New3=re:replace(New2, " ", "",[global,{return,list}]),
			[string:sub_word(New3,1,$.)]
	end.

att2rhs_add_last(Rule,Att)->
	case Att of 
		'error'->
			[Rule];
		_else ->
			SelectedRule=Rule++".",
			{ok,String,_}=erl_scan:string(SelectedRule),
			{ok,Forms}=erl_parse:parse_form(String),
			{function,Line,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
			New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body++[Att]}]}])),
			New3=re:replace(New2, " ", "",[global,{return,list}]),
			[string:sub_word(New3,1,$.)]
	end.

att2rhs_add_first(Rule,Att)->
	case Att of 
		'error'->
			[Rule];
		_else ->
			SelectedRule=Rule++".",
			{ok,String,_}=erl_scan:string(SelectedRule),
			{ok,Forms}=erl_parse:parse_form(String),
			{function,Line,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	

			New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,[Att]++Body}]}])),
			New3=re:replace(New2, " ", "",[global,{return,list}]),
			[string:sub_word(New3,1,$.)]
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                    %%%
%%%           11. rhs (list) ++ Att (list)  to rhs     %%%
%%%                                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



conc2rhs(Rule,Att) ->
	
	%io:format("Rule: ~p, Att: ~p~n",[Rule,Att]),
	case Att of 
		'error'->
			[Rule];
		_else ->
			SelectedRule=Rule++".",
			{ok,String,_}=erl_scan:string(SelectedRule),
			{ok,Forms}=erl_parse:parse_form(String),
			{function,Line,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
			[B]=Body,
	
	
			case length(Body)==1 of
				true ->
					case element(1,Att)== var of
						false ->
							Att_norm= erl_parse:normalise(Att),
							case element(1,B) == var of
								false ->
									case element(1,B) == cons of
										false->
											case element(1,B) == nil of
												true ->
													NewBody= [{op,Line,'++',B,Att}];
												false ->
													case  ((element(1,B) == op) and (element(3,B) == '++')) of 
														true ->
															case erlang:is_list(Att_norm) of 
																true ->
																	NewBody= [{op,Line,'++',B,Att}];
																false ->
																	NewBody= Body
															end;
														false ->
															NewBody= Body
													end
											end;
										true ->
											case erlang:is_list(Att_norm) of 
														true ->
															NewBody= [{op,Line,'++',B,Att}];
														false ->
															NewBody= Body
											end	
									end;
								true ->
									NewBody= [{op,Line,'++',B,Att}]
							end;
						true ->
							NewBody= [{op,Line,'++',B,Att}]
					end;
				false ->
					NewBody= Body
			end,

					
			New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,Patterns,Guards,NewBody}]}])),
			New3=re:replace(New2, " ", "",[global,{return,list}]),
			[string:sub_word(New3,1,$.)]
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                    %%%
%%% 12. nth_lists of elements of a list to a variable  %%%
%%%                                                    %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




nthlist2var(Rule,Att,Nth,ReturnVar)->

			
			
		
			SelectedRule=Rule++".",
			{ok,String,_}=erl_scan:string(SelectedRule),
			{ok,Forms}=erl_parse:parse_form(String),
			{function,Line,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
			
			case Att > length(Patterns) of
				true ->
					[Rule];
				false ->
					
					List= lists:nth(Att, Patterns),
			
					{NewList,Vars}=lookforNthList(List,Nth),
					%io:format("New List: ~p~n",[NewList]),
					{NewPatterns,_} = lists:mapfoldl(fun(X,NumAtt)-> case (NumAtt==Att) of 
																 true -> 
																	 {NewList,NumAtt+1}; 
																 false -> 
																	 {X,NumAtt+1}
															 end 
											 end ,1, Patterns),
					%io:format("New Patterns: ~p~n",[NewPatterns]),
					New2=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,NewPatterns,Guards,Body}]}])),
					New3=re:replace(New2, " ", "",[global,{return,list}]),
					
					case ReturnVar of
						false ->		
							[string:sub_word(New3,1,$.)];
						true ->
							{[string:sub_word(New3,1,$.)],Vars}
					end
				

			end.

			
			
			
			
lookforNthList(List,Nth)->
						
			lookforNthList2(List,Nth,1).


lookforNthList2({nil,LINE},_,Index)->
	{{nil,LINE},[]};


lookforNthList2({cons,LINE,E1,E2},Nth,Index)->
	case Nth == Index of
		true ->
			[NewVar]=givemeFreshVar(),
			{{cons,LINE,NewVar,E2},[NewVar]};
		false ->
			{NewCons,Vars}=lookforNthList2(E2,Nth,Index+1),
			{{cons,LINE,E1,NewCons},Vars}
	end;


lookforNthList2(Other,_,_)->
	{Other,[]}.	
			
			


look4VarList(List)->
	look4VarList2(List).
			
look4VarList2({cons,Line,{var,_,_},E2})->
	1 + look4VarList2(E2); 

look4VarList2({cons,Line,E1,E2})->
	0;

look4VarList2(Other)->
	0.
			
			
			
			
			
			
			
			
			

	

