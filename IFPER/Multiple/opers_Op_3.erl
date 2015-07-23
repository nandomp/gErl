-module(opers_Op_3).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1,oper_5/1,oper_6/1,oper_7/1,oper_8/1,oper_9/1,oper_10/1,oper_11/1,oper_12/1,oper_13/1,oper_14/1,oper_15/1]).
%-compile(export_all).


oper_1(Rule)->
	["sust([x])->[b]"].

oper_2(Rule)->
	["sust([x])->[b]"].

oper_3(Rule)->
	sustH(Rule,1,1).

oper_4(Rule)->
	["sust([x])->[b]"].

oper_5(Rule)->
	genTwoAttsifMap_LHS(Rule).	

oper_6(Rule)->
	getPrefix(Rule,1,1).

oper_7(Rule)->
	["sust([x])->[b]"].

oper_8(Rule)->
	sustNT(Rule,1,1).

oper_9(Rule)->
	genTwoAttsifMap_RHS(Rule).

oper_10(Rule)->
	getSuffix(Rule,1,1).

oper_11(Rule)->
	["sust([x])->[b]"].
oper_12(Rule)->
	["sust([x])->[b]"].
oper_13(Rule)->
	["sust([x])->[b]"].
oper_14(Rule)->
	["sust([x])->[b]"].
oper_15(Rule)->
	["sust([x])->[b]"].


giveme_last(Rule,NthLHS)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,Body}]}=Forms,


	ListLHS= lists:nth(NthLHS, Patterns),
	%ListRHS= lists:nth(NthRHS, Body),
	
	case Body of
		[{call,1,R,[F,L]}]->
			[Rule];
		[{match,1,L2,C2},F2]->
			[Rule];	
		[{op,1,'++',A,B}]->
			[Rule];
		[{call,1,R,L}]->
			[Rule];
		 [{cons,1,H,T}]->
			 [Rule];
		
		
		_else ->
			case ListLHS of
				{var,_,_} ->
					[Rule];
				_Else ->

					NewBody= [{call,1,{remote,1,{atom,1,lists},{atom,1,last}},[ListLHS]}],
							
						
					New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,NewBody}]}])),
					New31=re:replace(New21, "\n", "",[global,{return,list}]),
					New41=re:replace(New31, "\r", "",[global,{return,list}]),
					New51=re:replace(New41, "\t", " ",[global,{return,list}]),
					[string:sub_word(New51,1,$.)]


					
			end
	end.

	


genTwoAttsifMap_RHS(Rule)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,Body}]}=Forms,
	%io:format("Pat: ~p~n",[Patterns]),
	case lists:nth(1, Patterns) of 
		
		{var,_,_} -> 

			%[NewVar]=operatorsSet:givemeFreshVar(),
			{ok,[NewVar],1}=erl_scan:string("X"),
			case Body of
				[{call,1,Remote,[{call,1,Remote2,[Fun,List]}]}]->
						HowMany=1,NewBody2=false,
						NewBody=[{call,1,Remote,[{call,1,Remote2,[Fun,NewVar]}]}];
				[{call,LINE,Remote,[Fun,List]}]-> 
						HowMany=1,NewBody2=false,
						NewBody=[{call,LINE,Remote,[Fun,NewVar]}];
						%NewPatterns=[NewVar];
				[{match,LINE,{var,LINE,'NewList'},{call,LINE,Remote,[Fun,List]}},Flatten]->
						HowMany=1,	NewBody2=false,
						NewBody=[{match,LINE,{var,LINE,'NewList'},{call,LINE,Remote,[Fun,NewVar]}},Flatten];
						%NewPatterns=[NewVar];
				[{op,1,'++',A,B}]->
						NewBody=[{op,1,'++',NewVar,B}],
						NewBody2=[{op,1,'++',A,NewVar}],
						HowMany=2;
						
				[{call,LINE,Remote,List}]-> 
						HowMany=1,NewBody2=false,
						NewBody=[{call,LINE,Remote,[NewVar]}];
						%NewPatterns=[NewVar];
						
				Other ->
						HowMany=1,NewBody2=false,
						NewBody=Body
						%NewPatterns=Patterns
			end,
			case HowMany of
				1->
					New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,NewBody}]}])),
					New31=re:replace(New21, "\n", "",[global,{return,list}]),
					New41=re:replace(New31, "\r", "",[global,{return,list}]),
					New51=re:replace(New41, "\t", " ",[global,{return,list}]),
					[string:sub_word(New51,1,$.)];
				2->
					New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,NewBody}]}])),
					New31=re:replace(New21, "\n", "",[global,{return,list}]),
					New41=re:replace(New31, "\r", "",[global,{return,list}]),
					New51=re:replace(New41, "\t", " ",[global,{return,list}]),
					
					
					New22=erl_prettypr:format(erl_syntax:form_list([{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,NewBody2}]}])),
					New32=re:replace(New22, "\n", "",[global,{return,list}]),
					New42=re:replace(New32, "\r", "",[global,{return,list}]),
					New52=re:replace(New42, "\t", " ",[global,{return,list}]),
					[string:sub_word(New51,1,$.),string:sub_word(New52,1,$.)]
			end;
					
		_else ->
			[Rule]
	end.
		
		

genTwoAttsifMap_LHS(Rule)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,Body}]}=Forms,
	
	case lists:nth(1, Patterns) of 
		{var,_,_} -> 
			[Rule];
		_NORULE ->
			%[NewVar]=operatorsSet:givemeFreshVar(),
			{ok,[NewVar],1}=erl_scan:string("X"),
			case Body of
		
				[{call,LINE,Remote,[Fun,List]}]-> 
						%NewBody=[{call,LINE,Remote,[Fun,NewVar]}],
						NewPatterns=[NewVar];
				[{match,LINE,{var,LINE,'NewList'},{call,LINE,Remote,[Fun,List]}},Flatten]->
						%NewBody=[{match,LINE,{var,LINE,'NewList'},{call,LINE,Remote,[Fun,NewVar]}},Flatten],
						NewPatterns=[NewVar];
				[{op,1,'++',A,B}]->
						case A == lists:nth(1,Patterns)of 
							true -> %%suffix
								%NewBody=[{op,1,'++',NewVar,B}],
								NewPatterns=[NewVar];
							false -> %%prefix
								%NewBody=[{op,1,'++',A,NewVar}],
								NewPatterns=[NewVar]
						end;
				[{call,LINE,Remote,List}]-> 
						%NewBody=[{call,LINE,Remote,[NewVar]}],
						NewPatterns=[NewVar];
						
				Other ->
						%NewBody=Body,
						NewPatterns=Patterns
			end,
	
			New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE1,Name,Arity,[{clause,LINE2,NewPatterns,Guards,Body}]}])),
			New31=re:replace(New21, "\n", "",[global,{return,list}]),
			New41=re:replace(New31, "\r", "",[global,{return,list}]),
			New51=re:replace(New41, "\t", " ",[global,{return,list}]),
			[string:sub_word(New51,1,$.)]
	end.
		
genTwoAttsifMap(Rule)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,Body}]}=Forms,
	
	case lists:nth(1, Patterns) of 
		{var,_,_} -> 
			[Rule];
		_NORULE ->
			[NewVar]=operatorsSet:givemeFreshVar(),
			%{ok,[NewVar],1}=erl_scan:string("X"),
			case Body of
		
				[{call,LINE,Remote,[Fun,List]}]-> 
						NewBody=[{call,LINE,Remote,[Fun,NewVar]}],
						NewPatterns=[NewVar];
				[{match,LINE,{var,LINE,'NewList'},{call,LINE,Remote,[Fun,List]}},Flatten]->
						NewBody=[{match,LINE,{var,LINE,'NewList'},{call,LINE,Remote,[Fun,NewVar]}},Flatten],
						NewPatterns=[NewVar];
				[{op,1,'++',A,B}]->
						case A == lists:nth(1,Patterns)of 
							true -> %%suffix
								NewBody=[{op,1,'++',NewVar,B}],
								NewPatterns=[NewVar];
							false -> %%prefix
								NewBody=[{op,1,'++',A,NewVar}],
								NewPatterns=[NewVar]
						end;
				[{call,LINE,Remote,List}]-> 
						NewBody=[{call,LINE,Remote,[NewVar]}],
						NewPatterns=[NewVar];
						
				Other ->
						NewBody=Body,
						NewPatterns=Patterns
			end,
	
			New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE1,Name,Arity,[{clause,LINE2,NewPatterns,Guards,NewBody}]}])),
			New31=re:replace(New21, "\n", "",[global,{return,list}]),
			New41=re:replace(New31, "\r", "",[global,{return,list}]),
			New51=re:replace(New41, "\t", " ",[global,{return,list}]),
			[string:sub_word(New51,1,$.)]
	end.		


sustH(Rule,NthLHS,NthRHS)->	
	

	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,Body}]}=Forms,
	
	ListLHS= lists:nth(NthLHS, Patterns),
	ListRHS= lists:nth(NthRHS, Body),
	
	case Body of
		[{call,1,R,[F,L]}]->
			[Rule];
		[{match,1,L2,C2},F2]->
			[Rule];	
		[{op,1,'++',A,B}]->
			[Rule];
		[{call,LINE,Remote,List}]->
			[Rule];
		[{atom,_,_}]->
			[Rule];
		_else ->
			case ListLHS of
				{var,_,_} ->
					[Rule];
				_Else ->

					LH_norm= erl_parse:normalise(ListLHS),
					LR_norm= erl_parse:normalise(ListRHS),
					
					case (length(LH_norm) == length(LR_norm)) of 
						true ->
							IndexDif= indexDif(LH_norm,LR_norm),
	
							case IndexDif of 
								-1 ->
									[Rule];
								_Else2 ->
				
	
									[{call,1,Remote,[Fun,List]}]= giveMeSustFuncH(IndexDif, LH_norm, LR_norm),
									FunH=[{call,1,Remote,[Fun,ListLHS]}],
			
									%FunT= giveMeSustFuncT(IndexDif, LH_norm, LR_norm),

									New21=erl_prettypr:format(erl_syntax:form_list([{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,FunH}]}])),
									New31=re:replace(New21, "\n", "",[global,{return,list}]),
									New41=re:replace(New31, "\r", "",[global,{return,list}]),
									New51=re:replace(New41, "\t", " ",[global,{return,list}]),
									[string:sub_word(New51,1,$.)]
								
							end;
						false ->
							[Rule]
					end
			end
	end.



sustNT(Rule,NthLHS,NthRHS)->	
	

	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,Body}]}=Forms,
	
	ListLHS= lists:nth(NthLHS, Patterns),
	ListRHS= lists:nth(NthRHS, Body),
	
	case Body of
		[{call,1,R,[F,L]}]->
				[Rule];
		[{match,1,L2,C2},F2]->
			[Rule];	
		[{op,1,'++',A,B}]->
			[Rule];
		[{call,LINE,Remote,List}]->
			[Rule];
		[{atom,_,_}]->
			[Rule];
		_else ->
	
			case ListLHS of
				{var,_,_} ->
					[Rule];
				_Else ->
					LH_norm= erl_parse:normalise(ListLHS),
					LR_norm= erl_parse:normalise(ListRHS),
	
					case (length(LH_norm) == length(LR_norm)) of 
						
						false ->
							IndexDif= indexDif(LH_norm,LR_norm),
					
							%io:format("Index: ~p~n",[IndexDif]),

							case (IndexDif== -1) or (length(LH_norm)==1) or (length(LR_norm)==1) or 
							 (lists:suffix(LH_norm, LR_norm)) or (lists:prefix(LH_norm, LR_norm))  of 
								true ->
									[Rule];
						
								false ->
			
	
									sustT2(Rule,IndexDif,LH_norm,LR_norm,ListLHS,ListRHS,Forms)
							end;
						
						true ->
							[Rule]					
					end

			end
	end.


sustT2(RuleOrig,IndexDif,LH_norm,LR_norm,ListLHS,ListRHS,Forms)->
		
		Cuts=length(LR_norm)-IndexDif+1,
		%io:format("Index: ~p, Cuts: ~p ~n",[IndexDif,Cuts]),
	    case Cuts==1 of
			true ->
				[RuleOrig];
			false ->
				sustT3(RuleOrig,IndexDif,LH_norm,LR_norm,ListLHS,ListRHS,Forms,Cuts,[])
		end.
		  
		  	

sustT3(RuleOrig,_,_,_,_,_,_,1,Rules)->
	Rules;

sustT3(RuleOrig,IndexDif,LH_norm,LR_norm,ListLHS,ListRHS,Forms,Cuts,Rules)->
	
			
			{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,Body}]}=Forms,
			%[{match,1,{var,1,'NewList'},{call,1,Remote,[Fun,List]}},Flatten]= giveMeSustFuncNT(IndexDif, LH_norm, LR_norm,Cuts),
			
			[{call,1,Remote,[{call,1,Remote2,[Fun,List]}]}]= giveMeSustFuncNT(IndexDif, LH_norm, LR_norm,Cuts),	
			
			FunT=[{call,1,Remote,[{call,1,Remote2,[Fun,ListLHS]}]}],
			%FunT=[{match,1,{var,1,'NewList'},{call,1,Remote,[Fun,ListLHS]}},Flatten],	


			New2=erl_prettypr:format(erl_syntax:form_list([{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,FunT}]}])),
			New3=re:replace(New2, "\n", "",[global,{return,list}]),
						
			New4=re:replace(New3, "\r", "",[global,{return,list}]),
			New5=re:replace(New4, "\t", " ",[global,{return,list}]),
			NewRules=Rules ++ [string:sub_word(New5,1,$.)],
			sustT3(RuleOrig,IndexDif,LH_norm,LR_norm,ListLHS,ListRHS,Forms,Cuts-1,NewRules).



	

indexDif(A,B) when (erlang:is_list(A) and erlang:is_list(B))->

	indexDif2(A,B,1).


indexDif2([],[],_)->
	-1;

indexDif2([],TB,_)->
	-1;

indexDif2(TA,[],_)->
	-1;

indexDif2([HA|TA],[HB|TB],Index)->
	
	case HA == HB of
		true ->
			indexDif2(TA,TB,Index+1);
		false ->
			Index
	end.


giveMeSustFuncH(Index, A, B)->
	Elem1=lists:nth(Index, A),
	Elem2=lists:nth(Index, B),	
	
	%fun(X) -> case X of Elem1 -> Elem2; _else ->X end end.
	Return= [{call,1,{remote,1,{atom,1,lists},{atom,1,map}},
			  [{'fun',1,{clauses,[{clause,1,[{var,1,'X'}],[],
								   [{'case',1,{op,1,'==',{var,1,'X'},{atom,1,erlang:list_to_atom(io_lib:print(Elem1))}},
									 [{clause,1,[{atom,1,true}],[],[{atom,1,erlang:list_to_atom(io_lib:print(Elem2))}]},
									  {clause,1,[{atom,1,false}],[],[{var,1,'X'}]}]}]}]}},{var,1,'L'}]}].
	

	
	


giveMeSustFuncT(Index, A, B)->
	Elem1=lists:nth(Index, A),
	Elem2=lists:nthtail(Index-1, B),%%%cola desde index	
	%fun(X) -> case X of Elem1 -> Elem2; _else ->X end end.
	[{match,1,
     {var,1,'NewList'},
     {call,1,
         {remote,1,{atom,1,lists},{atom,1,map}},
         [{'fun',1,
              {clauses,
                  [{clause,1,
                       [{var,1,'X'}],
                       [],
                       [{'case',1,
                            {op,1,'==',{var,1,'X'},{atom,1,erlang:list_to_atom(io_lib:print(Elem1))}},
                            [{clause,1,
                                 [{atom,1,true}],
                                 [],
                                 [erl_parse:abstract(Elem2)]},
                             {clause,1,
                                 [{atom,1,false}],
                                 [],
                                 [{var,1,'X'}]}]}]}]}},
          {var,1,'L'}]}},
 	{call,1,{remote,1,{atom,1,lists},{atom,1,flatten}},[{var,1,'NewList'}]}].





giveMeSustFuncNT(Index, A, B,Until)->
	Elem1=lists:nth(Index, A),
	Elem2=lists:nthtail(Index-1, B),%%%cola desde index	
	{Elem2B,_}=lists:split(Until, Elem2),
	
	%io:format("ElemB2: ~p~n",[Elem2B]),
	%fun(X) -> case X of Elem1 -> Elem2; _else ->X end end.
	[{call,1,{remote,1,{atom,1,lists},{atom,1,flatten}},
	  [{call,1,
         {remote,1,{atom,1,lists},{atom,1,map}},
         [{'fun',1,
              {clauses,
                  [{clause,1,
                       [{var,1,'X'}],
                       [],
                       [{'case',1,
                            {op,1,'==',{var,1,'X'},{atom,1,erlang:list_to_atom(io_lib:print(Elem1))}},
                            [{clause,1,
                                 [{atom,1,true}],
                                 [],
                                 [erl_parse:abstract(Elem2B)]},
                             {clause,1,
                                 [{atom,1,false}],
                                 [],
                                 [{var,1,'X'}]}]}]}]}},
          {var,1,'L'}]}]}].	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%PREFIX


getPrefix(Rule,NthLHS,NthRHS)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,Body}]}=Forms,
	
	[NewVar]=operatorsSet:givemeFreshVar(),
	
	ListLHS= lists:nth(NthLHS, Patterns),
	ListRHS= lists:nth(NthRHS, Body),
	
	case Body of
		[{call,_,R,[F,L]}]->%H
			[Rule];
		[{match,_,L2,C2},F2]->%NT
			[Rule];	
		[{op,_,_,_,_}]->
			[Rule];
		[{call,LINE,Remote,List}]->
			[Rule];
		[{atom,_,_}]->
			[Rule];
		_else ->
	
			case ListLHS of
				{var,_,_} ->
					[Rule];
				_Else ->
					LH_norm= erl_parse:normalise(ListLHS),
					LR_norm= erl_parse:normalise(ListRHS),
	
					case lists:suffix(LH_norm, LR_norm) of
						true->
							{PREFIX,_}=lists:split(length(LR_norm)-length(LH_norm), LR_norm),
							
							NewBody=[{op,1,'++',erl_parse:abstract(PREFIX),ListLHS}],
							
							New2=erl_prettypr:format(erl_syntax:form_list([{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,NewBody}]}])),
							New3=re:replace(New2, "\n", "",[global,{return,list}]),
							New4=re:replace(New3, "\r", "",[global,{return,list}]),
							New5=re:replace(New4, "\t", " ",[global,{return,list}]),
							[string:sub_word(New5,1,$.)];
						
						false->
							[Rule]
					end
			end
	end.

getSuffix(Rule,NthLHS,NthRHS)->
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,Body}]}=Forms,
	
	[NewVar]=operatorsSet:givemeFreshVar(),
	
	ListLHS= lists:nth(NthLHS, Patterns),
	ListRHS= lists:nth(NthRHS, Body),
	
	case Body of
		[{call,_,R,[F,L]}]->%H
			[Rule];
		[{match,_,L2,C2},F2]->%NT
			[Rule];	
		[{op,_,_,_,_}]->
			[Rule];
		[{call,LINE,Remote,List}]->
			[Rule];
		[{atom,_,_}]->
			[Rule];
		_else ->
	
			case ListLHS of
				{var,_,_} ->
					[Rule];
				_Else ->
					LH_norm= erl_parse:normalise(ListLHS),
					LR_norm= erl_parse:normalise(ListRHS),
	
					case lists:prefix(LH_norm, LR_norm) of
						true->
							{_,SUFFIX}=lists:split(length(LH_norm), LR_norm),
							
							NewBody=[{op,1,'++',ListLHS,erl_parse:abstract(SUFFIX)}],
							
							New2=erl_prettypr:format(erl_syntax:form_list([{function,LINE1,Name,Arity,[{clause,LINE2,Patterns,Guards,NewBody}]}])),
							New3=re:replace(New2, "\n", "",[global,{return,list}]),
							New4=re:replace(New3, "\r", "",[global,{return,list}]),
							New5=re:replace(New4, "\t", " ",[global,{return,list}]),
							[string:sub_word(New5,1,$.)];
						
						false->
							[Rule]
					end
			end
	end.