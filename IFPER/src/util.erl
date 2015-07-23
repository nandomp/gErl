-module(util).
-compile(export_all).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%        LECTURA DE FICHERO DE TEXTO          %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

leer_fichero(File) ->	
	case file:open(File, read) of
		{ok,Fd} ->
			leer_texto_linea(Fd),
			file:close(Fd),
			{ok};
		{error,Motivo} ->
			{error,Motivo}
	end.

leer_texto_linea(Fd)->
	case io:get_line(Fd,'') of
		eof ->
			io:format("Fin de fichero ~n");
		{error, Motivo} ->
			{error, Motivo};
		Texto ->
			io:format("Texto leido: ~p~n", [Texto]),
			leer_texto_linea(Fd)
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%% MAX REWARDED OPERATOR (OF SET OF OPERATORS) %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	

maxReward(_,_,MaxKey,1,Rew,Veces)->
	{MaxKey,Rew,Veces};
	
maxReward(Tabla,MaxReward,MaxKey,Key,_,Veces)->

	Prev = ets:prev( Tabla, Key ),
	%io:format( "prev: ~w ~n ", [ Prev ] ),
	%io:format("Match: ~p~n",[ets:match( Tabla,  {Prev,'$1'})]),
	Match=ets:match( Tabla,  {Prev,'$1'}),
	{_,NewRew,VecesNew}=lists:nth(1,lists:nth(1,Match)),
	%{Act,NewRew}=ets:match( Tabla,  {Prev,'$1'}),
	%io:format("NewRew: ~p~n",[NewRew]),
	%io:format("NewKey: ~p~n",[Prev]),
	case NewRew>=MaxReward of
			true -> 
				maxReward(Tabla,NewRew,Prev,Key-1,NewRew,VecesNew);
			false ->
				maxReward(Tabla,MaxReward,MaxKey,Key-1,MaxReward,Veces)
	end.
				   


selectRule(Table,Last,Loop)->
	Count=ets:last(Table),
	
	case Last>=0 of
		true ->
			case Last < Count of
				true ->
					{Loop,ets:lookup(Table, Last+1)};
				false ->
					{Loop+1,ets:lookup(Table, 1)}
			end;
		false ->
			false
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%    COBERTURE FUNCTION (FUNCTIONAL WAY)      %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		

cobertura_Funcional(InstanciasP, InstanciasN,InstanciasPRec,InstanciasNRec,InstancesPosBase,InstancesNegBase, Newrule)->
	cobertura_Funcional(InstanciasP, InstanciasN,InstanciasPRec,InstanciasNRec,InstancesPosBase,InstancesNegBase, Newrule, false).

cobertura_Funcional(InstanciasP, InstanciasN,InstanciasPRec,InstanciasNRec,InstancesPosBase,InstancesNegBase, Newrule, PositiveConsistency)->
	
		%io:format("Rulecobertura: ~p~n",[Newrule]),
	Newrule1=lists:append(Newrule,"."),
	%io:format("Rule: ~p~n",[Newrule1]),
	%io:format(" NEG \n",[]),
	{CobNeg,RulesNegCov}=cobertura_Funcional2(Newrule1,InstanciasN,InstanciasNRec,InstancesNegBase,0,[],ets:last(InstanciasN),PositiveConsistency),
	%io:format("----------> Neg= ~w ~n",[RulesNegCov]),
	%io:format(" POS \n",[]),
	{CobPos,RulesPosCov}=cobertura_Funcional2(Newrule1,InstanciasP,InstanciasPRec,InstancesPosBase,0,[],ets:last(InstanciasP),PositiveConsistency),
	%io:format("----------> pos= ~w ~n",[RulesPosCov]),
	{CobPos,RulesPosCov,CobNeg,RulesNegCov}.


cobertura_Funcional(VarGlob, Newrule)->
	cobertura_Funcional(VarGlob, Newrule,false).

cobertura_Funcional(VarGlob, Newrule, PositiveConsistency)->
	
	InstanciasP=ets:lookup_element(VarGlob, instanciasP, 2),
	InstanciasN=ets:lookup_element(VarGlob, instanciasN, 2),
	InstanciasPRec=ets:lookup_element(VarGlob, instanciasPRec, 2),
	InstanciasNRec=ets:lookup_element(VarGlob, instanciasNRec, 2),
	InstancesPosBase = ets:lookup_element(VarGlob, recInstancesPosBase, 2),
	InstancesNegBase = ets:lookup_element(VarGlob, recInstancesNegBase, 2),
	
	%io:format("Rulecobertura: ~p~n",[Newrule]),
	Newrule1=lists:append(Newrule,"."),
	%io:format("Rule: ~p~n",[Newrule1]),
	%io:format(" NEG \n",[]),
	{CobNeg,RulesNegCov}=cobertura_Funcional2(Newrule1,InstanciasN,InstanciasNRec,InstancesNegBase,0,[],ets:last(InstanciasN),PositiveConsistency),
	%io:format("----------> Neg= ~w ~n",[CobNeg]),
	%io:format(" POS \n",[]),
	{CobPos,RulesPosCov}=cobertura_Funcional2(Newrule1,InstanciasP,InstanciasPRec,InstancesPosBase,0,[],ets:last(InstanciasP),PositiveConsistency),
	
	{CobPos,RulesPosCov,CobNeg,RulesNegCov}.
	
	
cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index)->
	cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index, false).

cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index, PositiveCons)->
	
	%io:format("Entro ~n)",[]),
	
	Test=smerl:new(testV),
	{ok, Test2} = smerl:add_func(Test, lists:flatten(Newrule)),
	case smerl:compile(Test2) of 
		{error,_,_} ->
			{0,[]};
		_ ->
			
			P1 = smerl:new(rule),
			RulesTrans=recRulesTransformation(Newrule),
	
	
			%io:format("Rules transformed: ~p~n",[RulesTrans]),
			case length(RulesTrans) of 
		
				1 ->
					%No es recursiva
					[NewRule1] = RulesTrans,
					InstanciasUso=Instancias,
					{ok, P2} = smerl:add_func(P1, lists:flatten(NewRule1));
		
				2 ->
					%Sí es recursiva
			
					%[Base,NewRule1]= RulesTrans,
					InstanciasUso=InstanciasRec,
					%NewRule2= lists:append(Base,NewRule1),
					NewRule2=getBaseRecProgram(RulesTrans,InstanciasBase,Index),
					%io:format("RecBase: ~p ~n",[NewRule2]),
					{ok, P2} = smerl:add_func(P1, lists:flatten(NewRule2))
		
			end,  
																		  
			case Index of
		
				0 ->
					{Cob,Rules_Cov};
			
				_else ->
			
					[{Index,{Regla,_,_}}]=ets:lookup(InstanciasUso, Index),
					Regla1=string:sub_word(Regla,1,$-),
					Rhs=string:sub_word(Regla,2,$>),			
					Code=lists:append("try ",Regla1),
					Code1=lists:append(Code, " of Val -> case Val=="),
					Code2=lists:append(Code1,Rhs),
					Code3=lists:append(Code2," of true -> true; false -> case Val==inf of true -> falseInf; false ->falseOther end end catch _:_ -> falseMatch end."),			
					CodeFin=lists:append("cubre()->",Code3),
					%io:format("Code: ~p~n",[CodeFin]),
					
					{ok, P3} = smerl:add_func(P2,CodeFin),
					smerl:compile(P3),
					Sol=rule:cubre(),
					%io:format("SOLUCION= ~w~n",[Sol]),
					case Sol of
						falseInf ->
							%io:format("Entro en FALSEINF -> ~p ~n",[Cob]),
							%cobertura_Funcional3(P2,Newrule,InstanciasUso,Cob,Rules_Cov,Index-1);(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index)
							cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index-1,PositiveCons);
						falseOther -> %Positive Consistency
							%io:format("Entro en FALSEOTHER -> ~p ~n",[Cob]),
							%cobertura_Funcional3(P2,Newrule,InstanciasUso,Cob,Rules_Cov,Index-1);(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index)
							case PositiveCons of
								true ->
									cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,[-1|Rules_Cov],Index-1,PositiveCons);
								false ->
									cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index-1,PositiveCons)
							end;
					
						falseMatch ->
							%io:format("Entro en FALSEMATCH -> ~p ~n",[Cob]),
							%cobertura_Funcional3(P2,Newrule,InstanciasUso,Cob,Rules_Cov,Index-1);(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index)
							cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index-1,PositiveCons);

						Else ->
							%io:format("Entro en ELSE:~p -> ~p ~n",[Else,Cob]),
							%cobertura_Funcional3(P2,Newrule,InstanciasUso,Cob+1,[Index|Rules_Cov],Index-1)
							cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob+1,[Index|Rules_Cov],Index-1,PositiveCons)

					end
		
			end
	end.
			

%%% Obtencion programa recursivo COBERTURA EXTENSIONAL %%%

getBaseRecProgram(RulesTrans,InstanciasBase,Eindex)->
	[Base,NewRule1]= RulesTrans,
	ProgramRecBase=lists:append(Base,NewRule1),
	getBaseRecProgram2(ProgramRecBase,InstanciasBase,Eindex,ets:last(InstanciasBase)).


getBaseRecProgram2(ProgramRecBase,_,_,0)->
	ProgramRecBase;

getBaseRecProgram2(ProgramRecBase,InstanciasBase,Eindex,Index)->	
	
	case Index =/= Eindex of
		true ->
			[{Index,{InstanciaBase,_,_}}]=ets:lookup(InstanciasBase, Index),
			NewInstance= lists:append(InstanciaBase, ";"),
			NewProgramRecBase= lists:append(NewInstance,ProgramRecBase),
			getBaseRecProgram2(NewProgramRecBase,InstanciasBase,Eindex,Index-1);
		false ->
			getBaseRecProgram2(ProgramRecBase,InstanciasBase,Eindex,Index-1)
	end.
			
	
%%% Generación reglas nueva Tail Recursion %%%

recRulesTransformation(Rule)->
	
	%SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(lists:flatten(Rule)),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	%% Comprobacion regla recursiva %%
	NewBody = addTailRecursionArgs(Body,Name),
	case NewBody == Body of
		true -> 
			%no recursividad
			[Rule];
		
		false ->
			
			New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity+1,[{clause,1,addTailRecursionPatt(Patterns),Guards,NewBody}]}])),
			New2=re:replace(New, " ", "",[global,{return,list}]),
			New3=re:replace(New2, "\n", "",[global,{return,list}]),
			Base=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity+1,[{clause,1,getBase(Arity),Guards, [{atom,1,inf}]}]}])),
			Base2=re:replace(Base, " ", "",[global,{return,list}]),
			Base3=re:replace(Base2, "\n", "",[global,{return,list}]),
			Base4=string:sub_word(Base3,1,$.),
			Base5=lists:append(Base4,";"),
	
			[Base5,New3]
	end.
		

%%% Transformacion instancias Tail Recursion %%%

recInstancesTransform(Instances,RecInstances)->
	
	recInstancesTransform2(Instances,RecInstances,ets:last(Instances)).


recInstancesTransform2(_,RecInstances,0)->
	RecInstances;
	
recInstancesTransform2(Instances,RecInstances,Index)->
	
	[{Index,{Instance,A,B}}]=ets:lookup(Instances, Index),
	SelectedInstance=Instance++".",
	{ok,String,_}=erl_scan:string(SelectedInstance),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,Line,Name,Arity,[{clause,Line,Patterns,Guards,Body}]}=Forms,
	
	New=erl_prettypr:format(erl_syntax:form_list([{function,Line,Name,Arity+1,[{clause,Line,Patterns++[{integer,Line,20}],Guards,Body}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	New3=re:replace(New2, "\n", "",[global,{return,list}]),
	New4=string:sub_word(New3,1,$.),
	ets:insert(RecInstances, {Index,{New4,A,B}}),
	
	

	recInstancesTransform2(Instances,RecInstances,Index-1).


	
%%% Transformacion instancias Caso Base %%%

recInstancesBaseCases(Instances,RecInstances)->
	
	recInstancesBaseCases2(Instances,RecInstances,ets:last(Instances)).


recInstancesBaseCases2(_,RecInstances,0)->
	RecInstances;
	
recInstancesBaseCases2(Instances,RecInstances,Index)->
	
	[{Index,{Instance,A,B}}]=ets:lookup(Instances, Index),
	SelectedInstance=Instance++".",
	{ok,String,_}=erl_scan:string(SelectedInstance),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,Line,Name,Arity,[{clause,Line,Patterns,Guards,Body}]}=Forms,
	
	New=erl_prettypr:format(erl_syntax:form_list([{function,Line,Name,Arity+1,[{clause,Line,Patterns++[{var,Line,'_'}],Guards,Body}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	New3=re:replace(New2, "\n", "",[global,{return,list}]),
	New4=string:sub_word(New3,1,$.),
	ets:insert(RecInstances, {Index,{New4,A,B}}),
	
	

	recInstancesBaseCases2(Instances,RecInstances,Index-1).	


%******************AddTailRecursionPatt******************%


getBase(Arity)->
	getBase2(Arity,[],Arity).

getBase2(_,Args,0)->
	Args++[{integer,1,0}];
	
getBase2(Arity,Args,Index)->
  	getBase2(Arity,Args++[{var,1,'_'}],Index-1).

	
	

%******************AddTailRecursionPatt******************%

addTailRecursionPatt(Patterns)->
	Patterns ++ [{var,1,'Acc'}].


%******************AddTailRecursionArgs******************%


addTailRecursionArgs([Arg|Args],Name)->
		
		%io:format("Arg-> ~p~n",[Arg]),
		[addTailRecursionArg(Arg,Name)|addTailRecursionArgs(Args,Name)];

addTailRecursionArgs(Other,Name)->
		
		addTailRecursionArg(Other,Name).


%******************AddTailRecursionArg******************%

addTailRecursionArg({match,LINE,E1,E2},Name)->
  	
  		{match,LINE,addTailRecursionArgs(E1,Name),addTailRecursionArgs(E2,Name)};


addTailRecursionArg({tuple,LINE,Exps},Name)->
  	
  		{tuple,LINE,addTailRecursionArgs(Exps,Name)};
		
		
addTailRecursionArg({cons,LINE,E1,E2},Name)->
  	
  		{cons,LINE,addTailRecursionArgs(E1,Name),addTailRecursionArgs(E2,Name)};
	

addTailRecursionArg({atom,LINE,Atom},_)->
  	
  		{atom,LINE,Atom};
		

addTailRecursionArg({integer,LINE,Int},_)->
  	
  		{integer,LINE,Int};


addTailRecursionArg({op,LINE,Op,E1,E2},Name)->
	
		{op,LINE,Op,addTailRecursionArgs(E1,Name),	addTailRecursionArgs(E2,Name)};
	

addTailRecursionArg({op,LINE,Op,E},Name)->
	
	{op,LINE,Op,addTailRecursionArgs(E,Name)};


addTailRecursionArg({call,LINE,{remote,LINE,EM,EF},Exps},Name)->
	
	
	{call,LINE,{remote,LINE,addTailRecursionArgs(EM,Name),addTailRecursionArgs(EF,Name)},addTailRecursionArgs(Exps,Name)};


addTailRecursionArg({call,LINE,EF,Exps},Name)->
	
	{atom,_,Fname}=EF,
	
	case Name == Fname of 
		true ->
			{call,LINE,EF,addTailRecursionArgs(Exps,Name)++[{op,1,'-',{var,1,'Acc'},{integer,1,1}}]};
		
		false ->
			{call,LINE,EF,addTailRecursionArgs(Exps,Name)}
	end;
	


addTailRecursionArg(Other, _)->
	Other.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%    		   DUPLICITY OF RULES              %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

already_exists(DuplicityVars,NewRule,TablaReglas,Index)->
	
	%io:format("Ya existe: ~n",[]),
	SelectedRule=NewRule++".",
	{ok,String,_}=erl_scan:string(lists:flatten(SelectedRule)),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	case Prev = ets:prev( TablaReglas, Index ) of 
		
		'$end_of_table' -> 
			%io:format("End of table ~n)",[]),
			false;
		
		_else ->
			Match=ets:match( TablaReglas,  {Prev,'$1'}),
			{Rule,_,_,_,_,_,_,_,_,_,_,_,_,_,_}=lists:nth(1,lists:nth(1,Match)),
			SelectedRule2=Rule++".",
			{ok,String2,_}=erl_scan:string(SelectedRule2),
			{ok,Forms2}=erl_parse:parse_form(String2),
			{function,1,Name2,Arity2,[{clause,1,Patterns2,Guards2,Body2}]}=Forms2,
				
				
			case (equal(DuplicityVars,Patterns,Patterns2)) and (equal(DuplicityVars,Guards,Guards2)) and (equal(DuplicityVars,Body,Body2)) of
				true ->
					%io:format("Iguales~n",[]),
					true;
				false ->
					%io:format("Diferentes~n",[]),
					already_exists(DuplicityVars,NewRule,TablaReglas,Index-1)
			end
	end.
		

	
%%% Igualdad de expresiones %%%

equal(Exp1,Exp2)->
	
	equal(false,Exp1,Exp2).

equal(_,[],[]) ->
	true;


equal(DuplicityVars,Exp1,Exp2)->
	
	
	%io:format("Exp1s ~p~n",[Exp1]),	
	%io:format("Exp2s ~p~n",[Exp2]),
	case erlang:is_list(Exp1) of
					
				true -> 
					
					case erlang:is_list(Exp2) of
							
						false ->  
							
							false;
						
						true ->
		  					case (length(Exp1) >1) of
								
								true ->
		   							[E1|Es1]=Exp1,
									case (length(Exp2) >1) of
										
										true ->
											[E2|Es2]=Exp2,
											equal(DuplicityVars,E1,E2) and equal(DuplicityVars,Es1,Es2);
										
										false ->
											false
									end;
								
								false ->
									case (length(Exp2) == length(Exp1)) of
										
										true ->
											[E1]=Exp1,
											[E2]=Exp2,
											equal(DuplicityVars,E1,E2);
										
										false ->
											false
									end
							end
					end;
				
				false -> 
					
					case erlang:is_list(Exp2) of
							
						false ->  
							
							case erlang:is_tuple(Exp1) of
									
								true ->
										
									case erlang:is_tuple(Exp2) of
											
											true ->
												
												equal2(DuplicityVars,Exp1,Exp2);
											
											false ->
													
												false
									end;
								
								false ->
										
									case erlang:is_tuple(Exp2) of
										
										true ->
												
												false;
											
										false ->
													
												equal3(DuplicityVars,Exp1,Exp2)
									end
							
							end;							
								
						
						true ->
		  
		   					false
					end
	
	end.
		
		  


equal2(DuplicityVars,Exp1, Exp2)->
		
	%io:format("Exp1 ~p~n",[Exp1]),
	%io:format("Exp2 ~p~n",[Exp2]),
	L1= size(Exp1),
	L2= size(Exp2),
	case L1 == L2 of
		
		true ->
		
			case L1 of
				2->
					{Type,E}=Exp1,
					{Type2,E2}=Exp2,
					
					case (Type==Type2) and equal(DuplicityVars,E,E2) of
						true ->
								true;
						false ->
								false
					end;
					
				3 ->
					
					{Type,_,E}=Exp1,
					{Type2,_,E2}=Exp2,
								
					%case (Type==var) of
					%	
					%	true -> 
					%			true;
					%	false ->
					%			case (Type2==var) of
					%					
					%					true ->
					%							true;
					%					false ->
					%							case (Type==Type2) and equal(E,E2) of
					%								true ->
					%									true;
					%								false ->
					%									false
					%								end
					%			end
					%end;
					case (Type == var) of 
						
						true -> 
								case (Type2== var) of
									
									true -> 
											case DuplicityVars of 
												true ->
													equal(DuplicityVars,E,E2);
												false ->
													true
											end;
									false ->
											false
								end;
						false ->
								case (Type==Type2) and equal(DuplicityVars,E,E2) of
										true ->
												true;
										false ->
												false
								end	
					end;
					
										
				4 ->
			
					{Type,_,E,Es}=Exp1,
					{Type2,_,E2,Es2}=Exp2,
					case (Type==Type2) and equal(DuplicityVars,E,E2) and equal(DuplicityVars,Es,Es2) of
						true ->
							true;
						false ->
							false
					end;
			
				5 -> 
				
					{Type,_,OP,E1,E2}=Exp1,
					{Type2,_,OP2,E12,E22}=Exp2,
					case (Type==Type2) and equal(DuplicityVars,OP,OP2) and equal(DuplicityVars,E1,E12) and equal(DuplicityVars,E2,E22) of
						true ->
							true;
						false ->
							false
					end
			end;
				
		false ->	
			false
	end.									 
												 
												 
												 
equal3(DuplicityVars,E1,E2)->
	
 	E1==E2.		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%     CREATE N (ARITY) OPERATORS ATOM2VAR     %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


new_var_actions(Arity) ->
	Actions=ets:new('Actions',  [ordered_set] ),
	new_var_actions2(Actions,Arity,1).


	
new_var_actions2(Actions,Arity,Index)->
	
	case Index==Arity of
		false ->
			Nom=string:concat("atom2var_",integer_to_list(Index)),
			%io:format("NOm ~p~n",[Nom]),
			ets:insert(Actions, {Index,{Nom,1.0,1}}),
			new_var_actions2(Actions,Arity,Index+1);
		true ->
			Nom=string:concat("atom2var_",integer_to_list(Index)),
				%io:format("NOm ~p~n",[Nom]),
			ets:insert(Actions, {Index,{Nom,1.0,1}}),
			Actions
	end.
			

arity(Table)->
	
	%io:format("~w~n",[ets:lookup(Table, 1)]),
	[{Key,{SelecRule,Rew,PrevActs,_,_,_,_,_}}]=ets:lookup(Table, 1),
	Rule=lists:append(SelecRule,"."),
	P1 = smerl:new(rule),
	{ok, P2} = smerl:add_func(P1, Rule),	
	%smerl:compile(P2),
 	%smerl:for_module(P2).
	[{function,_,_,Arity,_}|_]=smerl:get_forms(P2),
	Arity.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%      ETS TO FORMATED FILE  (GUI JAVA)       %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

etsinstances2file(Table,File)->
	{ok, FileDescriptor} = file:open(File, [write]),
	get_next_keyInstance(FileDescriptor,Table,1),
	file:close(FileDescriptor).


etsrules2file(Table,TableSteps,File)->
	{ok, FileDescriptor} = file:open(File, [write]),
	get_next_keyRule(FileDescriptor,Table, TableSteps,0),
	file:close(FileDescriptor).
	
etsactions2file(Table,File)->
	{ok, FileDescriptor} = file:open(File, [write]),
	get_next_keyAction(FileDescriptor,Table,0),
	file:close(FileDescriptor).	

etsprograms2file(Table,TablaSteps,File)->
	{ok, FileDescriptor} = file:open(File, [write]),
	get_next_keyProgram(FileDescriptor,Table,TablaSteps,0),
	file:close(FileDescriptor).	
	


get_next_keyInstance(FileDescriptor,Table,Key)->
	
	case Key == (ets:last(Table)+1) of
		false ->
			[{Index,{Ins,MML,B}}]=ets:lookup(Table,Key),
			io:format(FileDescriptor, "~4w//~p//~w//~p~n", [Index,Ins,MML,B]),
			get_next_keyInstance(FileDescriptor,Table,Key+1);
		true ->
			ok
	end.
			
	
get_next_keyRule(FileDescriptor, Instancias, TableSteps, Key) ->
        
	Next = ets:next( Instancias, Key ),
	
	case Next of
			'$end_of_table' -> 
				done;
			_else ->
				
    		    %io:format( " next ~w ~n ", [ Next ] ),
				%io:format("Encontrado: ~p~n",[ets:match( Instancias,  {Next,'$1'})]),		
				[[{Rule,Rew,PrevActs,CobP,RPos,CobN,RNeg,Size,Vars,Cons,Func,Struc,Rec,MML,MMLCov}]]=ets:match( Instancias,  {Next,'$1'}),
				[{_,Step}]=ets:lookup(TableSteps, Next),
				io:format(FileDescriptor, "~4w//~p//~w//~w//~w//~w//~w//~w//~w//~w//~w//~w//~w//~w//~w//~w//~w~n", [Next,Rule,Rew,PrevActs,CobP,RPos,CobN,RNeg,Size,Vars,Cons,Func,Struc,Rec,Step,MML,MMLCov]),	
				%io:format("~4w ~p ~w ~w ~n", [Next,Rule,Rew,PrevActs]),
    		    get_next_keyRule(FileDescriptor, Instancias,TableSteps, Next )
		end.
	

get_next_keyProgram(FileDescriptor, Instancias,TableSteps,  Key) ->
        
	Next = ets:next( Instancias, Key ),
	
	case Next of
			'$end_of_table' -> 
				done;
			_else ->
				
    		   	%io:format( " next ~w ~n ", [ Next ] ),
				%io:format("Encontrado: ~p~n",[ets:match( Instancias,  {Next,'$1'})]),		
				[[{KeysR,PosEx,NegEx,Opt,NumOpers}]]=ets:match( Instancias,  {Next,'$1'}),
				io:format("~p~n",[ets:match(TableSteps,'$1')]),
				io:format("NEXT: ~p~n",[Next]),
				[{_,Step}]=ets:lookup(TableSteps, Next),
				io:format(FileDescriptor, "~4w;~w;~w;~w;~w;~w;~w~n", [Next,sets:to_list(KeysR),sets:to_list(PosEx),sets:to_list(NegEx),Opt,NumOpers,Step]),		
    		    get_next_keyProgram(FileDescriptor, Instancias, TableSteps, Next )
		end.	
	
	
		
get_next_keyAction(FileDescriptor, Instancias,  Key) ->
        
	Next = ets:next( Instancias, Key ),
	
	case Next of
			'$end_of_table' -> 
				done;
			_else ->
				
    		   	%io:format( " next ~w ~n ", [ Next ] ),
				%io:format("Encontrado: ~p~n",[ets:match( Instancias,  {Next,'$1'})]),		
				[[{Action,_,Rew,Calls}]]=ets:match( Instancias,  {Next,'$1'}),
				io:format(FileDescriptor, "~4w;~p;~w;~w~n", [Next,Action,Rew,Calls]),		
    		    get_next_keyAction(FileDescriptor, Instancias, Next )
		end.
	
	