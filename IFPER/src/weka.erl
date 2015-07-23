-module(weka).

-compile(export_all).
%-record(arff, {global_optimality,rule_name,vars,arity,action,class}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%               GENERATE .ARFF                %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_arff(FileName,Relation,File,Actions,Rules) ->
	
	
	head_arff(FileName,Relation,Actions,Rules),
	
	case file:read_file_info(File) of
        {ok, FileInfo} ->
				
                 leer_fichero(File,FileName);
        {error, enoent} ->
                 % File doesn't exist
                 donothing
 	end.

leer_fichero(FileR,FileW) ->
	case file:open(FileR, read) of
		{ok,Fd} ->
			copiar_texto_linea(Fd,FileW),
			file:close(Fd),
			{ok};
		{error,Motivo} ->
			{error,Motivo}
	end.

copiar_texto_linea(Fd,FileW)->
	case io:get_line(Fd,'') of
		eof ->
			ok;
		{error, Motivo} ->
			{error, Motivo};
		Texto ->
			%io:format("Texto leido: ~p~n", [Texto]),
			file:write_file(FileW, Texto, [append]),
			copiar_texto_linea(Fd,FileW)
	end.


head_arff(FileName,Relation,Actions,Rules) ->
	
		case file:read_file_info(FileName) of
			 {ok, FileInfo} -> 
					file:delete(FileName), %io:format("Borro actions.arff~n",[]),
					file:write_file(FileName, "@relation "++Relation++"\n@attribute global_optimality real\n@attribute medium_size_Rules real\n@attribute medium_size_Programs real\n@attribute best_Rule_Ratio real\n@attribute best_Program_Ratio real\n@attribute rule_name {0,",[append]),
					rule_name(FileName,Rules,1),
					file:write_file(FileName,"@attribute size real\n@attribute arity real\n@attribute action {",[append]),
					action_name(FileName,Actions,1),
					
					file:write_file(FileName,"@attribute prev1 {0,",[append] ),
					action_name(FileName,Actions,1),
					
					file:write_file(FileName,"@attribute prev2 {0,",[append] ),
					action_name(FileName,Actions,1),
					
					file:write_file(FileName,"@attribute prev3 {0,",[append] ),
					action_name(FileName,Actions,1),
					
					file:write_file(FileName,"@attribute cob real\n@attribute cobNeg real\n@attribute PrevOps real\n@attribute vars real\n",[append]),
					file:write_file(FileName,"@attribute cons real\n@attribute func real\n@attribute struc real\n@attribute rec real\n",[append]),
					file:write_file(FileName,"@attribute class real\n@data\n",[append]);
			{error, enoent} ->
					%io:format("NO borro actions.arff~n",[]),
					file:write_file(FileName, "@relation "++Relation++"\n@attribute global_optimality real\n@attribute medium_size_Rules real\n@attribute medium_size_Programs real\n@attribute best_Rule_Ratio real\n@attribute best_Program_Ratio real\n@attribute rule_name {0,",[append]),
					rule_name(FileName,Rules,1),
					file:write_file(FileName,"@attribute vars real\n@attribute arity real\n@attribute action {",[append]),
					action_name(FileName,Actions,1),
					
					file:write_file(FileName,"@attribute prev1 {0,",[append] ),
					action_name(FileName,Actions,1),
					
					file:write_file(FileName,"@attribute prev2 {0,",[append] ),
					action_name(FileName,Actions,1),
					
					file:write_file(FileName,"@attribute prev3 {0,",[append] ),
					action_name(FileName,Actions,1),
					
					file:write_file(FileName,"@attribute cob real\n@attribute cobNeg real\n@attribute PrevOps real\n@attribute vars real\n",[append]),
					file:write_file(FileName,"@attribute cons real\n@attribute func real\n@attribute struc real\n@attribute rec real\n",[append]),
					file:write_file(FileName,"@attribute class real\n@data\n",[append])
		end.
		



	




rule_name(FileName,Table,Index)->
	
	End=ets:last(Table),
	case Index == End of
		false ->
			[{Key,{_,_,_,_,_,_,_,_,_,_,_,_,_,_,_}}]=ets:lookup(Table, Index),
			file:write_file(FileName,integer_to_list(Key)++",",[append]),
			rule_name(FileName,Table,Index+1);
		true ->
			[{Key,{_,_,_,_,_,_,_,_,_,_,_,_,_,_,_}}]=ets:lookup(Table, Index),
			file:write_file(FileName,integer_to_list(Key)++"}\n",[append])
	end.
		

action_name(FileName,Table,Index)->
	
	End=ets:last(Table),
	case Index == End of
		false ->
			[{Key,{_,_,_,_}}]=ets:lookup(Table, Index),
			file:write_file(FileName,integer_to_list(Key)++",",[append]),
			action_name(FileName,Table,Index+1);
		true ->
			[{Key,{_,_,_,_}}]=ets:lookup(Table, Index),
			file:write_file(FileName,integer_to_list(Key)++"}\n",[append])
	end.





new(FileName,Global_optimality,MedRules,MedProgs,RatRules,RatProgs,Rule_name,Size,Arity,Action,Prev1,Prev2,Prev3,CobRule,CobNegRule,PrevOps,Vars,Cons,Funcs,Strucs,Recs,Class)  ->
	%write necesita que todo sea strings
 	%file:write_file(FileName,  io_lib:format("~.5f",[Global_optimality])++","++io_lib:format("~.5f",[MedRules])++","
%++io_lib:format("~.5f",[MedProgs])++","++io_lib:format("~.5f",[RatRules])++","++io_lib:format("~.5f",[RatProgs])++","
%++integer_to_list(Rule_name)++","++io_lib:format("~.5f",[Vars])++","++integer_to_list(Arity)++","++integer_to_list(Action)++
%	","++io_lib:format("~.5f",[CobRule])++","++io_lib:format("~.5f",[Class])++"\n", [append]).
       
	file:write_file(FileName,  io_lib:format("~.5f",[Global_optimality])++","++io_lib:format("~.5f",[MedRules])++","
				   ++io_lib:format("~.5f",[0.0])++","++io_lib:format("~.5f",[0.0])++","++io_lib:format("~.5f",[0.0])
				   ++","++integer_to_list(Rule_name)++","++io_lib:format("~.5f",[Size])++","++integer_to_list(Arity)++","
				   ++integer_to_list(Action)++","++integer_to_list(0)++","++integer_to_list(0)++","++integer_to_list(0)
				   ++","++io_lib:format("~.5f",[CobRule])++","++io_lib:format("~.5f",[CobNegRule])++","++integer_to_list(PrevOps)
				   ++","++io_lib:format("~.5f",[Vars])++","++io_lib:format("~.5f",[Cons])++","++io_lib:format("~.5f",[Funcs])++","
				   ++io_lib:format("~.5f",[Strucs])++","++io_lib:format("~.5f",[Recs])++","++io_lib:format("~.5f",[Class])
				   ++"\n", [append]).
     


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%      Prev Operator of a Rule (att weka)     %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 givemePrevOps(PrevOps)->
	case length(PrevOps) of 
		0 ->{0,0,0};
		1 -> {lists:nth(1, PrevOps),0,0};
		2 -> {lists:nth(1, PrevOps),lists:nth(2, PrevOps),0};
		_ -> {lists:nth(1, PrevOps),lists:nth(2, PrevOps),lists:nth(3, PrevOps)}
	end.

		
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%         NUMBER OF VARIABLES OF A RULE       %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





count_variablesR(SelecRule)->
	Rule=lists:append(SelecRule,"."),
	P1 = smerl:new(rule),
	{ok, P2} = smerl:add_func(P1, Rule),	
    Forms=smerl:get_forms(P2),
	count_varName_funsGeneric(Forms).	

count_variablesT(Index,TableRules)->
	[{_,{SelecRule,_,_,_,_,_,_,_,_,_,_,_}}]=ets:lookup(TableRules, Index),
	Rule=lists:append(SelecRule,"."),
	P1 = smerl:new(rule),
	{ok, P2} = smerl:add_func(P1, Rule),	
    Forms=smerl:get_forms(P2),
	count_varName_funsGeneric(Forms).	
	
	
count_varName_funsGeneric([])->[]; 

count_varName_funsGeneric([{function,_,_,Arity,Clauses}|_]) -> 
	%[Arity,count_varName_clausesGeneric(Clauses)];
	[Arity,0];
 	
count_varName_funsGeneric(Other)-> 
	Other. 
	



count_varName_clausesGeneric([]) -> []; 

count_varName_clausesGeneric([{clause,_,Patterns,_,_}|_])-> 
	%NBody = change_varName_expressionsGeneric(Body), 
	count_varName_expressionsGeneric(Patterns); 
	%NGuards = change_varName_expressionsGeneric(Guards), 
	
count_varName_clausesGeneric(Other)-> 
	Other.	
	



count_varName_expressionsGeneric([])->0; 

count_varName_expressionsGeneric([Exp|Exps]) -> 
	
 	count_varName_expressionGeneric(Exp)+count_varName_expressionsGeneric(Exps);
	
count_varName_expressionsGeneric(Other)-> 
 	0.




count_varName_expressionGeneric({match,LINE,E1,E2})->
  	
  		count_varName_expressionGeneric(E1)+count_varName_expressionGeneric(E2);


count_varName_expressionGeneric({tuple,LINE,Exps})->
  	
  		count_varName_expressionsGeneric(Exps);
		
		
count_varName_expressionGeneric({cons,LINE,E1,E2})->
  	
  		count_varName_expressionGeneric(E1)+count_varName_expressionGeneric(E2);
	

count_varName_expressionGeneric({atom,LINE,Atom})->
  	
  		0;
		

count_varName_expressionGeneric({integer,LINE,Int})->
  	
  		0;


count_varName_expressionGeneric({op,LINE,Op,E1,E2})->
	
		count_varName_expressionGeneric(E1)+count_varName_expressionsGeneric(E2);
	

count_varName_expressionGeneric({op,LINE,Op,E})->
	
	count_varName_expressionGeneric(E);


count_varName_expressionGeneric({call,LINE,{remote,LINE,EM,EF},Exps})->
	
	
	count_varName_expressionsGeneric(Exps);


count_varName_expressionGeneric({call,LINE,EF,Exps})->
	
	count_varName_expressionsGeneric(Exps);

count_varName_expressionGeneric({var,LINE,Var})->
	1;
	
count_varName_expressionGeneric(Other)->
	0.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%               MAX(F(ACTION))                %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%best_action(Actions,Rules,1,1,0,0,0)

best_action(MedRules,MedProgs,RuleRatio,ProgRatio,NumPos,Actions,ListActions,Rules,LastRule,GlobalOpt,IndexA,IndexR,ClassMax,OP,Deuce)->
	
	
	%io:format("............Dentro BestAction-> LastRules: ~p, idRule: ~p ~n",[LastRule,IndexR]),
	EndA=ets:last(Actions),
	case IndexA == EndA+1 of 
		false ->
			
			[{KeyA,{Action,_,_,_}}]=ets:lookup(Actions, IndexA),
			
			case IndexR == LastRule+1 of
				false ->
					
					[{KeyR,{Rule,Opt,PrevOps,Cob,_,CobNeg,_,SizeRule,Vars,Cons,Funcs,Strucs,Rec,_,_}}]=ets:lookup(Rules, IndexR),
					{Prev1,Prev2,Prev3}=weka:givemePrevOps(PrevOps),
					%[Arity,NumVars]=weka:count_variablesR(Rule),
					
					case gb_sets:is_element({KeyA,KeyR},ListActions) of
								false ->
									%Class=clientWeka:client_DM_Clas(GlobalOpt,MedRules,MedProgs,RuleRatio,ProgRatio,integer_to_list(KeyR),
									%								SizeRule, 1, integer_to_list(KeyA),integer_to_list(Prev1),integer_to_list(Prev2),
									%								integer_to_list(Prev3),Cob+0.0,CobNeg+0.0, length(PrevOps)),

									Class=clientWeka:client_DM_Clas(GlobalOpt,MedRules,0.0,0.0,0.0,integer_to_list(0),
																	SizeRule, 1, integer_to_list(KeyA),integer_to_list(0),integer_to_list(0),
																	integer_to_list(0),Cob+0.0,CobNeg+0.0,length(PrevOps),Vars,Cons,Funcs,Strucs,Rec),
									
									%io:format("Weka: Action ~p, Rule ~p, Class: ~p~n",[KeyA,KeyR,Class]),
									
									
									case (Class > ClassMax) of
										true ->
											DeuceNew = [{KeyA,KeyR,SizeRule,Vars,Cons,Funcs,Strucs,Rec,Cob,CobNeg}],
											%io:format("Weka: BEST~n",[]),
											best_action(MedRules,MedProgs,RuleRatio,ProgRatio,NumPos,Actions,ListActions,Rules,LastRule,
														GlobalOpt,IndexA,IndexR+1,Class,KeyA,DeuceNew);
											
										false ->
											case (Class == ClassMax)  of %and (KeyA == OP) of
												true ->
													DeuceNew = Deuce ++ [{KeyA,KeyR,SizeRule,Vars,Cons,Funcs,Strucs,Rec,Cob,CobNeg}],
													best_action(MedRules,MedProgs,RuleRatio,ProgRatio,NumPos,Actions,ListActions,Rules,LastRule,
														GlobalOpt,IndexA,IndexR+1,ClassMax,KeyA,DeuceNew);
												false ->
													best_action(MedRules,MedProgs,RuleRatio,ProgRatio,NumPos,Actions,ListActions,Rules,LastRule,
														GlobalOpt,IndexA,IndexR+1,ClassMax,OP,Deuce)
											end
					
									end;
									
								true ->%io:format("Weka: EXISTS (~p~p)~n",[KeyA,KeyR]),
									best_action(MedRules,MedProgs,RuleRatio,ProgRatio,NumPos,Actions,ListActions,Rules,LastRule,
												GlobalOpt,IndexA,IndexR+1,ClassMax,OP,Deuce)
							end;

				true ->
					
					best_action(MedRules,MedProgs,RuleRatio,ProgRatio,NumPos,Actions,ListActions,Rules,LastRule,
								GlobalOpt,IndexA+1,1,ClassMax,OP,Deuce)
			end;
		
		true ->
			%RuleReturned=R,
			case length(Deuce) of
				0 ->
					RuleReturned= 0, KeyOp=0;
				1 ->
					{KeyOp,KeyR,Zret,Vret,Cret,Fret,Sret,Rret,Pret,Nret}= lists:nth(1, Deuce),
					RuleReturned=giveMeRule(KeyOp, ListActions,Rules,Zret,Vret,Cret,Fret,Sret,Rret,Pret,Nret);
					%RuleReturned=giveMeRule(Rules,Size_B,CobP_B,CobN_B);
				L ->
					%random:seed(now()),
					Random= random:uniform(),
					Index=trunc(Random/(1/L))+1,
					{KeyOp,KeyR,Zret,Vret,Cret,Fret,Sret,Rret,Pret,Nret}= lists:nth(Index, Deuce),
					RuleReturned=giveMeRule(KeyOp, ListActions, Rules,Zret,Vret,Cret,Fret,Sret,Rret,Pret,Nret)
					%RuleReturned=giveMeRule(Rules,Zret,Pret,Nret)
			end,
			
			{ClassMax,KeyOp,RuleReturned}
	end.


giveMeRule(Rules, Size,CobP,CobN)->
	Rs = ets:match(Rules, {'$1',{'_','_','_',CobP,'_',CobN,'_','_','_','_','_','_','_','_','_'}}),
	
	case length(Rs) of
		0 ->
			imposible;
		
		Lenght ->
			
			Random=random:uniform(),
			Index=trunc(Random/(1/Lenght))+1,
			[R]=lists:nth(Index, Rs),
			R
	end.
	
	
	
giveMeRule(A,ListActions,Rules,Size_B,Vars_B,Cons_B,Funcs_B,Strucs_B,Rec_B,CobP_B,CobN_B)->
	
   %Rs = ets:match(Rules, {'$1',{'_','_','_',CobP_B,'_',CobN_B,'_',Size_B,Vars_B,Cons_B,Funcs_B,Strucs_B,Rec_B,'_','_'}}),
	Rs = ets:match(Rules, {'$1',{'_','_','_',CobP_B,'_',CobN_B,'_','_',Vars_B,Cons_B,Funcs_B,Strucs_B,Rec_B,'_','_'}}),
	case length(Rs) of
		0 ->
			imposible;
		
		Lenght ->
			%io:format("RulesMatchPrev: ~p~n",[Rs]),
			%io:format("Operator: ~p~n",[A]),
			%io:format("ListActions: ~p~n",[ListActions]),
			RsFiltered= lists:filter(fun ([X])-> case (gb_sets:is_element({A,X},ListActions))  of true -> false; false -> true  end end, Rs),	
			%io:format("RulesMatchFilter: ~p~n",[RsFiltered]),
			%random:seed(now()),
			Random=random:uniform(),
			NLenght=length(RsFiltered),
			Index=trunc(Random/(1/NLenght))+1,
			[R]=lists:nth(Index, RsFiltered),
			R
	end.

	
	


argmaxQ(ListActions,NumPos,NewState,MedRules,MedProgs,RuleRatio,ProgRatio, TabOperators, TabRules, LastRule) ->
		argmaxQ2(ListActions,NumPos,NewState,MedRules,MedProgs,RuleRatio,ProgRatio, TabOperators, TabRules, LastRule, 1, 1, -10, 0, 0).

argmaxQ2(ListActions,NumPos,NewState, MedRules,MedProgs,RuleRatio,ProgRatio, TabOperators, TabRules, LastRule, IndexOp, IndexR, ClassMax, A, R)->
	
		
	EndOp=ets:last(TabOperators),
	case IndexOp == EndOp+1 of 
		false ->
			
			[{KeyA,{Operator,_,_,_}}]=ets:lookup(TabOperators, IndexOp),
			
			case IndexR == LastRule+1 of
				false ->
					
					[{KeyR,{Rule,Opt,PrevOps,Cob,_,CobNeg,_,Size,Vars,Cons,Funcs,Strucs,Rec,MML,MMLCov}}]=ets:lookup(TabRules, IndexR),
					{Prev1,Prev2,Prev3}=weka:givemePrevOps(PrevOps),
					%[Arity,NumVars]=weka:count_variablesR(Rule),
					
					case gb_sets:is_element({KeyA,KeyR},ListActions) of
						false ->
					
							%Class=clientWeka:client_DM_Clas(NewState,MedRules,MedProgs,RuleRatio,ProgRatio,integer_to_list(KeyR) ,
							% Size, 1, integer_to_list(KeyA),integer_to_list(Prev1),integer_to_list(Prev2),integer_to_list(Prev3),Cob+0.0,CobNeg+0.0,length(PrevOps)),
					
							Class=clientWeka:client_DM_Clas(NewState,MedRules,0.0,0.0,0.0,integer_to_list(0) , Size, 1, 
													integer_to_list(KeyA),integer_to_list(0),integer_to_list(0),
													integer_to_list(0),Cob+0.0,CobNeg+0.0,length(PrevOps),Vars,Cons,Funcs,Strucs,Rec),
					
					
							%io:format("Weka: Action ~p, Rule ~p, Class: ~p~n",[KeyA,KeyR,Class]),
							case (Class > ClassMax)  of
									true ->
											%io:format("Weka: BEST~n",[]),
											argmaxQ2(ListActions,NumPos,NewState,MedRules,MedProgs,RuleRatio,ProgRatio, TabOperators, 
													 TabRules, LastRule,IndexOp,IndexR+1,Class,KeyA,KeyR);
											
									false ->
																
											argmaxQ2(ListActions,NumPos,NewState,MedRules,MedProgs,RuleRatio,ProgRatio, TabOperators, 
													 TabRules, LastRule,IndexOp,IndexR+1,ClassMax,A,R)
					
							end;
						true->
							argmaxQ2(ListActions,NumPos,NewState,MedRules,MedProgs,RuleRatio,ProgRatio, TabOperators, 
													 TabRules, LastRule,IndexOp,IndexR+1,ClassMax,A,R)
					end;

				true ->
					
					argmaxQ2(ListActions,NumPos,NewState,MedRules,MedProgs,RuleRatio,ProgRatio, TabOperators, TabRules, LastRule,IndexOp+1,1,ClassMax,A,R)
			end;
		
		true ->
			
			{ClassMax,A,R}
	end.

		

