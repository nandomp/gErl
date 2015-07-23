-module(reinforcement).
-compile(export_all).



iniQ(VarGlob)->
	
	LastOp= ets:last(ets:lookup_element(VarGlob, tableOperators, 2)),
	LastRule = ets:last(ets:lookup_element(VarGlob, tablaReglas, 2)),
	%PolicyWEKA = ets:lookup_element(VarGlob, policyWEKA, 2),
	
	case ets:lookup_element(VarGlob, wantProgs,2) of
				
		0 ->
			GlobalOpt= ets:lookup_element(VarGlob, globalOptRules, 2);
			
		1 ->%globalOptPrograms
			GlobalOpt= ets:lookup_element(VarGlob, globalOptRules, 2)
	end,
	
			
	[{_,{Rule,_,_,_,_,_,_,_,_,_,_,_,_,_,_}}]=ets:lookup(ets:lookup_element(VarGlob, tablaReglas, 2), 1),
	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,_,_,Arity,_}=Forms,
	

	NumPos=ets:lookup_element(VarGlob, numPos, 2),
	io:format("--7a--\n",[]),
	
	case ets:lookup_element(VarGlob, inf_Prev, 2) of
		true -> 
			File=ets:lookup_element(VarGlob, policyPrevFile, 2);
			%iniQ2(ets:lookup_element(VarGlob, policyPrevFile, 2),LastOp,LastRule,GlobalOpt,Arity,NumPos,1,1,VarGlob);
		false ->
			File="DataArff.txt",
			iniQ2("DataArff.txt",LastOp,LastRule,GlobalOpt,Arity,NumPos,1,1,VarGlob)
	end,
	
	io:format("--7b--\n",[]),
	weka:gen_arff("Actions.arff","Actions",File,ets:lookup_element(VarGlob, tableOperators, 2),ets:lookup_element(VarGlob, tablaReglas, 2)),
	io:format("--7c--\n",[]),
	clientWeka:client_DM_Gen(),
	io:format("--7d--\n",[]).


iniQ2(File,LastOp,LastRule,GlobalOpt,Arity,NumPos,Oi,Ri,VarGlob)->
	
		case Oi==LastOp+1 of
		
		false -> 
				
			case Ri==LastRule+1 of
					
				false ->
					[{KeyR,{SelecRule,Cob,PrevActs,CobPos,RulesPos,CobNeg,RulesNeg,Size,Vars,Cons,Func,Struc,Rec,MML,MMLCov}}]=ets:lookup(ets:lookup_element(VarGlob, tablaReglas, 2), Ri),
					
					%weka:new("DataArff.txt",GlobalOpt,0.0,1.0,0.0,0.0,Ri,0.0,Arity,Oi,0,0,0,1.0,0.0,0,1.00),
					weka:new(File,GlobalOpt,0.0,0.0,0.0,0.0, 0,MML,Arity,Oi,0,0,0,1.0,0.0,0,Vars,Cons,Func,Struc,Rec,0.9),
					%ets:insert(PolicyWEKA, {ets:last(PolicyWEKA)+1,{ets:lookup_element(VarGlob, globalOptRules,2),ets:lookup_element(VarGlob, globalOptPrograms,2),MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),ets:lookup_element(VarGlob, bestProgramRatio, 2),KeyR,SizeRule,Ari,KeyA,Cob/NumPos,Q}}),
	
			
					iniQ2(File,LastOp,LastRule,GlobalOpt,Arity,NumPos,Oi,Ri+1,VarGlob);
					%Solo una regla (no todas, ya que son los mismos datos, solo es necesaria 1 por operador)
					%iniQ2(PolicyWEKA,LastOp,LastRule,GlobalOpt,Arity,NumPos,Oi,LastRule+1);
				
				true ->
					
					iniQ2(File,LastOp,LastRule,GlobalOpt,Arity,NumPos,Oi+1,1,VarGlob)
			end;
		
		true ->
			
			ok
	end.
	
calcularQ(VarGlob,Reward,NewState,MedRules,MedProgs,NumPos)->

	LastRule= ets:lookup_element(VarGlob, lastRule, 2),
	%io:format("LAst rule: ~p~n",[LastRule]),
	ListActions=ets:lookup_element(VarGlob, listActions, 2),
	
	{ClassMax,A,R} = weka:argmaxQ(ListActions,NumPos,NewState,MedRules,MedProgs,ets:lookup_element(VarGlob, bestRuleRatio, 2),ets:lookup_element(VarGlob, bestProgramRatio, 2), ets:lookup_element(VarGlob, tableOperators, 2), ets:lookup_element(VarGlob, tablaReglas, 2), LastRule),
	
	io:format("Calc Q: q->~p, learningR->~p, Reward->~p, disc->~p, Classmax->~p ~n",[ets:lookup_element(VarGlob, q, 2),ets:lookup_element(VarGlob, learningRate, 2),Reward,ets:lookup_element(VarGlob, discountFactor, 2),ClassMax]),
	Q=ets:lookup_element(VarGlob, q, 2) + ets:lookup_element(VarGlob, learningRate, 2) * ( Reward + ets:lookup_element(VarGlob, discountFactor, 2) * ClassMax - ets:lookup_element(VarGlob, q, 2)),

	{Q,A,R,ClassMax}.
			




creditAssignActions(TabAct,[Ult|Prev],Cob,Dis,NumPos)->
	
	case Prev of
			
			[] ->
								
				[{KeyA,{NomAct,ArityOp,CobAnt,Veces}}]=ets:lookup(TabAct, Ult),
				
				%NomAct= element(1,element(2,lists:nth(1,ets:lookup(TabAct, Ult)))),
				%Veces=  element(3,element(2,lists:nth(1,ets:lookup(TabAct, Ult)))),
				%CobAnt=element(2,element(2,lists:nth(1,ets:lookup(TabAct, Ult)))),
				%io:format("Last   ~p-~n",[NomAct]),
				CobDis=Dis*Cob,
				NewCob=CobAnt+(((CobDis/NumPos)-CobAnt)/Veces),
				ets:update_element(TabAct, Ult, {2,{NomAct,ArityOp,NewCob,Veces}});
				%ets:update_element(TabAct, Ult, {2,{NomAct,NewCob,Veces}});
				%io:format("~p~n",[ets:match(TabAct,'$1')]);
							
			_else ->
				
				[{KeyA,{NomAct,ArityOp,CobAnt,Veces}}]=ets:lookup(TabAct, Ult),
				%NomAct= element(1,element(2,lists:nth(1,ets:lookup(TabAct, Ult)))),
				%Veces=  element(3,element(2,lists:nth(1,ets:lookup(TabAct, Ult)))),
				%CobAnt=element(2,element(2,lists:nth(1,ets:lookup(TabAct, Ult)))),
				%io:format("Last   ~p-~n",[NomAct]),
				CobDis=Dis*Cob,
				NewCob=CobAnt+(((CobDis/NumPos)-CobAnt)/Veces),
				ets:update_element(TabAct, Ult, {2,{NomAct,ArityOp,NewCob,Veces}}),
				%ets:update_element(TabAct, Ult, {2,{NomAct,NewCob,Veces}}),
				creditAssignActions(TabAct,Prev,CobDis,Dis,NumPos)
		
		end.

creditAssignActions2([Ult|Prev],Cob,Dis)->
		
	%[Ult|Prev]=Prevs,
	io:format("Ult: ~w~n",[Ult]),
	io:format("Prev: ~w~n",[Prev]),
	
	TabAct = ets:new('Actions',  [ordered_set] ),
				ets:insert(TabAct, {1,{"atom1var",1.0,1}}),
				ets:insert(TabAct, {2,{"atom2var",1.0,1}}),
				ets:insert(TabAct, {3,{"atom3var",1.0,1}}),
				ets:insert(TabAct, {4,{"atom4var",1.0,1}}),
	%io:format("~p~n",[ets:match(TabAct,'$1')]),
	io:format("-------------------------~n",[]),
	
		case Prev of
			[] ->
				
				NomAct= element(1,element(2,lists:nth(1,ets:lookup(TabAct, Ult)))),
				io:format("Last   ~p-~n",[NomAct]),
				NewCob=Dis*Cob,
				%ets:update_element(Actions, 1, {2,{"atom1var",NewCob,Veces+1}}),
				ets:update_element(TabAct, Ult, {2,{NomAct,NewCob,7}}),
				io:format("~p~n",[ets:match(TabAct,'$1')]);
				
			
			_else ->
				
			
				NomAct= element(1,element(2,lists:nth(1,ets:lookup(TabAct, Ult)))),
				NewCob=Dis*Cob,
				io:format("~p-~n",[NomAct]),
				ets:update_element(TabAct, Ult, {2,{NomAct,NewCob,7}}),
				io:format("~p~n",[ets:match(TabAct,'$1')]),
				creditAssignActions2(Prev,NewCob,0.9)
		
		end,
		io:format("~p~n",[ets:match(TabAct,'$1')]).



gen_matrix_reward(Actions,TablaReglas)->	

	RewActs=matrix_reward_Act(Actions,1,[]),
	RewRules=matrix_reward_Act(TablaReglas,1,[]),
					  
	[[I,J,lists:nth(I,RewActs)+lists:nth(J,RewRules)]||I<-lists:seq(1,length(RewActs)),J<-lists:seq(1,length(RewRules))].
					  
	


matrix_reward_Act(Actions,Index,RewA)->
	
	case Index==ets:last(Actions)+1 of
		false ->
			[{_,{_,RewAct,_}}]=ets:lookup(Actions, Index),
			TempA=lists:append(RewA,[RewAct]),
			matrix_reward_Act(Actions,Index+1,TempA);
		true ->
			RewA
	end.

			
matrix_reward_Rules(TablaReglas,Index,RewR)->
	
	case Index==ets:last(TablaReglas)+1 of
		false ->
			[{_,{_,RewRule,_}}]=ets:lookup(TablaReglas, Index),
			TempR=lists:append(RewR,[RewRule]),
			matrix_reward_Rules(TablaReglas,Index+1,TempR);
		true ->
			RewR
	end.



betterAction(Actions,TablaReglas)->
	Matrix=gen_matrix_reward(Actions,TablaReglas),
	%io:format("Matrix: ~w~n",[Matrix]),
	maxMatrix(Matrix,1,0,0,0).

maxMatrix(Matrix,Index,A,R,Rew)->
	
	End=length(Matrix),
	
	case Index == End+1 of
		false ->
			Nth=lists:nth(Index, Matrix),
			%io:format("Nth: ~w~n",[Nth]),
			NewRew=lists:nth(3,Nth),
			%io:format("NewRew: ~w~n",[NewRew]),
			case NewRew > Rew of 
				true ->
					maxMatrix(Matrix,Index+1,lists:nth(1,Nth),lists:nth(2,Nth),NewRew);
				false ->
					maxMatrix(Matrix,Index+1,A,R,Rew)
			end;
		true ->
			[A,R,Rew]
	end.
			


randomAction(AS,RS,ListActions)->
	randomAction2(AS,RS,ListActions,0).

randomAction2(AS,RS,ListActions,Veces)->

	A=random:uniform(AS),
	R=random:uniform(RS),
	case gb_sets:is_element({A,R},ListActions) of
		true ->
			case Veces < 1000 of
				true ->					
					randomAction2(AS,RS,ListActions,Veces+1);
				false ->
					{A,R,gb_sets:add_element({A,R},ListActions)}
			end;
		false ->
			{A,R,gb_sets:add_element({A,R},ListActions)}
	end.



averageOptGlobal(ListsOptGlobal,Number)->
  
  	case length(ListsOptGlobal)>Number of 
	
			true -> 
				Length=length(ListsOptGlobal),
				Tail=Length-Number,
				AvgList=lists:nthtail(Tail, ListsOptGlobal),
				Sum= lists:foldl(fun(X,Sum)->Sum+X end, 0, AvgList),
				Avg=Sum/Number,
				lists:nth(Length,ListsOptGlobal)-Avg;

			false ->
				1
	end.


randomActionNonUniform(AS,RS,ListActions,InfPrevia)->
	randomActionNonUniform2(AS,RS,ListActions,InfPrevia,0).

randomActionNonUniform2(AS,RS,ListActions,InfPrevia,Veces)->

	
	ListTab=ets:tab2list(InfPrevia),
	ListTabOrd=[{I,element(1,lists:nth(length(ListTab)+1-I,ListTab))}||I<-lists:reverse(lists:seq(1,length(ListTab)))],
	Parts= lists:foldr(fun(X,Sum)-> Sum+X end, 0, lists:seq(1,length(ListTab))),
	ListTabOrdPorc=[{element(2,lists:nth(I,ListTabOrd)),50/length(ListTabOrd)+50/Parts*element(1,lists:nth(I,ListTabOrd))}||I<-lists:seq(1,length(ListTabOrd))],
	
		
	A=randomActionNonUniform3(ListTabOrdPorc,1,0),
	R=random:uniform(RS),
	
	case gb_sets:is_element({A,R},ListActions) of
		true ->
			case Veces < 100 of
				true ->					
					randomActionNonUniform2(AS,RS,ListActions,InfPrevia,Veces+1);
				false ->
					{A,R,gb_sets:add_element({A,R},ListActions)}
			end;
		false ->
			{A,R,gb_sets:add_element({A,R},ListActions)}
	end.

randomActionNonUniform3(ListTabOrdPorc,Index,Percentage)->
	

			
			case random:uniform(1000) =< ((element(2,lists:nth(Index, ListTabOrdPorc)) + Percentage)*10) of
			
				true -> 
			 			element(1,lists:nth(Index, ListTabOrdPorc));


				false ->
						randomActionNonUniform3(ListTabOrdPorc,Index+1,Percentage+element(2,lists:nth(Index, ListTabOrdPorc)))
			end.
		
actionNotUsed(OS,RS,ListActions)->
	actionNotUsed2(OS,RS,ListActions,1,1).

actionNotUsed2(OS,RS,ListActions,Oi,Ri)->

	%A=random:uniform(AS),
	%R=random:uniform(RS),
	case Oi==OS+1 of
		
		false -> 
				
			case Ri==RS+1 of
					
				false ->
					
					case gb_sets:is_element({Oi,Ri},ListActions) of
		
							true ->
			
								actionNotUsed2(OS,RS,ListActions,Oi,Ri+1);
				
							false ->
					
								{Oi,Ri,gb_sets:add_element({Oi,Ri},ListActions)}
					end;
				
				true ->
					
					actionNotUsed2(OS,RS,ListActions,Oi+1,1)
			end;
		
		_Else ->
			
			{0,0,ListActions}
	end.
			
			
reuseOrNot(Op, Rule, VarGlob)->
	io:format("Rule: ~p   ",[Rule]),
	[{KeyR,{SelecRule,Cob,PrevActs,CobPos,RulesPos,CobNeg,RulesNeg,Size}}]= ets:lookup(ets:lookup_element(VarGlob, tablaReglas, 2),Rule),
	[{KeyA,{NameOp,ArityOp,CobAnt,Veces}}]= ets:lookup(ets:lookup_element(VarGlob, tableOperators, 2), Op),			
	%{Prev1,Prev2,Prev3}=weka:givemePrevOps(PrevActs),
	Policy=ets:lookup_element(VarGlob, policyPrevETS, 2),
	%io:format("~p~n",[ets:match(Policy,'$1')]),
	%PrevOpR = lists:foldr( fun(X,Acc)-> case X == 0 of true -> Acc; false -> Acc++[X] end end, [], [Prev1,Prev2,Prev3]),
	PrevOpR=PrevActs,
	io:format("PrevOpR: ~p ~n", [PrevOpR]),
	case PrevOpR of 
		[] -> 
			Op;
		
		_Else ->
			
			OpsReturned=lists:foldl(fun (I, Acc) -> [{I,SeqOps}] = ets:lookup(Policy,I), case lists:suffix(PrevOpR, SeqOps) of true -> case length(SeqOps)>length(PrevOpR) of true -> Acc ++ [lists:nth(length(SeqOps)-length(PrevOpR),SeqOps)]; false -> Acc end   ; false -> Acc end end, [],lists:seq(1, ets:last(Policy))),
			io:format("OpsReturned: ~p,", [OpsReturned]),
	
			%Random Multi Ops

			case length(OpsReturned) >0 of
				true ->
					Random=random:uniform(),
					io:format(" Random1: ~p,", [Random]),
					Op_policy=lists:nth(erlang:trunc(Random/(1/length(OpsReturned)))+1, OpsReturned),	
					io:format(" Op_policy: ~p,", [Op_policy]),	
					%Random Between policies

					Random2= random:uniform(),
					io:format(" Random2: ~p~n,", [Random2]),
					case length(PrevOpR) of 
						3 -> 
							%0.9
							case Random2 < 0.9 of
								true ->
									Op_policy;
								false ->
									Op
							end;
						2 ->
							%0.6
							case Random2 < 0.6 of
								true ->
									Op_policy;
								false ->
									Op
							end;
						1 ->
							
							case Random2 < 0.3 of
								true ->
									Op_policy;
								false ->
									Op
							end;
						0->
							Op
					end;
		

				false ->
			
					Op
			end
	end.
					
			
		

savePolicy(VarGlob,SaveFile)->
	
	Programs= ets:lookup_element(VarGlob, programs, 2),
	TablaReglas= ets:lookup_element(VarGlob, tablaReglas, 2),
	
	case ets:lookup_element(VarGlob, wantProgs, 2)  of
		1 ->
			
			SetBP= ets:lookup_element(VarGlob, bestProgs, 2),
			ListBP=lists:map(fun (X)-> [{X,{SetRules,_,_,Opt,_}}]=ets:lookup(Programs, X),{X,sets:to_list(SetRules),Opt} end, sets:to_list(SetBP)),
			%io:format("~p~n",[ListBP]),
			SortSetBP= lists:keysort(3,ListBP),
			%io:format("~p~n",[SortSetBP]),
			{I,R,O}=lists:nth(length(SortSetBP), SortSetBP),
			%io:format("~p~n",[R]),
			SecuencesOps= lists:map(fun (X) -> [{_,{_,_,PrevActs,_,_,_,_,_}}]=ets:lookup(TablaReglas, X), PrevActs end, R),
			EtsBP=ets:new('bestProgs',[ordered_set]),
			[ets:insert(EtsBP,{I,lists:nth(I,SecuencesOps)})||I<-lists:seq(1, length(SecuencesOps))],
			%io:format("~p~n",[ets:match(EtsBP,'$1')]),
			ets:tab2file(EtsBP, SaveFile);
		
		0->
			ok
	end.

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
		