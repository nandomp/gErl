-module(main).
-compile(export_all).


main()-> 
	
	%compile:file(util,nowarn_unused_vars),
	%compile:file(actions,nowarn_unused_vars),

	Pos="../Instances/Old/playtennis.pex",
	Neg="../Instances/Old/playtennis.nex",
	FileOperators="../Operators/op_pt.erl",
	
	OutRules="../Solution/outputRules.txt",
	OutAct="../Solution/outputActions.txt",
	OutPrograms="../Solution/outputPrograms.txt",
	
	WantProgs=0,
	Debug=1,
	%data_analysis:analysis(Pos,Neg),
	
	main(Pos,Neg,OutRules,OutAct,OutPrograms,FileOperators,"policyLOAD","policySAVE", WantProgs,Debug, 100).
		
%%% Llamada desde la GUI Multiple SAVING pcy

main_save(Pos,Neg,Steps,FileOperators,WantProgs,PolicySavePath, OutR,OutA,OutP) ->

	
	%io:format("Entro por main/2",[]),
	OutRules=("../Solution/Rules_"++OutR)++".csv",
	OutAct=("../Solution/Acts_"++OutA)++".csv",
	OutPrograms=("../Solution/Progs_"++OutP)++".csv",
	Debug=0,
	main(Pos,Neg,OutRules,OutAct,OutPrograms,FileOperators,"",PolicySavePath,WantProgs,Debug,Steps).	

%%% Llamada desde la GUI Multiple LOADING pcy

main_load(Pos,Neg,Steps,FileOperators,WantProgs,PolicyLoadPath, OutR,OutA,OutP) ->

	
	%io:format("Entro por main/2",[]),
	%OutRules=("../Solution/Rules_"++OutR)++".csv",
	%OutAct=("../Solution/Acts_"++OutA)++".csv",
	%OutPrograms=("../Solution/Progs_"++OutP)++".csv",
	Debug=0,
	main(Pos,Neg,OutR,OutA,OutP,FileOperators,PolicyLoadPath,"",WantProgs,Debug,Steps).

%%% Llamada desde la GUI

main(Pos,Neg,Steps,FileOperators,WantProgs,PolicyLoadPath,PolicySavePath) ->

	
	%io:format("Entro por main/2",[]),
	OutRules="../Solution/outputRules.txt",
	OutAct="../Solution/outputActions.txt",
	OutPrograms="../Solution/outputPrograms.txt",
	Debug=1,
	main(Pos,Neg,OutRules,OutAct,OutPrograms,FileOperators,PolicyLoadPath,PolicySavePath,WantProgs,Debug,Steps).


main(Pos,Neg,OutRules,OutAct,OutPrograms,FileOperators,PolicyPrev,PolicySaveName, WantProgs,Debug,Steps)->	
	
	random:seed(22,23,24),
	
	%%% ETS almacen variables globales
	
	io:format("-- Initial Variables --",[]),
	VarGlob = ets:new('VarGlob',  [ordered_set] ),
	ets:insert(VarGlob, {wantProgs,WantProgs}),
	ets:insert(VarGlob, {steps,Steps}),
	ets:insert(VarGlob, {stepAct,0}),
	ets:insert(VarGlob, {fileTabOperatorsPrev,PolicyPrev}),
	ets:insert(VarGlob, {bestRuleRatio,-100000.0}),
	ets:insert(VarGlob, {bestProgramRatio,0.0}),
	ets:insert(VarGlob, {bestRuleId,0}),
	ets:insert(VarGlob, {bestProgramId,0}),
	ets:insert(VarGlob, {bestProgramIdStep,0}),
	
	io:format("-- Delete previous default .arff files --",[]),
	
	case file:read_file_info("DataArff.txt") of
			 {ok, _} -> 
					file:delete("DataArff.txt");
			_Else ->
				ok
	end,
	
	
	%%% ETS inicialización tablas instancias/reglas %%%

	io:format("-- Initialise ETS tables --",[]),
	InstanciasP = ets:new('InstanciasP',  [ordered_set] ),
	InstanciasN = ets:new('InstanciasN',  [ordered_set] ),
	InstanciasPRec =ets:new('InstanciasPRec',  [ordered_set] ),
	InstanciasNRec =ets:new('InstanciasNRec',  [ordered_set] ),
	RecInstancesPosBase= ets:new('InstanciasPosRecB', [ordered_set] ),
	RecInstancesNegBase = ets:new('InstanciasPosRecB', [ordered_set] ),
	PolicyWeka =  ets:new('PolicyWEKA', [ordered_set] ),
	TablaReglas = ets:new('ReglasGen',  [ordered_set] ),
	TablaReglasStep = ets:new('ReglasStep',  [ordered_set] ),
	
	ets:insert(VarGlob, {instanciasP,InstanciasP}),
	ets:insert(VarGlob, {instanciasN,InstanciasN}),
	ets:insert(VarGlob, {instanciasPRec,InstanciasPRec}),
	ets:insert(VarGlob, {instanciasNRec,InstanciasNRec}),
	ets:insert(VarGlob, {recInstancesPosBase,RecInstancesPosBase}),
	ets:insert(VarGlob, {recInstancesNegBase,RecInstancesNegBase}),
	ets:insert(VarGlob, {policyWEKA,PolicyWeka}),
	
	
	%%% Modulo operadores usuario %%%

	{TableOperators,NumOperators,OperatorsModule}=operators:new_operators(FileOperators),
	

	%%% ETS almacen variables globales (tablas operadores, num operadores y modulo operadores) %%%

	ets:insert(VarGlob, {tableOperators,TableOperators}),
	ets:insert(VarGlob, {numOperators,NumOperators}),
	ets:insert(VarGlob, {operatorsModule,OperatorsModule}),
	
	
	io:format("-- Instances  --",[]),
	
	actions:instance2rule(Pos, Neg, InstanciasP, InstanciasN, TablaReglas,TablaReglasStep,VarGlob),
	
	util:recInstancesTransform(InstanciasP, InstanciasPRec),
	util:recInstancesTransform(InstanciasN, InstanciasNRec),
	
	util:recInstancesBaseCases(InstanciasP,RecInstancesPosBase),
	util:recInstancesBaseCases(InstanciasN,RecInstancesNegBase),
	
	
		
	%%% ETS almacen variables globales (tablas ETS anteriores)

	
	ets:insert(VarGlob, {tablaReglas,TablaReglas}),
	ets:insert(VarGlob, {tablaReglasStep,TablaReglasStep}),
	
	ets:insert(VarGlob, {learningRate,0.5}),
	ets:insert(VarGlob, {discountFactor,0.5}),
	ets:insert(VarGlob, {q,1}),
	
	%%% MML F,C,V from instances, update mml values in instances and rules %%%
	
	Instances_mml=mml:decom_Ne(VarGlob),
	ets:insert(VarGlob, {instances_mml,Instances_mml}),

	mml:update_mml_e(VarGlob),
	mml:update_mmm_rules_ini(VarGlob),
	util:etsinstances2file(InstanciasP, "../Solution/mmlPosE.txt"),
	util:etsinstances2file(InstanciasN, "../Solution/mmlNegE.txt"),
		
	
	
	%%% Programas: tabla programas y Matrix Programas [P1,P2,Opt]
	
	io:format("-- Matrix & Programs  --",[]),
	Programs = ets:new('Programs',[ordered_set]),
	ProgramsStep = ets:new('ReglasStep',  [ordered_set] ),
	
	io:format("--Matrix & Programs 1--",[]),
	programs:generate_programs_ini(ProgramsStep,TablaReglas, Programs, ets:lookup_element(VarGlob, numPos,2),ets:lookup_element(VarGlob, numNeg,2)),
	
	io:format("--Matrix & Programs 2--",[]),
	%io:format("Programs: ~p~n",[ets:match(Programs,'$1')]),
	%io:format("~p~n",[ets:lookup(VarGlob, numPos)]),
	MatrixPrograms = ets:new('MatrixProgs',  [ordered_set] ),
	programs:gen_matrix_overlap(VarGlob,Programs,ets:lookup_element(VarGlob, numPos,2),ets:lookup_element(VarGlob, numNeg,2),MatrixPrograms),
	
	io:format("-- Matrix & Programs c--",[]),
	%io:format("CardMatrix: ~p~n",[ets:last(MatrixPrograms)]),
	%io:format("~p~n",[ets:match(MatrixPrograms,'$1')]),
	
	io:format("--Matrix & Programs d--",[]),
	
	BestProgs= sets:new(),

	
	%%% ETS almacen variables globales (tablas Programas y Matrix Programs)

	ets:insert(VarGlob, {programs,Programs}),
	ets:insert(VarGlob, {programsStep,ProgramsStep}),
	ets:insert(VarGlob, {matrixPrograms,MatrixPrograms}),
	ets:insert(VarGlob, {bestProgs, BestProgs}),
		

	
	io:format("-- Actions & More Variables--",[]),
	
	%io:format("~p~n",[ets:match(TablaReglas,'$1')]),
	%io:format("-----------------------------------------------~n"),
	%io:format("~p~n",[ets:match(TableOperators,'$1')]),
	%io:format("-----------------------------------------------~n"),
	%io:format("~p~n",[ets:match(InstanciasP,'$1')]),
	%io:format("-----------------------------------------------~n"),
	%io:format("~p~n",[ets:match(InstanciasN,'$1')]),
	%io:format("-----------------------------------------------~n"),
	%io:format("~p~n",[ets:match(TablaReglas,'$1')]),
	%io:format("-----------------------------------------------~n"),		
	%io:format("~p~n",[ets:match(Programs,'$1')]),
	%io:format("-----------------------------------------------~n"),
	
	%%% Listas de Acciones y Uniones ya realizadas y Ultimos OptGlobal
	
	ListActions=gb_sets:new(),
	ListUnions=gb_sets:new(),
	ListsOptGlobal=[],
	
	%%% ETS almacen variables globales (Listas de Acciones y Uniones ya realizadas y Ultimos OptGlobal)

	ets:insert(VarGlob, {listActions,ListActions}),
	ets:insert(VarGlob, {listUnions,ListUnions}),
	ets:insert(VarGlob, {listOptGlobal,ListsOptGlobal}),
	ets:insert(VarGlob, {lastRule,ets:last(TablaReglas)}),
	
	ets:insert(VarGlob, {globalOptPrograms,programs:global_optimality(Programs)}),
	ets:insert(VarGlob, {globalOptRules,programs:global_optimalityRules(TablaReglas)}),
	%io:format("Variables globales: ~p~n",[ets:match(VarGlob,'$1')]),
	
	
	%%% Existe Política previa? %%%
	
	case file:read_file_info(PolicyPrev) of
		
		{ok, FileInfo} -> 
	
				%%% Inicializacion modelo Q(s,a) =1 
				ets:insert(VarGlob, {policyPrevFile,PolicyPrev}),
				ets:insert(VarGlob, {inf_Prev,true}),
				reinforcement:iniQ(VarGlob),
				io:format("-- Exists Policy prev--",[]),
				metaflip(VarGlob,Debug,ets:lookup_element(VarGlob, steps, 2));
				
		{error, enoent} ->
				ets:insert(VarGlob, {policyPrevFile,"DataArff.txt"}),
				ets:insert(VarGlob, {inf_Prev,false}),
				%%% Inicializacion modelo Q(s,a) =1 
				reinforcement:iniQ(VarGlob),
				io:format("-- Does not exist policy prev--",[]),	
				metaflip(VarGlob,Debug,ets:lookup_element(VarGlob, steps, 2))

	end,
	
	
	


	io:format("-- gErl finished--",[]),
	case Debug of 
		1 ->
	io:format("-----------------------------------------------~n"),
	io:format("~p~n",[ets:match(TablaReglas,'$1')]),
	io:format("-----------------------------------------------~n"),
	io:format("~p~n",[ets:match(TableOperators,'$1')]),
	io:format("-----------------------------------------------~n"),	
	%io:format("~p~n",[ets:match(Programs,'$1')]),
	io:format("-----------------------------------------------~n"),	
	io:format("~p~n",[ets:match(MatrixPrograms,'$1')]),
	io:format("-----------------------------------------------~n"),	
	%io:format("~p~n",[ets:match(TablaReglasStep,'$1')]),
	io:format("-----------------------------------------------~n");
	%io:format("~p~n",[ets:match(ets:lookup_element(VarGlob, tablaWeka, 2),'$1')]);
		0 ->
			ok
	
	end,

	io:format("-- Saving Rules & programs--",[]),
	util:etsrules2file(TablaReglas,TablaReglasStep,OutRules),
	util:etsactions2file(TableOperators,OutAct),
	util:etsprograms2file(Programs,ProgramsStep,OutPrograms),
	
	%%%%% GUARDAR: Reuso política-> MatrixQ %%%%%
	
	case PolicySaveName == "" of
		false ->
			%reinforcement:savePolicy(VarGlob,PolicySaveName);
			case file:read_file_info(PolicyPrev) of
				 {ok, FileInfo2} ->
				    	file:copy(PolicyPrev, PolicySaveName);
       			 {error, enoent} ->
                 		file:copy("DataArff.txt", PolicySaveName)
			end;
 	
		true ->
			ok
	end,
	
	
	

	
	%% Tabla operadores a lista, ordenar, a tabla y almacenar %%
	
	%ListTaB=ets:tab2list(TableOperators),
	%Ordered=lists:sort(fun(A,B)-> element(3,element(2,A))>=element(3,element(2,B)) end, ListTaB),
	%OpsOrd = ets:new('OpsOrd',  [set] ),
	%io:format("Ordered ~p~n",[Ordered]),
	%[ets:insert(OpsOrd,I)|| I<-Ordered],
	%ets:tab2file(OpsOrd, "tableOperators2"),
	
		
	case Debug of 
		1 ->
			ok;
		%io:format("Lists Actions: ~p~n",[ets:lookup_element(VarGlob, listActions, 2)]),
		%io:format("Variables globales: ~p~n",[ets:match(VarGlob,'$1')]);
		0 ->
			ok
	end,
	
	%%% Best Rule %%%%

	BR=ets:lookup_element(VarGlob, bestRuleId, 2),
	io:format("BestRule: ~p~n",[BR]),
	io:format("~p~n",[ets:match(ets:lookup_element(VarGlob, tablaReglasStep, 2),'$1')]),
	[{BR,Step}]=ets:lookup(ets:lookup_element(VarGlob, tablaReglasStep, 2), BR),
	
	
	%%% Best Program %%%%
	
	BP=ets:lookup_element(VarGlob, bestProgramId, 2),
	io:format("BestProgram: ~p~n",[BP]),
	BPs=ets:lookup_element(VarGlob, bestProgramIdStep, 2),
	
	ets:delete(InstanciasP),
	ets:delete(InstanciasN),
	ets:delete(TablaReglas),
	ets:delete(TableOperators),
	ets:delete(Programs),
		
	%[OutRules,OutAct,OutPrograms].
	["../IFPER/Solution/outputRules.txt","../IFPER/Solution/outputActions.txt","../IFPER/Solution/outputPrograms.txt",BR,Step,BP,BPs].





metaflip(_,_, 0)->
	
	ok;


metaflip(VarGlob,Debug, Step) ->
	
		
	%Epsilon=reinforcement:averageOptGlobal(ListsOptGlobal,20),
	Epsilon=1000,
	BR=ets:lookup_element(VarGlob, bestRuleId, 2), %% 0 at first step
	
	
	%%% Particular Problems stop condition $$$
	case BR =/= 0 of
		true ->
			[{BR,{_,_,_,CobPosRuleSelected,_,_,_,_,_,_,_,_,_,_,_}}]=ets:lookup(ets:lookup_element(VarGlob, tablaReglas,2), BR),
			case (BR =/= 0) and (CobPosRuleSelected >=ets:lookup_element(VarGlob, numPos, 2)) of
				true -> 
					Stop=true;
				false ->
					Stop=false
			end;
		false ->
			Stop=false
	end,
	
	%case (ets:lookup_element(VarGlob, wantProgs,2)==1) and (sizeRules:stopCriterium(ets:last(ets:lookup_element(VarGlob, programs, 2)), VarGlob)) of
	%Stop=false,
	case false of 
				  
		false ->
		
			case Step rem 20 of 
				
				0 -> 
						
						%% Generación del modelo incremental%%
						case Debug of 
								1 ->
									io:format("Modelo---~n",[]);
								0 ->
									ok
						end,
						weka:gen_arff("Actions.arff","Actions",ets:lookup_element(VarGlob, policyPrevFile, 2),ets:lookup_element(VarGlob, tableOperators, 2),ets:lookup_element(VarGlob, tablaReglas, 2)),
						clientWeka:client_DM_Gen(),
							
					 		
						%----------ets:update_element(VarGlob, globalOptPrograms, {2,programs:global_optimality(ets:lookup_element(VarGlob, programs, 2))}),
						%ets:update_element(VarGlob, globalOptRules, {2,programs:global_optimalityRules(ets:lookup_element(VarGlob, tablaReglas, 2))}),
																			
						%%Última regla para poder ser elegida por el modelo, como es nuevo modelo, me quedo con todas las reglas. (cuando no genero modelo, tengo que buscar entre las entrenadas, no nuevas).
						LastRule=ets:last(ets:lookup_element(VarGlob, tablaReglas, 2)),
										
										
						%% Selección de la mejor acción a partir del modelo %%

						%MedRules=programs:mediumSizeRules(ets:lookup_element(VarGlob, tablaReglas, 2)), %% Size
						MedRules=programs:mediumSizeRules(ets:lookup_element(VarGlob, tablaReglas, 2)), %% Size
						MedPrograms=programs:mediumSizePrograms(ets:lookup_element(VarGlob, programs, 2)), %% Number of rules
						case ets:lookup_element(VarGlob, wantProgs,2) of
											
								1 ->	%Global Opt Programs
										{QsaModel,BestOpNew,BestRule}=weka:best_action(MedRules, MedPrograms, ets:lookup_element(VarGlob, bestRuleRatio, 2),
																					   ets:lookup_element(VarGlob, bestProgramRatio, 2),ets:lookup_element(VarGlob, numPos,2),
																					   ets:lookup_element(VarGlob, tableOperators, 2),ets:lookup_element(VarGlob, listActions, 2),
																					   ets:lookup_element(VarGlob, tablaReglas, 2),LastRule,
																					   ets:lookup_element(VarGlob, globalOptRules, 2),1,1,-1,0,[]);
											
								0 ->	%Global Opt Rules
										{QsaModel,BestOpNew,BestRule}=weka:best_action(MedRules, MedPrograms, ets:lookup_element(VarGlob, bestRuleRatio, 2),
																					   ets:lookup_element(VarGlob, bestProgramRatio, 2),ets:lookup_element(VarGlob, numPos,2),
																					   ets:lookup_element(VarGlob, tableOperators, 2),ets:lookup_element(VarGlob, listActions, 2),
																					   ets:lookup_element(VarGlob, tablaReglas, 2),LastRule,
																					   ets:lookup_element(VarGlob, globalOptRules, 2),1,1,-1,0,[])
						end,
						
					%%%%globalOptPrograms
						BestAct=BestOpNew,						
						NewListActions=gb_sets:add_element({BestAct,BestRule},ets:lookup_element(VarGlob, listActions, 2)),				
						ets:update_element(VarGlob, listActions, {2,NewListActions});
			
				_Else ->
						
						MedRules=programs:mediumSizeRules(ets:lookup_element(VarGlob, tablaReglas, 2)),
						MedPrograms=programs:mediumSizePrograms(ets:lookup_element(VarGlob, programs, 2)),
						
						%% Selección de la mejor acción a partir del modelo %%
						case ets:lookup_element(VarGlob, wantProgs,2) of
											
								1 ->	
										{QsaModel,BestOpNew,BestRule}=weka:best_action(MedRules, MedPrograms, ets:lookup_element(VarGlob, bestRuleRatio, 2), 
																					   ets:lookup_element(VarGlob, bestProgramRatio, 2),ets:lookup_element(VarGlob, numPos,2),
																					   ets:lookup_element(VarGlob, tableOperators, 2),ets:lookup_element(VarGlob, listActions, 2),
																					   ets:lookup_element(VarGlob, tablaReglas, 2),ets:lookup_element(VarGlob, lastRule, 2),
																					   ets:lookup_element(VarGlob, globalOptRules, 2),1,1,-1,0,[]);
											
								0 ->	
										{QsaModel,BestOpNew,BestRule}=weka:best_action(MedRules, MedPrograms, ets:lookup_element(VarGlob, bestRuleRatio, 2),
																					   ets:lookup_element(VarGlob, bestProgramRatio, 2),ets:lookup_element(VarGlob, numPos,2),
																					   ets:lookup_element(VarGlob, tableOperators, 2),ets:lookup_element(VarGlob, listActions, 2),
																					   ets:lookup_element(VarGlob, tablaReglas, 2),ets:lookup_element(VarGlob, lastRule, 2),
																					   ets:lookup_element(VarGlob, globalOptRules, 2),1,1,-1,0,[])
									
						end,	
						
						
						BestAct=BestOpNew,						


						NewListActions=gb_sets:add_element({BestAct,BestRule},ets:lookup_element(VarGlob, listActions, 2)),
						ets:update_element(VarGlob, listActions, {2,NewListActions}),
										
						LastRule=ets:lookup_element(VarGlob, lastRule, 2)
					
			
			end,
			
						
			%io:format("BestR---> ~p BestOp--> ~p~n",[BestRule,BestOpNew]),
			%En el caso de no encontrar mejor accion, lanzo una aleatoria.
			case ((BestRule==0) or (BestAct==0)) of
				
				true ->
					
					{BestActR,BestRuleR,NewListActions2}=reinforcement:actionNotUsed(ets:last(ets:lookup_element(VarGlob, tableOperators, 2)), ets:last(ets:lookup_element(VarGlob, tablaReglas, 2)),ets:lookup_element(VarGlob, listActions, 2)),
					
					
					case (BestRuleR==0) or (BestActR==0) of
						
						true->
							case Debug of 
								1 ->
									io:format("FIN: se han generado todas las reglas posibles",[]);
								0 ->
									ok
							end,
							RowRule=[{KeyR,{SelecRule,Cob,PrevActs,CobPos,RulesPos,CobNeg,RulesNeg,Size,Vars,Cons,Func,Struc,Rec,Mml,CovMml}}]= ets:lookup(ets:lookup_element(VarGlob, tablaReglas, 2), 1),
							RowAction=[{KeyA,{NameOp,ArityOp,CobAnt,Veces}}]= ets:lookup(ets:lookup_element(VarGlob, tableOperators, 2), 1),				
							metaflip(VarGlob,Debug,0);
						
						false->
							ets:update_element(VarGlob, listActions, {2,NewListActions2}),
							RowRule=[{KeyR,{SelecRule,Cob,PrevActs,CobPos,RulesPos,CobNeg,RulesNeg,Size,Vars,Cons,Func,Struc,Rec,Mml,CovMml}}]= ets:lookup(ets:lookup_element(VarGlob, tablaReglas, 2), BestRuleR),
							RowAction=[{KeyA,{NameOp,ArityOp,CobAnt,Veces}}]= ets:lookup(ets:lookup_element(VarGlob, tableOperators, 2), BestActR),
							case Debug of 
								1 ->
									io:format("Action Random (0,0) to  (~w,~w)~n ",[BestActR,BestRuleR]);
								0 ->
									ok
							end
					end;
							
					
				false ->
					
					%io:format("Best: ~p RowRule: ~w~n ",[BestRule,ets:lookup(ets:lookup_element(VarGlob, tablaReglas, 2), BestRule)]),
					NewListActions2=NewListActions,
					RowRule=[{KeyR,{SelecRule,Cob,PrevActs,CobPos,RulesPos,CobNeg,RulesNeg,Size,Vars,Cons,Func,Struc,Rec,Mml,CMml}}]=ets:lookup(ets:lookup_element(VarGlob, tablaReglas, 2), BestRule),
					RowAction=[{KeyA,{NameOp,ArityOp,CobAnt,Veces}}]=ets:lookup(ets:lookup_element(VarGlob, tableOperators, 2), BestAct)
					%RowRule=ets:lookup(ets:lookup_element(VarGlob, tablaReglas, 2), BestRule),
					%RowAction=ets:lookup(ets:lookup_element(VarGlob, tableOperators, 2), BestAct)
				
			end,
	
			
			
			ets:update_element(VarGlob, lastRule, {2,LastRule}),
			ets:update_element(VarGlob, q, {2,QsaModel}),
	
			% Load user's operators %

			OperatorsModule=ets:lookup_element(VarGlob, operatorsModule, 2),
			OpSelected=fun OperatorsModule:NameOp/1,
			NewruleS=OpSelected(SelecRule),
			
			case Debug of 
				1 ->
					io:format("---------------------------------------------------------------~n"),
					io:format("RULE TRANSFORMATION: (~p) ~p  ---(~p)--->  ~p~n ",[KeyR,SelecRule,NameOp,NewruleS]);
				0 ->
					ok
			end,
			
			
			case ets:lookup_element(VarGlob, wantProgs, 2) of
				
				1 ->		
					flipRulesPrograms(VarGlob, NewruleS, RowRule, RowAction, BestAct, BestRule, Debug, Step, length(NewruleS));
			
				0 ->
					flipRules(VarGlob, NewruleS, RowRule, RowAction, BestAct, BestRule, Debug, Step, length(NewruleS))
					
			end;

		true->
			case Debug of 
				1 ->
		 			io:format("acabo por average<0.001 -> ~p~n",[Epsilon]),
					io:format("Opt: ~p~n",[ets:lookup_element(VarGlob, listOptGlobal, 2)]);
				0 ->
					ok
			end,
			metaflip(VarGlob,Debug,0)
	end.






flipRulesPrograms(VarGlob, NewruleS, RowRule, RowAction, BestAct, BestRule, Debug, Step, IndexNewRules)->
	
	ets:update_element(VarGlob, stepAct, {2,Step}),
	
	[{KeyR,{SelecRule,CobRuleSelected,PrevActs,CobPosRuleSelected,RulesPosRuleSelected,CobNegRuleSelected,RulesNegSelected,
			SizeSelected,VarsSelected,ConsSelected,FuncSelected,StrucSelected,RecSelected,MMLSelected,CovMMLSelected}}]=RowRule,
	[{KeyA,{NameOp,ArityOp,CobAnt,Veces}}]=RowAction,
	
	NumPos=ets:lookup_element(VarGlob, numPos, 2),
	NumNeg= ets:lookup_element(VarGlob, numNeg, 2),
	Newrule=lists:nth(IndexNewRules, NewruleS),
	
	%io:format("RULES: ~p ~n",[NewruleS]),
	%io:format("Index: ~p, R: ~p ~n",[IndexNewRules,Newrule]),
	
	NumOpers=ets:lookup_element(VarGlob, numOperators, 2),
	
	SelectedRule=lists:flatten(Newrule++"."),
	
	%io:format("SR: ~p ~n",[SelectedRule]),
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,_,_,Arity,_}=Forms,
	Ari=Arity,
	
	
	%% Is it allowed the duplicity of variables (with different names)? f(A)->B and F(A2)->B 
	DuplicityVars=false,
	
	case util:already_exists(DuplicityVars,Newrule,ets:lookup_element(VarGlob, tablaReglas, 2),ets:last(ets:lookup_element(VarGlob, tablaReglas, 2))+1) of
					
		false ->
							
			
			{Cob,PosCob,CobNeg,NegCov}=util:cobertura_Funcional(VarGlob, Newrule),
			NewKeyRule=ets:last(ets:lookup_element(VarGlob, tablaReglas, 2))+1,
			
			case Debug of 
				1 ->
					
					io:format("Cob:  ~p, PosCob:  ~p, CobNeg: ~p, NegCob: ~p~n ",[Cob,PosCob,CobNeg,NegCov]);
				0 ->
					ok
			end,
			
					
			%%% Insercion Reglas %%%
					
			%%% Optimalidad Reglas %%%

			{_,_,Vars,Cons,Funcs,Strucs,Recs}=sizeRules:sizeOfRuleDecomposed(Newrule),			
			%OptRule= sizeRules:optRules(VarGlob, PrevActs, Cob, CobNeg, SizeRule),
			OptRule=mml:getOptMML(VarGlob, PosCob, NegCov, Newrule, length(PrevActs)+1),
			
			FoilGain=OptRule,
			SizeRule=mml:getMML(VarGlob, Newrule),
			MMLCov=mml:getCovMML(VarGlob, PosCob, NegCov, Newrule),
			
				case Debug of 
				1 ->
					
					io:format("Size ~p,Ari:  ~p, NumVars:  ~p~n ",[SizeRule,Ari,Vars]);
				0 ->
					ok
			end,
			
			
			ets:insert(ets:lookup_element(VarGlob, tablaReglas, 2), {NewKeyRule,{Newrule,OptRule,[KeyA|PrevActs],Cob,PosCob,CobNeg,NegCov,SizeRule,Vars,Cons,Funcs,Strucs,Recs,SizeRule,MMLCov}}),
			ets:insert(ets:lookup_element(VarGlob, tablaReglasStep, 2), {NewKeyRule,ets:lookup_element(VarGlob, steps, 2)-Step}),
			
			case OptRule > ets:lookup_element(VarGlob, bestRuleRatio, 2) of
				
				true ->
					ets:update_element(VarGlob, bestRuleRatio, {2,OptRule}),
					ets:update_element(VarGlob, bestRuleId, {2,NewKeyRule});
			
				_Else ->
					ok
			end,
									
			%%% Generación de programas %%%

			
			case (Cob =/= 0) of
				
				true ->
				
					%io:format("Inserto Regla como programa unitario: ~w~n -> ",[NewKeyRule]),
					programs:newrule2programs(NewKeyRule,Cob,PosCob,CobNeg,NegCov,PrevActs,VarGlob),
					programs:add2Matrix(VarGlob),
					
					%io:format("~p~n",[ets:match(MatrixPrograms,'$1')]),
					%io:format("Add Regla/Programa c: ~w~n -> ",[ets:last(Programs)]),
					%{IdProgA,P1a,P2a,NewUnions,OptAdd}=programs:addition(ets:last(Programs),Unions,NumPos,Programs),
					
					{IsBestProg,IdProgA,P1a,P2a,NewUnions,OptAdd}=programs:addition_withMatrix(VarGlob),
					ets:update_element(VarGlob, listUnions, {2,NewUnions}),
					
					case IsBestProg of
						false ->
							programs:add2Matrix(VarGlob);
						true ->
							ets:update_element(VarGlob, bestProgramIdStep, {2,ets:lookup_element(VarGlob, steps, 2)-Step})
					end,
					
					%io:format("~p~n",[ets:match(MatrixPrograms,'$1')]),
					%io:format("Voy a UNION -> ~n",[]),	
					%io:format("end--: ~p~n",[ets:lookup_element(VarGlob, matrixPrograms, 2)]),
					
					{IsBestProg1,IdProgU,P1u,P2u,NewUnions2,OptUni}=programs:union_programs_matrix(VarGlob),
					ets:update_element(VarGlob, listUnions, {2,NewUnions2}),
					case OptUni == -100 of 
						true -> 
							ok;
						false ->
							case IsBestProg1 of
								false ->
									programs:add2Matrix(VarGlob);
								true ->
									ets:update_element(VarGlob, bestProgramIdStep, {2,ets:lookup_element(VarGlob, steps, 2)-Step})
							end
					end;
					
					%io:format("~p~n",[ets:match(MatrixPrograms,'$1')]);
					%io:format("Uniones realizadas: ~w~n",[NewUnions2]);
				
				false ->
					
					%io:format("Voy a UNION -> ~n",[]),	
					P1a=0,P2a=0,OptAdd=-300,
					{IsBestProg2,IdProgU,P1u,P2u,NewUnions2,OptUni}=programs:union_programs_matrix(VarGlob),
					ets:update_element(VarGlob, listUnions, {2,NewUnions2}),
					IdProgA=0,
					%{IdProgU,P1u,P2u,NewUnions2,OptUni}=programs:union_programs_ranking(Programs, NumPos, Unions),
					case OptUni == -100 of 
						true -> 
							ok;
						false ->
							case IsBestProg2 of
								false ->
									programs:add2Matrix(VarGlob);
								true ->
									ok
							end
					end
					
					%io:format("~p~n",[ets:match(MatrixPrograms,'$1')])
					%io:format("Uniones realizadas: ~w~n",[NewUnions2])
			end,
			
		
			
			%% Generacion fichero .arff para Weka %%
			%[Ari,NumVars]=weka:count_variablesR(Newrule),
						
			
			GlobalOptNew=programs:global_optimality(ets:lookup_element(VarGlob, programs, 2)),
			GlobalOptRules=programs:global_optimalityRules(ets:lookup_element(VarGlob, tablaReglas, 2)),
			
			
			NewListsOptGlobal=ets:lookup_element(VarGlob, listOptGlobal, 2)++[GlobalOptNew],
			ets:update_element(VarGlob, listOptGlobal, {2,NewListsOptGlobal}),
			
						
			case Debug of 
				1 ->
					io:format("Weka Instance: State(GlobOpt): ~p, Rule: ~p, Vars: ~p, Arity: ~p, Operator: ~p, CobRule: ~p, Inc(Class):~p ~n",[ets:lookup_element(VarGlob, globalOptPrograms,2),KeyR,SizeRule,Ari,KeyA,Cob/NumPos,GlobalOptNew-ets:lookup_element(VarGlob, globalOptPrograms, 2)]);
				0 ->
					ok
			end,
		
			%%% Q calcularQ %%%
			
			GlobalOptPAnt=ets:lookup_element(VarGlob,globalOptPrograms, 2),
			GlobalOptRAnt=ets:lookup_element(VarGlob,globalOptRules, 2),
			
			MedRules=programs:mediumSizeRulesMML(ets:lookup_element(VarGlob, tablaReglas, 2)),
			MedPrograms=programs:mediumSizePrograms(ets:lookup_element(VarGlob, programs, 2)),
			{Q,A,R,ClassMax}=reinforcement:calcularQ(VarGlob,FoilGain,GlobalOptRules,MedRules,MedPrograms,NumPos),
			
			case Debug of 
				1 ->
					io:format("Med Rules: ~w,  Med Programs: ~w  ~n",[MedRules,MedPrograms]);
				0 ->
					ok
			end,
			
			
			%ets:insert(ets:lookup_element(VarGlob, tablaWEKA, 2), {{ets:lookup_element(VarGlob, globalOptPrograms,2), KeyR, SizeRule, Ari, Cob/NumPos, KeyA},{Q}}),
			case Debug of 
				1 ->
					io:format("Q prev: ~w --> Q new: ~w ---- argmaxQ -> (Op:~w, R:~w)=~w ~n",[ets:lookup_element(VarGlob, q, 2),Q,A,R,ClassMax]);
				0 ->
					ok
			end,
			
			% Nueva clase ARFF -> Q(s,a)
			
			{Prev1,Prev2,Prev3}=weka:givemePrevOps(PrevActs),
				
			weka:new(ets:lookup_element(VarGlob, policyPrevFile, 2),ets:lookup_element(VarGlob, globalOptRules,2),MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),
					 ets:lookup_element(VarGlob, bestProgramRatio, 2),0,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobPosRuleSelected+0.0,CobNegRuleSelected+0.0,length(PrevActs),
								 Vars,Cons,Funcs,Strucs,Recs,Q),
			
												
			%%Fin Step%%			
			case Debug of 
				1 ->
					io:format("Step: ~w | RULE + UNION + [ADDITION] |  Action: (~w,~w), Addtion: (~w,~w), ProgramAdd: ~w, OptAdd: ~w, Union: (~w,~w), ProgramUnion: ~w, OptIndiv: ~w, Opt Before: ~w, Opt After: ~w ~n",[Step,BestAct,BestRule,P1a,P2a,IdProgA,OptAdd,P1u,P2u,IdProgU,OptUni,ets:lookup_element(VarGlob, globalOptPrograms, 2),GlobalOptNew]),
					%io:format("Uniones: ~w~n",[NewUnions2]),
					io:format(" Weka Instance: State(GlobOpt): ~p, Rule: ~p, Vars: ~p, Arity: ~p, Operator: ~p, CobRule: ~p, Qp(Class):~p ~n",[ets:lookup_element(VarGlob, globalOptPrograms,2),KeyR,SizeRule,Ari,KeyA,Cob/NumPos,Q]);
				0 ->
					ok
			end,
			
			%ets:update_element(VarGlob, globalOptPrograms, {2,GlobalOptNew-GlobalOptPAnt}),
			%ets:update_element(VarGlob, globalOptRules, {2,GlobalOptRules-GlobalOptRAnt}),

			ets:update_element(VarGlob, globalOptPrograms, {2,GlobalOptNew}),
			ets:update_element(VarGlob, globalOptRules, {2,GlobalOptRules}),
			
			case IndexNewRules > 1 of
				
				true ->
					flipRulesPrograms(VarGlob, NewruleS, RowRule, RowAction, BestAct, BestRule, Debug, Step, IndexNewRules-1);
					%flipRulesPrograms(NewRules, TablaReglas,TablaReglasStep, InstanciasN, InstanciasP, KeyA, KeyR,  NumPos, PrevActs, ArityOp, Veces, CobAnt, Actions, NameOp, Programs, MatrixPrograms, Unions, GlobalOpt, BestAct, BestRule, Steps, NewListActions2, OperatorsModule, LastRule, NewListsOptGlobal, Inf_Prev, InfPrevETS, Hasta, IndexNewRules-1);

				false ->
					
					metaflip(VarGlob, Debug, Step-1)
					%metaFlip(InstanciasP, InstanciasN, TablaReglas, LastRule, TablaReglasStep,Actions, NewListActions2,Programs,MatrixPrograms,NewUnions2,OperatorsModule, NewListsOptGlobal, Inf_Prev, InfPrevETS, 1,Hasta,Steps-1)	
					%metaFlip(InstanciasP, InstanciasN, TablaReglas, LastRule, Actions, NewListActions2,Programs,NewUnions2,Hasta,Steps-1);	
			end;
				
		true ->
			
			case Debug of 
				1 ->			
					io:format("Nueva Regla ya existente en ETS reglas~n ",[]);
				0 ->
					ok
			end,
			%%% Unión %%%
			{IsBestProgram3,IdProgU,P1u,P2u,NewUnions2,OptUni}=programs:union_programs_matrix(VarGlob),
			ets:update_element(VarGlob, listUnions, {2,NewUnions2}),
			case OptUni == -100 of 
				true -> 
						ok;
				false ->
					case IsBestProgram3 of
						false ->
							programs:add2Matrix(VarGlob);
						true ->
							ets:update_element(VarGlob, bestProgramIdStep, {2,ets:lookup_element(VarGlob, steps, 2)-Step})
					end
			end,			

			
			%%% Generacion fichero .arff para Weka %%%
		
			SizeRuleNO= testCovP:sizeOfRule(Newrule),
			{_,_,Vars,Cons,Funcs,Strucs,Recs}=sizeRules:sizeOfRuleDecomposed(Newrule),
			{Cob,PosCob,CobNeg,NegCov}=util:cobertura_Funcional(VarGlob, Newrule),
			
			%%% Optimalidad Global %%%
			GlobalOptNew2=programs:global_optimality(ets:lookup_element(VarGlob, programs, 2)),
			GlobalOptRules=programs:global_optimalityRules(ets:lookup_element(VarGlob, tablaReglas, 2)),
			
		 	GlobalOptPAnt=ets:lookup_element(VarGlob,globalOptPrograms, 2),
			GlobalOptRAnt=ets:lookup_element(VarGlob,globalOptRules, 2),
			
			
			NewListsOptGlobal=ets:lookup_element(VarGlob, listOptGlobal, 2)++[GlobalOptNew2],
			ets:update_element(VarGlob, listOptGlobal, {2,NewListsOptGlobal}),		
			SizeRule=mml:getMML(VarGlob, Newrule),
			%io:format("----------------GlobalOptNew: ~w~n ",[GlobalOptNew]),
			case Debug of 
				1 ->
					io:format("Weka Instance: State(GlobOpt): ~p, Rule: ~p, Vars: ~p, Arity: ~p, Operator: ~p, CobRule: ~p, Qp(Class):~p ~n",[ets:lookup_element(VarGlob, globalOptPrograms,2),KeyR,SizeRule,Ari,KeyA,Cob/NumPos,GlobalOptNew2-ets:lookup_element(VarGlob, globalOptPrograms, 2)]);
				0 ->
					ok
			end,
			
		 	%%--------NUEVO------------Aunque ya exista, calculo su optimalidad
			%OptRule= sizeRules:optRules(VarGlob, PrevActs, Cob, CobNeg, SizeRule),
			OptRule=mml:getOptMML(VarGlob, PosCob, NegCov, Newrule, length(PrevActs)+1),
		 
			%%%% Q=calcularQ %%%
			
			MedRules=programs:mediumSizeRules(ets:lookup_element(VarGlob, tablaReglas, 2)),
			MedPrograms=programs:mediumSizePrograms(ets:lookup_element(VarGlob, programs, 2)),
			{Q,A,R,ClassMax}=reinforcement:calcularQ(VarGlob, OptRule ,GlobalOptRules,MedRules,MedPrograms,NumPos),
			case Debug of 
				1 ->
					io:format("Med Rules: ~w,  Med Programs: ~w  ~n",[MedRules,MedPrograms]);
				0 ->
					ok
			end,
			case Debug of 
				1 ->
					io:format("Q prev: ~w --> Q new: ~w ---- argmaxQ -> (Op:~w, R:~w)=~w ~n",[ets:lookup_element(VarGlob, q, 2),Q,A,R,ClassMax]);
				0 ->
					ok
			end,
			
			%%% Weka %%%
			
			%io:format("Q prev: ~w --> Q new: ~w ---- argmaxQ -> (Op:~w, R:~w)=~w ~n",[ets:lookup_element(VarGlob, q, 2),Q,A,R,ClassMax]),
			% Nueva clase ARFF -> Q(s,a)

			{Prev1,Prev2,Prev3}=weka:givemePrevOps(PrevActs),			
			%weka:new("DataArff.txt",ets:lookup_element(VarGlob, globalOptPrograms,2),MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),
			%		 ets:lookup_element(VarGlob, bestProgramRatio, 2),KeyR,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobRuleSelected+0.0,CobNegRuleSelected+0.0,length(PrevActs),Q),
			
			
			weka:new(ets:lookup_element(VarGlob, policyPrevFile, 2),ets:lookup_element(VarGlob, globalOptRules,2),MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),
					 ets:lookup_element(VarGlob, bestProgramRatio, 2),0,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobPosRuleSelected+0.0,CobNegRuleSelected+0.0,length(PrevActs),
							 Vars,Cons,Funcs,Strucs,Recs,Q),
			
			
			%case ets:last(ets:lookup_element(VarGlob, policyWEKA, 2)) of 
			%	'$end_of_table' ->
			%				ets:insert(ets:lookup_element(VarGlob, policyWEKA, 2), {1,{ets:lookup_element(VarGlob, globalOptRules,2),ets:lookup_element(VarGlob, globalOptPrograms,2),
			%												MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),ets:lookup_element(VarGlob, bestProgramRatio, 2),
			%												KeyR,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobPosRuleSelected+0.0,CobNegRuleSelected+0.0,length(PrevActs),Q}});
			%	_ ->
			%				ets:insert(ets:lookup_element(VarGlob, policyWEKA, 2), {ets:last(ets:lookup_element(VarGlob, policyWEKA, 2))+1,{ets:lookup_element(VarGlob, globalOptRules,2),
			%												ets:lookup_element(VarGlob, globalOptPrograms,2),MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),
			%												ets:lookup_element(VarGlob, bestProgramRatio, 2),KeyR,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobPosRuleSelected+0.0,
			%												CobNegRuleSelected+0.0,length(PrevActs),Q}})
			%end,
			
			%io:format("Step: ~w, | UNION | Action: (~w,~w), NO Addtion, Union: (~w,~w), ProgramUnion: ~w, OptUnion: ~w, Opt Before: ~w, Opt After: ~w ~n",[Step,BestAct,BestRule,P1u,P2u,IdProgU,OptUni,ets:lookup_element(VarGlob, globalOptPrograms, 2),GlobalOptNew2]),
			%programs:add2Matrix(ets:lookup_element(VarGlob, matrixPrograms, 2), ets:lookup_element(VarGlob, programs, 2),NumPos),
			
			
			ets:update_element(VarGlob, globalOptPrograms, {2,GlobalOptNew2}),
			ets:update_element(VarGlob, globalOptRules, {2,GlobalOptRules}),		 
		 
			%ets:update_element(VarGlob, globalOptPrograms, {2,GlobalOptNew2-GlobalOptPAnt}),
			%ets:update_element(VarGlob, globalOptRules, {2,GlobalOptRules-GlobalOptRAnt}),		 
		 
	
			
			%io:format("~p~n",[ets:match(MatrixPrograms,'$1')]),
			%io:format("Uniones: ~w~n",[NewUnions2]),
			
			case IndexNewRules > 1 of
				
				true ->
					flipRulesPrograms(VarGlob, NewruleS, RowRule, RowAction, BestAct, BestRule, Debug, Step, IndexNewRules-1);
					
				false ->
					metaflip(VarGlob, Debug, Step-1)
					
			end
			
					
	end.
	




flipRules(VarGlob, NewruleS, RowRule, RowAction, BestAct, BestRule, Debug, Step, IndexNewRules)->
	
	ets:update_element(VarGlob, stepAct, {2,Step}),
	
	[{KeyR,{SelecRule,CobRuleSelected,PrevActs,CobPosRuleSelected,RulesPosRuleSelected,CobNegRuleSelected,RulesNegSelected,
			SizeSelected,VarsSelected,ConsSelected,FuncSelected,StrucSelected,RecSelected,MMLSelected,MMLCovSelected}}]=RowRule,
	[{KeyA,{NameOp,ArityOp,CobAnt,Veces}}]=RowAction,
	
	NumPos=ets:lookup_element(VarGlob, numPos, 2),
	NumNeg= ets:lookup_element(VarGlob, numNeg, 2),
	Newrule=lists:nth(IndexNewRules, NewruleS),
	
	SelectedRule=lists:flatten(Newrule++"."),
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,_,_,Ari,_}=Forms,
	
	DuplicityVars=false,
	
	case util:already_exists(DuplicityVars,Newrule,ets:lookup_element(VarGlob, tablaReglas, 2),ets:last(ets:lookup_element(VarGlob, tablaReglas, 2))+1) of
					
		false ->
							
			
			{Cob,PosCob,CobNeg,NegCov}=util:cobertura_Funcional(VarGlob, Newrule),
			NewKeyRule=ets:last(ets:lookup_element(VarGlob, tablaReglas, 2))+1,
			
			%%% Optimalidad Reglas %%%
			
			case Debug of 
				1 ->			
					io:format("CobPos: ~p CobNeg: ~p ~n",[Cob,CobNeg]);
				0 ->
					ok
			end,			
			
			%%% Insercion nueva regla %%%
		
			{_,_,Vars,Cons,Funcs,Strucs,Recs}=sizeRules:sizeOfRuleDecomposed(Newrule),
			
			%OptRule= sizeRules:optRules(VarGlob, PrevActs, Cob, CobNeg, SizeRule),
			OptRule=mml:getOptMML(VarGlob, PosCob, NegCov, Newrule, length(PrevActs)+1),
			%OptRule=(0.4*(Cob/NumPos)-0.3*(CobNeg/NumNeg)-0.1*OPtOpers-0.2*(1/NumPos))/(0.4),
			FoilGain=OptRule,
			case OptRule > ets:lookup_element(VarGlob, bestRuleRatio, 2) of
				
				true ->
					ets:update_element(VarGlob, bestRuleRatio, {2,OptRule}),
					ets:update_element(VarGlob, bestRuleId, {2,NewKeyRule});
				
			
				_ ->
					ok
			end,	
			SizeRule=mml:getMML(VarGlob, Newrule),
			MMLCov=mml:getCovMML(VarGlob, PosCob, NegCov, Newrule),
			
			ets:insert(ets:lookup_element(VarGlob, tablaReglas, 2), {NewKeyRule,{Newrule,OptRule,[KeyA|PrevActs],Cob,PosCob,CobNeg,NegCov,SizeRule,Vars,Cons,Funcs,Strucs,Recs,SizeRule,MMLCov}}),
			ets:insert(ets:lookup_element(VarGlob, tablaReglasStep, 2), {NewKeyRule,ets:lookup_element(VarGlob, steps, 2)-Step}),
						
						
			%% Generacion de programas (1º Regla unitaria nueva -> programa unitario (Cob>0); 2º Combiancion de programas %%
		
			%%% Optimalidad Global %%%

			GlobalOptNewRules=programs:global_optimalityRules(ets:lookup_element(VarGlob, tablaReglas, 2)),
			NewListsOptGlobalRules=ets:lookup_element(VarGlob, listOptGlobal, 2)++[GlobalOptNewRules],
			ets:update_element(VarGlob, listOptGlobal, {2,NewListsOptGlobalRules}),
			GlobalRAnt=ets:lookup_element(VarGlob, globalOptRules, 2),
		
			%%% Q=calcularQ %%%
					
			MedRules=programs:mediumSizeRules(ets:lookup_element(VarGlob, tablaReglas, 2)),
			MedPrograms=programs:mediumSizePrograms(ets:lookup_element(VarGlob, programs, 2)),
			{Q,A,R,ClassMax}=reinforcement:calcularQ(VarGlob,FoilGain,GlobalOptNewRules,MedRules,MedPrograms,NumPos),
			case Debug of 
				1 ->
					io:format("Q prev: ~w --> Q new: ~w ---- argmaxQ -> (Op:~w, R:~w)=~w ~n",[ets:lookup_element(VarGlob, q, 2),Q,A,R,ClassMax]);
					%io:format("FoilGain Rule: ~w, OptOpers: ~w, OptRule: ~w ~n",[FoilGain,OPtOpers,OptRule]);
				0 ->
					ok
			end,
			
			%%% Weka %%%
			%ets:insert(ets:lookup_element(VarGlob, tablaWeka, 2), {{PrevActs,KeyA,KeyR},{ets:lookup_element(VarGlob, globalOptRules,2),SizeRule,Ari,Cob/NumPos,Q}}),	

			{Prev1,Prev2,Prev3}=weka:givemePrevOps(PrevActs),
			
			
			weka:new(ets:lookup_element(VarGlob, policyPrevFile, 2),ets:lookup_element(VarGlob, globalOptRules,2),MedRules,MedPrograms,
							 ets:lookup_element(VarGlob, bestRuleRatio, 2),ets:lookup_element(VarGlob, bestProgramRatio, 2),
					 0,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobPosRuleSelected+0.0,CobNegRuleSelected+0.0,length(PrevActs),Vars,Cons,Funcs,Strucs,Recs,Q),
			
			
			
			%case ets:last(ets:lookup_element(VarGlob, policyWEKA, 2)) of 
			%	'$end_of_table' ->
			%				ets:insert(ets:lookup_element(VarGlob, policyWEKA, 2), {1,{ets:lookup_element(VarGlob, globalOptRules,2),ets:lookup_element(VarGlob, globalOptPrograms,2),
			%							MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),ets:lookup_element(VarGlob, bestProgramRatio, 2),
			%							KeyR,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobPosRuleSelected+0.0,CobNegRuleSelected+0.0,length(PrevActs),Q}});
			%	_ ->
			%				ets:insert(ets:lookup_element(VarGlob, policyWEKA, 2), {ets:last(ets:lookup_element(VarGlob, policyWEKA, 2))+1,{ets:lookup_element(VarGlob, globalOptRules,2),
			%							ets:lookup_element(VarGlob, globalOptPrograms,2),MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),
			%							ets:lookup_element(VarGlob, bestProgramRatio, 2),KeyR,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobPosRuleSelected+0.0,CobNegRuleSelected+0.0,length(PrevActs),Q}})
			%end,
	
			%%Fin Step%%			
			
			case Debug of 
				1 ->
					io:format(" Weka Instance: State(GlobOpt): ~p, Rule: ~p, Size: ~p, Arity: ~p, Operator: ~p, CobRule: ~p, Q(Class):~p ~n",[ets:lookup_element(VarGlob, globalOptRules,2),KeyR,SizeRule,Ari,KeyA,Cob,Q]);
				0 ->
					ok
			end,
			
			GlobalRAnt=ets:lookup_element(VarGlob, globalOptRules, 2),
			%ets:update_element(VarGlob, globalOptRules, {2,GlobalOptNewRules-GlobalRAnt}),
			ets:update_element(VarGlob, globalOptRules, {2,GlobalOptNewRules}),
			
			case IndexNewRules > 1 of
				
				true ->
					flipRulesPrograms(VarGlob, NewruleS, RowRule, RowAction, BestAct, BestRule, Debug, Step, IndexNewRules-1);
					%flipRulesPrograms(NewRules, TablaReglas,TablaReglasStep, InstanciasN, InstanciasP, KeyA, KeyR,  NumPos, PrevActs, ArityOp, Veces, CobAnt, Actions, NameOp, Programs, MatrixPrograms, Unions, GlobalOpt, BestAct, BestRule, Steps, NewListActions2, OperatorsModule, LastRule, NewListsOptGlobal, Inf_Prev, InfPrevETS, Hasta, IndexNewRules-1);

				false ->
					
					metaflip(VarGlob, Debug, Step-1)
					%metaFlip(InstanciasP, InstanciasN, TablaReglas, LastRule, TablaReglasStep,Actions, NewListActions2,Programs,MatrixPrograms,NewUnions2,OperatorsModule, NewListsOptGlobal, Inf_Prev, InfPrevETS, 1,Hasta,Steps-1)	
					%metaFlip(InstanciasP, InstanciasN, TablaReglas, LastRule, Actions, NewListActions2,Programs,NewUnions2,Hasta,Steps-1);	
			end;
				
		true ->
			case Debug of 
				1 ->			
					io:format("Nueva Regla ya existente en ETS reglas~n ",[]);
				0 ->
					ok
			end,		
			
			%% Generacion fichero .arff para Weka %%
			
			
			{_,_,Vars,Cons,Funcs,Strucs,Recs}=sizeRules:sizeOfRuleDecomposed(Newrule),
			
						
			%%% Optimalidad Global %%%

			GlobalOptNewRules=programs:global_optimalityRules(ets:lookup_element(VarGlob, tablaReglas, 2)),
			NewListsOptGlobalRules=ets:lookup_element(VarGlob, listOptGlobal, 2)++[GlobalOptNewRules],
			ets:update_element(VarGlob, listOptGlobal, {2,NewListsOptGlobalRules}),	
		
			{Cob,PosCob,CobNeg,NegCov}=util:cobertura_Funcional(VarGlob, Newrule),
			%OptRule= sizeRules:optRules(VarGlob, PrevActs, Cob, CobNeg, SizeRule ),
			OptRule=mml:getOptMML(VarGlob, PosCob, NegCov, Newrule, length(PrevActs)+1),
			GlobalRAnt=ets:lookup_element(VarGlob, globalOptRules, 2),
			%%%% Q=calcularQ %%%			
			
			MedRules=programs:mediumSizeRules(ets:lookup_element(VarGlob, tablaReglas, 2)),
			MedPrograms=programs:mediumSizePrograms(ets:lookup_element(VarGlob, programs, 2)),
			{Q,A,R,ClassMax}=reinforcement:calcularQ(VarGlob, OptRule ,GlobalOptNewRules,MedRules,MedPrograms,NumPos),
			
			case Debug of 
				1 ->
					io:format("Q prev: ~w --> Q new: ~w ---- argmaxQ -> (Op:~w, R:~w)=~w ~n",[ets:lookup_element(VarGlob, q, 2),Q,A,R,ClassMax]);
				0 ->
					ok
			end,


							
			%%% Weka %%%
			
			{Prev1,Prev2,Prev3}=weka:givemePrevOps(PrevActs),
			
			%weka:new("DataArff.txt",ets:lookup_element(VarGlob, globalOptRules,2),MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),ets:lookup_element(VarGlob, bestProgramRatio, 2),
			%		 KeyR,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobRuleSelected+0.0,CobNegRuleSelected+0.0,length(PrevActs),Q),
			
			SizeRule=mml:getMML(VarGlob, Newrule),
			
			weka:new(ets:lookup_element(VarGlob, policyPrevFile, 2),ets:lookup_element(VarGlob, globalOptRules,2),MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),ets:lookup_element(VarGlob, bestProgramRatio, 2),
					 0,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobPosRuleSelected+0.0,CobNegRuleSelected+0.0,length(PrevActs),Vars,Cons,Funcs,Strucs,Recs,Q),
		
			
			%case ets:last(ets:lookup_element(VarGlob, policyWEKA, 2)) of 
			%	'$end_of_table' ->
			%				ets:insert(ets:lookup_element(VarGlob, policyWEKA, 2), {1,{ets:lookup_element(VarGlob, globalOptRules,2),ets:lookup_element(VarGlob, globalOptPrograms,2),
			%							MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),ets:lookup_element(VarGlob, bestProgramRatio, 2),
			%							KeyR,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobPosRuleSelected+0.0,CobNegRuleSelected+0.0,length(PrevActs),Q}});
			%	_ ->
			%				ets:insert(ets:lookup_element(VarGlob, policyWEKA, 2), {ets:last(ets:lookup_element(VarGlob, policyWEKA, 2))+1,{ets:lookup_element(VarGlob, globalOptRules,2),
			%							ets:lookup_element(VarGlob, globalOptPrograms,2),MedRules,MedPrograms,ets:lookup_element(VarGlob, bestRuleRatio, 2),
			%							ets:lookup_element(VarGlob, bestProgramRatio, 2),KeyR,SizeRule,Ari,KeyA,Prev1,Prev2,Prev3,CobPosRuleSelected+0.0,CobNegRuleSelected+0.0,length(PrevActs),Q}})
			%end,


			case Debug of 
				1 ->
					io:format("FoilGain Rule: ~w, OptOpers: ~w, OptRule: ~w ~n",[0,0,0]),
					io:format("Weka Instance: State(GlobOpt): ~p, Rule: ~p, Size: ~p, Arity: ~p, Operator: ~p, CobRule: ~p, Q(Class):~p ~n",[ets:lookup_element(VarGlob, globalOptRules,2),KeyR,SizeRule,Ari,KeyA,Cob,Q]);
				0 ->
					ok
			end,
			
			%GlobalRAnt=ets:lookup_element(VarGlob, globalOptRules, 2),
			ets:update_element(VarGlob, globalOptRules, {2,GlobalOptNewRules}),
			
			
			case IndexNewRules > 1 of
				
				true ->
					flipRulesPrograms(VarGlob, NewruleS, RowRule, RowAction, BestAct, BestRule, Debug, Step, IndexNewRules-1);
					%flipRulesPrograms(NewRules, TablaReglas, TablaReglasStep, InstanciasN, InstanciasP, KeyA, KeyR,  NumPos, PrevActs, ArityOp, Veces, CobAnt, Actions, NameOp, Programs, MatrixPrograms, Unions, GlobalOpt, BestAct, BestRule, Steps, NewListActions2, OperatorsModule, LastRule, NewListsOptGlobal, Inf_Prev, InfPrevETS, Hasta, IndexNewRules-1);
				
				false ->
					metaflip(VarGlob, Debug, Step-1)
					%metaFlip(InstanciasP, InstanciasN, TablaReglas, LastRule, TablaReglasStep, Actions, NewListActions2,Programs,MatrixPrograms,NewUnions2,OperatorsModule, NewListsOptGlobal,Inf_Prev, InfPrevETS, 1,Hasta,Steps-1)	
			
			end
			
		
			
	end.
























