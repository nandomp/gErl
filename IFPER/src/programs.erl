-module(programs).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%     GENERATE UNITARY PROGRAMS WITH E+       %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_programs_ini(ProgramsStep,TableReglas,Programs,NumPos,NumNeg)->
	generate_programs_ini(ProgramsStep,TableReglas,Programs,NumPos,NumNeg,1).
	
generate_programs_ini(ProgramsStep,TableReglas,Programs,NumPos,NumNeg,Index)->
			
	End=ets:last(TableReglas),
	case Index == End+1 of 
		false ->
			[{KeyR,{_,_,PrevActs,CobPos,_,CobNeg,_,SizeR,_,_,_,_,_,_,_}}]=ets:lookup(TableReglas, Index),	
			
			OPtOpers= 0.0,
			NumOpers= 0,
			%OptProgram=(0.4*(CobPos)-0.3*(CobNeg)-0.1*OPtOpers-0.2*(1))/(0.4*NumPos),
			%OptProgram=(0.4*(CobPos/NumPos)-0.3*(CobNeg/NumNeg)-0.1*OPtOpers-0.2*(1/NumPos))/(0.4),

			
			%OptProgram=(1*(CobPos/NumPos)-0.4*(CobNeg/NumNeg)-0.2*OPtOpers-0.4*(1/NumPos)+0.1*SizeR),
			OptProgram=(1*((CobPos/NumPos)-(CobNeg/NumNeg))-0.2*OPtOpers+0.1*SizeR),
					
			%io:format("KeyR: ~w~n",[KeyR]),
			ets:insert(Programs, {Index,{sets:from_list([KeyR]),sets:from_list([KeyR]),sets:from_list([]),OptProgram,NumOpers}}),
			ets:insert(ProgramsStep, {Index,0}),
						
			%ets:insert(Programs, {Index,{[KeyR],[KeyR],Cob-(math:log(1))}}),
			%io:format("Programas: ~w~n",[KeyR]),
			%io:format("~p~n",[ets:match(Programs,'$1')]),
			%io:format("-----------------------------------------------~n"),
			generate_programs_ini(ProgramsStep,TableReglas,Programs,NumPos,NumNeg,Index+1);
		true ->
			ok
	end.	

			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%    GENERATE UNITARY PROGRAM WITH NEW RULE   %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

newrule2programs(KeyR,Cov,Pos_Cov,CovNeg,Neg_cov,PrevActs,VarGlob)->
	
	
	
	%NumPos=ets:lookup_element(VarGlob, numPos, 2),
	%NumNeg=ets:lookup_element(VarGlob, numNeg, 2),
	Programs= ets:lookup_element(VarGlob, programs, 2),
	
	%OPtOpers = (length(PrevActs)+1)/(1*ets:lookup_element(VarGlob, numOperators, 2)),
	%OptProg=(0.4*(Cov/NumPos)-0.3*(CovNeg/NumNeg)-0.1*(OPtOpers)-0.2*(1/NumPos))/(0.4),
	%OptProg=(1*(Cov/NumPos)-0.4*(CovNeg/NumNeg)-0.2*(OPtOpers)-0.4*(1/NumPos)),
	
	OptProg=sizeRules:optPrograms(VarGlob, Cov, CovNeg, (length(PrevActs)+1), 1,[KeyR]),
	
	%ets:insert(Programs, {ets:last(Programs)+1,{sets:from_list([KeyR]),sets:from_list(Pos_Cov),1+(length(Pos_Cov)/NumPos)-1/NumPos}}).
	ets:insert(Programs, {ets:last(Programs)+1,{sets:from_list([KeyR]),sets:from_list(Pos_Cov),sets:from_list(Neg_cov),OptProg,length(PrevActs)+1}}),
	ets:insert(ets:lookup_element(VarGlob, programsStep, 2), {ets:last(Programs),ets:lookup_element(VarGlob, steps, 2)-ets:lookup_element(VarGlob, stepAct, 2)}),
	
	case OptProg > ets:lookup_element(VarGlob, bestProgramRatio, 2) of
				
				true ->
					ets:update_element(VarGlob, bestProgramRatio, {2,OptProg});
			
				_Else ->
					ok
			end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%        COMBINER UNION (ALL WITH ALL)        %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



					
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%     COMBINER UNION (TWO BEST OR RANDOM)     %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% 2 ALEATORY PROGRAMS %%

randomProgram(Last)->
	P1=random:uniform(Last),
	P2=random:uniform(Last),
	case P1=/=P2 of
		true ->
			{P1,P2};
		false ->
			randomProgram(Last)
	end.

%% GENERATE NEW PROGRAM WITH 2 ALEATORY PROGRAMS %%
union_programs_Random(Programs,CardPos,Used)->
	
	{P1,P2}=randomProgram(ets:last(Programs)),
	case P1=/=P2 of
		true ->
			%io:format("Select RAndom (~w,~w)~n",[P1,P2]),
			NewUsed=generation_new_program(Programs,CardPos,Used,P1,P2),
			case Used==NewUsed  of
				true ->
					union_programs_Random(Programs,CardPos,Used);
				false ->
					NewUsed
			end;
		false ->
			%io:format("Repetidos Random~n",[]),
			union_programs_Random(Programs,CardPos,Used)
	end.
	

%% SELECT 2 BEST PROGRAMS %%
select_bests(Programs,P1,Opt1,P2,Index)->
	
	End=ets:last(Programs),
	case Index== End+1 of
		false->
			%[{KeyAnt,{RulesAnt,PosCovAnt,NewOptAnt}}]=ets:lookup(Programs, Index),
			[{KeyNew,{_,_,NewOpt1}}]=ets:lookup(Programs, Index),
			%io:format("Rules: ~w~n",[Rules1]),
			%io:format("Pos: ~w~n",[PosCov1]),
			case (NewOpt1>Opt1) of %and (length(Rules1)<length(PosCov1)) of
				true->
					select_bests(Programs,KeyNew,NewOpt1,P1,Index+1);
				false ->
					select_bests(Programs,P1,Opt1,P2,Index+1)
			end;
		true ->
			
			{P1,P2}
	end.

%% GENERATE NEW PROGRAM WITH THE 2 BEST PROGRAMS OR 1) IF ARE THE SAME ID -> RANDOM PROGRAMS, 2) IF IS ALREADY USED -> RANDOM PROGRAMS %%  

union_programs_selectBest(Programs,CardPos,Used)->
	
	{P1,P2}=select_bests(Programs,1,0,1,1),
	%io:format("--->Select Best (~w,~w)~n",[P1,P2]),
	case P1=/=P2 of
		true ->
			%io:format("Select Best ~n",[P1,P2]),
			%io:format("Used ~p~n",[Used]),
			NewUsed=generation_new_program(Programs,CardPos,Used,P1,P2);
												  
		false ->
			%io:format("Select Random (Son Igulaes los Best)~n",[]),
			%io:format("Used ~p~n",[Used]),
			NewUsed=union_programs_Random(Programs,CardPos,Used)
	end,
	
	%solo entrera si BEST SELECTED
	case Used==NewUsed of
		true->
			%io:format("Ya usado Programs (~w,~w)~n",[P1,P2]),
			union_programs_Random(Programs,CardPos,Used);
		false ->
			%io:format("Programs (~w,~w)~n",[P1,P2]),
			NewUsed
	end.
				


%% GENERATE NEW PROGRAM WITH THE 2 BEST PROGRAMS %%  
union_programs_selectBest_WithoutRandom(Programs,CardPos,Used)->
	
	{P1,P2}=select_bests(Programs,1,0,1,1),
	%io:format("--->Select Best Programs (~w,~w)~n",[P1,P2]),
	case P1=/=P2 of
		true ->
			%io:format("Select Best ~n",[P1,P2]),
			%io:format("Used ~p~n",[Used]),
			{P1,P2,generation_new_program(Programs,CardPos,Used,P1,P2)};
												  
		false ->
			{P1,P2,Used}
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
select_bests_notused_pos(Programs,P1,Opt1,P2,Used,Index)->
	
	End=ets:last(Programs),
	case Index== End+1 of
		false->
			
			
			[{KeyNew,{RulesIndex,PosCovIndex,NewOptIndex}}]=ets:lookup(Programs, Index),
			
			case gb_sets:is_element({KeyNew,P1},Used) of
				false ->
					
					[{Key1,{Rules1,PosCov1,NewOpt1}}]=ets:lookup(Programs, P1),
					[{Key2,{Rules2,PosCov2,NewOpt2}}]=ets:lookup(Programs, P2),
					
					L1= sets:size(sets:union(PosCov1,PosCov2)),
					L2= sets:size(sets:union(PosCov1,PosCovIndex)),
					PosCovNew1=sets:union(PosCov1,PosCov2),
					PosCovNew2=sets:union(PosCov1,PosCovIndex),
					RulesNew1=sets:union(Rules1,Rules2),
					RulesNew2=sets:union(Rules1,RulesIndex),
					%io:format("~w, ~w, ~w, ~w --- ~w~n",[sets:to_list(PosCovNew1),sets:to_list(PosCovNew2),sets:to_list(RulesNew1),sets:to_list(RulesNew2),(sets:size(PosCovNew1)-sets:size(RulesNew1))/sets:size(PosCovNew1)]),
					Op1=(sets:size(PosCovNew1)-sets:size(RulesNew1))/sets:size(PosCovNew1),
					Op2=(sets:size(PosCovNew2)-sets:size(RulesNew2))/sets:size(PosCovNew2),
					
					%case (L2>L1)  and (Op1>=Op2) of
					case (Op1>=Op2) of
						true->
							select_bests_notused_pos(Programs,KeyNew,NewOptIndex,P1,Used,Index+1);

						false ->
							select_bests_notused_pos(Programs,P1,Opt1,P2,Used,Index+1)
					end;
				true ->
					select_bests_notused_pos(Programs,P1,Opt1,P2,Used,Index+1)					
			end;
		true ->
			
			{P1,P2}
	end.

select_bests_notused(Programs,P1,Opt1,P2,Used,Index)->
	
	End=ets:last(Programs),
	case Index== End+1 of
		false->
		
			[{KeyNew,{Rules1,PosCov1,NewOpt1}}]=ets:lookup(Programs, Index),
			case gb_sets:is_element({KeyNew,P1},Used) of
				false ->
					case (NewOpt1>=Opt1) of
						true->
							select_bests_notused(Programs,KeyNew,NewOpt1,P1,Used,Index+1);

						false ->
							select_bests_notused(Programs,P1,Opt1,P2,Used,Index+1)
					end;
				true ->
					select_bests_notused(Programs,P1,Opt1,P2,Used,Index+1)					
			end;
		true ->
			
			{P1,P2}
	end.

union_programs_ranking(Programs,CardPos,Used)->
	
	{P1,P2}=select_bests_notused_pos(Programs,1,0,1,Used,2),
	%io:format("--->Select Best Programs (~w,~w)~n",[P1,P2]),
	%io:format("Existe ~w ~n",[gb_sets:is_element({P1,P2},Used)]),
	case P1=/=P2 of
		true ->
			%io:format("Select Best ~n",[P1,P2]),
			%io:format("Used ~p~n",[Used]),
			
			{IdProg,NewUsed,Opt}=generation_new_program(Programs,CardPos,Used,P1,P2),
			{IdProg,P1,P2,NewUsed,Opt};
												  
		false ->
			IdProg=ets:last(Programs),
			{IdProg,P1,P2,Used,-200}
	end.
	
	
	
	
	
%% INSERT THE NEW PROGRAM INTO ETS %%
generation_new_program(Programs,CardPos,Used,P1,P2) when P1=/=P2->	
	
	%io:format("UNION~n",[]),
	%case lists:keyfind(P2,2,[lists:keyfind(P1,1,Used)]) of
		
	%	false ->
			%io:format("Union No usada (~w,~w) ",[P1,P2]),
			%io:format("Used ~p~n",[Used]),
			[{_,{Rules1,PosCov1,Opt1}}]=ets:lookup(Programs, P1),
		 	[{_,{Rules2,PosCov2,Opt2}}]=ets:lookup(Programs, P2),
  					
			%case Opt1>0 of
						
				%true->						
						%io:format("OPT1 > 0 ",[]),	
						%case Opt2>0 of 
						%	true ->
								%io:format("OPT2 > 0 ~n",[]),
								
								RulesNew=sets:union(Rules1,Rules2),
								PosCovNew=sets:union(PosCov1,PosCov2),	
								%io:format("Rules1: ~w, Rules2: ~w~n POsCov1: ~w, Poscov2: ~w~n RulesNew: ~w, PosCovNew: ~w~n",[Rules1,Rules2,PosCov1,PosCov2,RulesNew,PosCovNew]),	
  								%OptNew=(1/length(RulesNew))+(length(PosCovNew)/CardPos),
								%OptNew=(length(PosCovNew)/CardPos)-(math:log(length(RulesNew))),
								%OptNew=(length(PosCovNew))-math:log(length(RulesNew))/math:log(2)),
								%OptNew=length(PosCovNew)-length(RulesNew),
								OptNew=(sets:size(PosCovNew)-sets:size(RulesNew))/sets:size(PosCovNew),
								
									case ets:match(Programs,{'$1',{RulesNew,'$2','$3'}}) of
										[] ->
											ets:insert(Programs, {ets:last(Programs)+1,{RulesNew,PosCovNew,OptNew}}),
											%ets:insert(ets:lookup_element(VarGlob, programsStep, 2), {IndexNewP,ets:lookup_element(VarGlob, steps, 2)-ets:lookup_element(VarGlob, stepAct, 2)}),
											%io:format("Nuevo Programa: ~w, ~w, ~w, ~w  ~n",[ets:last(Programs)+1,RulesNew,PosCovNew,OptNew]),
											NewUsed=gb_sets:add_element({P1,P2},Used),			
											NewUsed2=gb_sets:add_element({P2,P1},NewUsed),
											{ets:last(Programs),NewUsed2,OptNew};
											
											
										Else ->
											%io:format("Nuevo Programa con mismas reglas~n",[]),
											NewUsed=gb_sets:add_element({P1,P2},Used),			
											NewUsed2=gb_sets:add_element({P2,P1},NewUsed),
											{ets:last(Programs),NewUsed2,-100}
											
									end.
										
						%	false ->
						%		Used
						%end;
									
  					%false ->
					%		Used
  				%end.
				
		%_Else ->
		%		io:format("Union Usada~n",[]),
		%		%io:format("Used ~p~n",[Used]),
		%		Used
	%end.
		
				
					
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%  GLOBAL OPTIMALLITY OF THE SET OF PROGRAMS  %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

  
 global_optimality(Programs)->
	Return=global_optimality2(Programs,0,1),
	Return.

global_optimality2(Programs,Global,Index)->
	
	End=ets:last(Programs),
	case Index == End+1 of
		false-> 
			[{_,{_,_,_,Opt_ind,_}}]=ets:lookup(Programs, Index),
			NewGlobal= Global + Opt_ind,
			global_optimality2(Programs,NewGlobal,Index+1);
		true ->
			Return=Global/End,
			Return
	end.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%      MEDIUM SIZE OF THE SET OF PROGRAMS     %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

  
 mediumSizePrograms(Programs)->
	End=ets:last(Programs),
	Return=mediumSizePrograms2(Programs,End,0,1),
	Return/End.

mediumSizePrograms2(Programs,End,Global,Index)->
	
	
	case Index == End+1 of
		false-> 
			[{_,{Rules,_,_,_,_}}]=ets:lookup(Programs, Index),			
			NewGlobal= Global + sets:size(Rules),
			%io:format("------NumRules: ~p Globlal: ~p NewGlobal: ~p ~n",[sets:size(Rules),Global,NewGlobal]),
			mediumSizePrograms2(Programs,End,NewGlobal,Index+1);
		true ->
			Global
			%io:format("------Global: ~p End: ~p Return: ~p ~n",[Global,End,Global/End])
			
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%  GLOBAL OPTIMALLITY OF THE SET OF RULES     %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

  
 global_optimalityRules(Rules)->
	Return=global_optimalityRules2(Rules,0,1),
	Return.

global_optimalityRules2(Rules,Global,Index)->
	
	End=ets:last(Rules),
	case Index == End+1 of
		false-> 
			[{_,{_,OptR,_,_,_,_,_,_,_,_,_,_,_,_,_}}]=ets:lookup(Rules, Index),
			NewGlobal= Global + OptR,
			global_optimalityRules2(Rules,NewGlobal,Index+1);
		true ->
			Return=Global/End,
			Return
	end.  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%       MEDIUM SIZE OF THE SET OF RULES       %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

  
 mediumSizeRules(Rules)->
	End=ets:last(Rules),
	Return=mediumSizeRules2(Rules,End,0,1),
	Return/End.

old_mediumSizeRules2(Rules,End,Global,Index)->
	
	
	case Index == End+1 of
		false-> 
			[{_,{_,_,_,_,_,_,_,Size,_,_,_,_,_,_,_}}]=ets:lookup(Rules, Index),
			NewGlobal= Global + Size,
			mediumSizeRules2(Rules,End,NewGlobal,Index+1);
		true ->
			Global
	end.    




mediumSizeRules2(Rules,End,Global,Index)->
	
	
	case Index == End+1 of
		false-> 
			[{_,{_,_,_,_,_,_,_,_,_,_,_,_,_,MML,_}}]=ets:lookup(Rules, Index),
			NewGlobal= Global + MML,
			mediumSizeRules2(Rules,End,NewGlobal,Index+1);
		true ->
			Global
	end.  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%     COMBINER ADDITION (NEW RULE & PROGRAM)  %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
  
  

addition(P1,Used,CardPos,Programs)->

	{P2,P3}=select_bests_notused(Programs,1,0,1,Used,1),
	%io:format("P1: ~w, P2: ~w,P3: ~w~n",[P1,P2,P3]),
	case P1 =/= P2 of
		true->
		%	io:format("Diferentes~n",[]),
			[{ID1,{Rules1,PosCov1,Opt1}}]=ets:lookup(Programs, P1),
			[{ID2,{Rules2,PosCov2,Opt2}}]=ets:lookup(Programs, P2);
		false ->
		%	io:format("Iguales~n",[]),
			[{ID1,{Rules1,PosCov1,Opt1}}]=ets:lookup(Programs, P1),
			[{ID2,{Rules2,PosCov2,Opt2}}]=ets:lookup(Programs, P3)
	end,
  	%io:format("ID1: ~w, ID2: ~w",[ID1,ID2]),
	%RulesNew=sets:to_list(sets:union(sets:from_list(Rules1), sets:from_list(Rules2))),
	%PosCovNew=sets:to_list(sets:union(sets:from_list(PosCov1), sets:from_list(PosCov2))),	
	RulesNew=sets:union(Rules1,Rules2),
	PosCovNew=sets:union(PosCov1,PosCov2),
	
  	%OptNew=(1/length(RulesNew))+(length(PosCovNew)/CardPos),
	%OptNew=(length(PosCovNew)/CardPos)-(math:log(length(RulesNew))),
	%OptNew=(length(PosCovNew))-(math:log(length(RulesNew))/math:log(2)),
	%OptNew=length(PosCovNew)-length(RulesNew),
	OptNew=(sets:size(PosCovNew)-sets:size(RulesNew))/sets:size(PosCovNew),
	%case ets:match(Programs,{'$1',{'$2',PosCovNew,'$3'}}) of
	%	[] ->
			ets:insert(Programs, {ets:last(Programs)+1,{RulesNew,PosCovNew,OptNew}}),
			%io:format("Nuevo Programa Add: ~w ~w ~n",[P1,P2]),
			NewUsed=gb_sets:add_element({ID1,ID2},Used),			
			NewUsed2=gb_sets:add_element({ID2,ID1},NewUsed),
			{ets:last(Programs),ID1,ID2,NewUsed2,OptNew}.
											
	%	_Else ->			
	%		{P1,P2,Used}
	%end.
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% MATRIX %%%%%

impEpos(Epos,0)->
	ok;
	
impEpos(Epos,Index)->
	case Index == length(Epos)+1 of
		false ->
			
			io:format("Index: ~p, Value: ~p ~n",[Index,sets:to_list(lists:nth(Index,Epos))]),
			impEpos(Epos,Index+1);
		true ->
			ok
	end.
	

gen_matrix_overlap(VarGlob,Programs,NumPos,NumNeg,MatrixPrograms)->	
	
	
	%io:format("~p~n",[ets:match(Programs,'$1')]),
	{Epos1,Epos2}=matrix_overlap_posEcovered(Programs,ets:last(Programs),[],[]),
	%{Opt1}=matrix_overlap_optProg(Programs,ets:last(Programs),[]),
	
	
	%UnionPos=[{I,J,((1*(sets:size(sets:union(lists:nth(I,Epos1),lists:nth(J,Epos1))))/NumPos)-
	%					0.3*((sets:size(sets:union(lists:nth(I,Epos2),lists:nth(J,Epos2))))/NumNeg)-
	%					0.1*((0))-0.3*(2/NumPos)-0.3*(1/NumPos))}||I<-lists:seq(1,length(Epos1)),J<-lists:seq(I+1,length(Epos1))],
	

	%UnionPos=[{I,J,(lists:nth(I,Opt1)+lists:nth(J,Opt1))/2}||I<-lists:seq(1,length(Opt1)),J<-lists:seq(I+1,length(Opt1))],
	
	NumOpers=ets:lookup_element(VarGlob, numOperators, 2),
	
	UnionPos=[{I,J,((1*(sets:size(sets:union(lists:nth(I,Epos1),lists:nth(J,Epos1))))/NumPos)-
						0.4*((sets:size(sets:union(lists:nth(I,Epos2),lists:nth(J,Epos2))))/NumNeg)-
						0.2*(0)-0.5*(2/NumPos)+0.1*(sizeRules:sizeProgLog([I,J], VarGlob)))}||I<-lists:seq(1,length(Epos1)),J<-lists:seq(I+1,length(Epos1))],
	
	%UnionPos=[{I,J,(1*(((sets:size(sets:union(lists:nth(I,Epos1),lists:nth(J,Epos1))))/NumPos)-
	%					((sets:size(sets:union(lists:nth(I,Epos2),lists:nth(J,Epos2))))/NumNeg))-
	%					0.2*(0)+0.1*(sizeRules:sizeProgLog([I,J], VarGlob)))}||I<-lists:seq(1,length(Epos1)),J<-lists:seq(I+1,length(Epos1))],
	
	
	%io:format("UnionPos: ~p~n",[UnionPos]),
	insert2MatrixPrograms(UnionPos,MatrixPrograms,1).


matrix_overlap_optProg(_,0,VecOpts)->
	{VecOpts};

matrix_overlap_optProg(Programs,Index,VecOpts)->
	
	[{_,{_,_,_,OptP,_}}]=ets:lookup(Programs, Index),
	TempA=[OptP|VecOpts],	
	matrix_overlap_optProg(Programs,Index-1,TempA).


matrix_overlap_posEcovered(_,0,VecPosCov,VecNegCov)->
	{VecPosCov,VecNegCov};
  
  
matrix_overlap_posEcovered(Programs,Index,VecPosCov,VecNegCov)->
	
	[{_,{_,PosCov,PosNeg,_,_}}]=ets:lookup(Programs, Index),
	TempA=[PosCov|VecPosCov],
	TempB=[PosNeg|VecNegCov],
	matrix_overlap_posEcovered(Programs,Index-1,TempA,TempB).


insert2MatrixPrograms(UnionPos,MatrixPrograms,Index)->
	

	End=length(UnionPos),
	case Index == End+1 of
		false ->
			{I,J,Inc}=lists:nth(Index, UnionPos),
			ets:insert(MatrixPrograms,{Index,{I,J,Inc}}),
			insert2MatrixPrograms(UnionPos,MatrixPrograms,Index+1);
		true->
			ok
	end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


maxMatrix_notUsed(Matrix,Used,Index,P1,P2,Inc)->
	%io:format("end: ~p~n",[ets:last(Matrix)]),
	End=ets:last(Matrix),
	
	case Index == End+1 of
		false ->
			[{Key,{P1new,P2new,Incnew}}]=ets:lookup(Matrix, Index),
			%io:format("Nth: ~w~n",[Nth]),
			
			case gb_sets:is_element({P1new,P2new},Used) of
				false ->
					case (Incnew>=Inc) of
						true->
							%io:format("Es miembro ~n",[]),
							maxMatrix_notUsed(Matrix,Used,Index+1,P1new,P2new,Incnew);

						false ->
							%io:format("Es miembro ~n",[]),
							maxMatrix_notUsed(Matrix,Used,Index+1,P1,P2,Inc)
					end;
					
				true ->
					maxMatrix_notUsed(Matrix,Used,Index+1,P1,P2,Inc)				
			end;
		
		
		true ->
			
			{Index,P1,P2,Inc}
	end.
			
maxMatrix_notUsed2(Matrix,Used)->
	
	A= ets:tab2list(Matrix),
	[H|T]= lists:filter(fun ({Idf,{ P1f, P2f, Inc}}) -> ((gb_sets:is_element({P1f,P2f},Used)==false) and (gb_sets:is_element({P2f,P1f},Used)== false)) end, A),
	{IdRet,{P1Ret,P2Ret,IncRet}}=lists:foldl(fun ({Id1,{P11,P21,Inc1}},{_,{_,_,Inc2}}) when Inc1 >= Inc2 -> {Id1,{P11,P21,Inc1}}; (_,{Id2,{P12,P22,Inc2}})->{Id2,{P12,P22,Inc2}} end, H, T),
	{IdRet,P1Ret,P2Ret,IncRet}.
			


add2Matrix(VarGlob) ->
	
	
	Matrix=ets:lookup_element(VarGlob, matrixPrograms, 2),
	Programs= ets:lookup_element(VarGlob, programs, 2),
	TRules= ets:lookup_element(VarGlob, tablaReglas, 2),
	NumPos= ets:lookup_element(VarGlob, numPos, 2),
	NumNeg= ets:lookup_element(VarGlob, numNeg, 2),
	NumOpers = ets:lookup_element(VarGlob, numOperators, 2),
	BestProgs = ets:lookup_element(VarGlob, bestProgs, 2),
	 
  	LastProg=ets:last(Programs),
	[{_,{Rules1,PosCov1,NegCov1,OptProg1,OpsApplied}}]=ets:lookup(Programs, LastProg),
	addMatrix2(VarGlob,NumPos,NumNeg,Programs,TRules,LastProg,Rules1,PosCov1,NegCov1,Matrix,OpsApplied,NumOpers,BestProgs, OptProg1,LastProg-1).

addMatrix2(_,_,_,_,_,_,_,_,_,_,_,_,_,_,0)->
	ok;

addMatrix2(VarGlob,NumPos,NumNeg,Programs,TRules,LastProg,Rules1,PosCovUlt,NegCovUlt,Matrix,OpsApplied,NumOpers,BestProgs,OptProg1,Index)->

	case sets:is_element(Index, BestProgs) of 
		
		false ->
		
				[{_,{Rules2,PosCovPrev,NegCovPrev,OptProg2,OpsAppliedPrev}}]=ets:lookup(Programs, Index),
	
				CardOpers=optOpersProgram(sets:to_list(sets:union(Rules1,Rules2)),TRules),
				%CardOpers=OpsAppliedPrev+OpsApplied,
				
				%Inc= (1*((sets:size(sets:union(PosCovUlt,PosCovPrev)))/(NumPos))-
				%		  0.3*((sets:size(sets:union(NegCovUlt,NegCovPrev)))/NumNeg)-
				%		  0.1*(CardOpers/(NumOpers*sets:size(sets:union(Rules1,Rules1))))-
				%		  0.3*((sets:size(sets:union(Rules1,Rules2)))/NumPos)-
				%		  0.3*(testCovP:instUsedAsProgs(sets:to_list(sets:union(Rules1,Rules1)), NumPos)/NumPos)),
				
				%Inc= (OptProg1 + OptProg2)/2,
				OPtOpers=(CardOpers/(NumOpers*sets:size(sets:union(Rules1,Rules2)))),
				
				%Pow=math:pow(2, OPtOpers)/NumOpers,
				SizeP= sizeRules:sizeProgLog(sets:to_list(sets:union(Rules1,Rules2)), VarGlob),
				Inc= (1*((sets:size(sets:union(PosCovUlt,PosCovPrev)))/(NumPos))-
						  0.4*((sets:size(sets:union(NegCovUlt,NegCovPrev)))/NumNeg)-
					  	  0.2*OPtOpers-
						  0.5*((sets:size(sets:union(Rules1,Rules2)))/NumPos)+
						  0.1*SizeP),
				
				%Inc= (1*((sets:size(sets:union(PosCovUlt,PosCovPrev)))/NumPos-
				%		  (sets:size(sets:union(NegCovUlt,NegCovPrev)))/NumNeg)-
				%	  	  0.2*OPtOpers+
				%		 % 0.5*((sets:size(sets:union(Rules1,Rules2)))/NumPos)+
				%		  0.1*SizeP),
				
	
				ets:insert(Matrix,{ets:last(Matrix)+1,{Index,LastProg,Inc}}),
		
				addMatrix2(VarGlob,NumPos,NumNeg,Programs,TRules,LastProg,Rules1,PosCovUlt,NegCovUlt,Matrix,OpsApplied,NumOpers, BestProgs,OptProg1,Index-1);
		
		true ->
				
				addMatrix2(VarGlob,NumPos,NumNeg,Programs,TRules,LastProg,Rules1,PosCovUlt,NegCovUlt,Matrix,OpsApplied,NumOpers, BestProgs,OptProg1,Index-1)
	end.
				




union_programs_matrix(VarGlob)->
	
	
	Programs=ets:lookup_element(VarGlob, programs, 2),
	CardPos=ets:lookup_element(VarGlob, numPos, 2),
	Used= ets:lookup_element(VarGlob, listUnions, 2),
	Matrix= ets:lookup_element(VarGlob, matrixPrograms, 2),
	
	%{_,P1,P2,Inc}=maxMatrix_notUsed(Matrix,Used,1,1,1,0),
	{_,P1,P2,Inc}=maxMatrix_notUsed2(Matrix,Used),
	
	%io:format("------------P1,P2 old: (~p,~p)-----P1,P2 new: (~p,~p)~n",[P1,P2,P1b,P2b]),
	
	{IsBestProg,IdProg,NewUsed,Opt}=generation_new_program_Matrix(VarGlob,P1,P2,Inc),
			
	{IsBestProg,IdProg,P1,P2,NewUsed,Opt}. 


%% INSERT THE NEW PROGRAM INTO ETS %%
generation_new_program_Matrix(VarGlob,P1,P2,Inc) ->	
	

		Programs=ets:lookup_element(VarGlob, programs, 2),
		CardPos=ets:lookup_element(VarGlob, numPos, 2),
		CardNeg=ets:lookup_element(VarGlob, numNeg, 2),
		Used= ets:lookup_element(VarGlob, listUnions, 2),	
					
		%io:format("Used ~p~n",[Used]),
		[{_,{Rules1,PosCov1,NegCov1,_,Opers1}}]=ets:lookup(Programs, P1),
		[{_,{Rules2,PosCov2,NegCov2,_,Opers2}}]=ets:lookup(Programs, P2),
  					
		RulesNew=sets:union(Rules1, Rules2),
		
		
  		%OptNew=(1/length(RulesNew))+(length(PosCovNew)/CardPos),
		%OptNew=(length(PosCovNew)/CardPos)-(math:log(length(RulesNew))),
		%OptNew=(length(PosCovNew))-math:log(length(RulesNew))/math:log(2)),
		%OptNew=(sets:size(PosCovNew)-sets:size(RulesNew))/sets:size(PosCovNew),

			case ets:match(Programs,{'$1',{RulesNew,'$2','$3','$4','$5'}}) of
					[] ->
						%PosCovNew=sets:union(PosCov1,PosCov2),
						%NegCovNew=sets:union(NegCov1,NegCov2),
						
						case (testCovP:isProgRec(P1, VarGlob)) or (testCovP:isProgRec(P1, VarGlob)) of
								true ->
										{CobPos,RulesPos,CobNeg,RulesNeg} = testCovP:cobFunc_Progs(VarGlob, RulesNew,2),
										RulesPosCov=sets:from_list(RulesPos),
										RulesNegCov=sets:from_list(RulesNeg);
		
								false ->
										RulesPosCov=sets:union(PosCov1,PosCov2),
										RulesNegCov=sets:union(NegCov1,NegCov2),
										CobPos= sets:size(RulesPosCov),
										CobNeg=sets:size(RulesNegCov)
						end,
						
						
						
						
						
						CardOpers=Opers1+Opers2,
						NumRulesNew=sets:size(RulesNew),
						OptProgram=sizeRules:optPrograms(VarGlob, CobPos, CobNeg, CardOpers, NumRulesNew, sets:to_list(RulesNew) ),
						
						%CardOpers=optOpersProgram(sets:to_list(RulesNew),ets:lookup_element(VarGlob, tablaReglas, 2)),
						%OPtOpers = CardOpers/(ets:lookup_element(VarGlob, numOperators, 2)*sets:size(RulesNew)),
						
						%OptProgram=(0.4*(CobPos/CardPos)-0.3*(CobNeg/CardNeg)-0.1*OPtOpers-0.2*(sets:size(RulesNew)/CardPos))/0.4, 
						%OptProgram=(1*(CobPos/CardPos)-0.4*(CobNeg/CardNeg)-0.2*OPtOpers-0.4*(sets:size(RulesNew)/CardPos)),
						%io:format("Nuevo Programa~n",[]),
						IndexNewP=ets:last(Programs)+1,
						ets:insert(Programs, {IndexNewP,{RulesNew,RulesPosCov,RulesNegCov,OptProgram,CardOpers}}),
						ets:insert(ets:lookup_element(VarGlob, programsStep, 2), {IndexNewP,ets:lookup_element(VarGlob, steps, 2)-ets:lookup_element(VarGlob, stepAct, 2)}),
						
						%io:format("Nuevo Programa: ~w, ~w, ~w, ~w  ~n",[ets:last(Programs)+1,RulesNew,PosCovNew,OptNew]),
						NewUsed=gb_sets:add_element({P1,P2},Used),			
						NewUsed2=gb_sets:add_element({P2,P1},NewUsed),
						case OptProgram > ets:lookup_element(VarGlob, bestProgramRatio, 2) of 
				
							true ->
								ets:update_element(VarGlob, bestProgramRatio, {2,OptProgram}),
								ets:update_element(VarGlob, bestProgramId, {2,IndexNewP});
			
							_Else ->
								ok
						end,
						case  (CobPos == CardPos) and (CobNeg == 0) of
							true ->
								NewBestProgs=sets:add_element(IndexNewP, ets:lookup_element(VarGlob, bestProgs, 2)),
								ets:update_element(VarGlob, bestProgs, {2,NewBestProgs}),
								{true,ets:last(Programs),NewUsed2,Inc};
							false ->
								{false,ets:last(Programs),NewUsed2,Inc}
						end;
										
											
					Else ->
						%io:format("Nuevo Programa con mismas reglas~n",[]),
						NewUsed=gb_sets:add_element({P1,P2},Used),			
						NewUsed2=gb_sets:add_element({P2,P1},NewUsed),
						{true,ets:last(Programs),NewUsed2,-100}
											
			end.


maxMatrix_addition_notUsed(Matrix,Used,LastProg,BestAdd,Inc,Index)->
	
	[{KeyMatrix,{P1new,P2new,Incnew}}]=ets:lookup(Matrix, Index),
	
	case (P1new=/=LastProg) and (P2new=/=LastProg)  of
		
		false ->
			
			case gb_sets:is_element({P1new,P2new},Used) of
				
				false ->
					
					case (Incnew>=Inc) of
						true->
							%io:format("Es miembro ~n",[]),
							maxMatrix_addition_notUsed(Matrix,Used,LastProg,KeyMatrix,Incnew,Index-1);

						false ->
							maxMatrix_addition_notUsed(Matrix,Used,LastProg,BestAdd,Inc,Index-1)
							
					end;
					
				true ->
					maxMatrix_addition_notUsed(Matrix,Used,LastProg,BestAdd,Inc,Index-1)			
			end;
		
		
		true ->
			
			{BestAdd}
	end.


maxMatrix_addition_notUsed2(Matrix,Used,LastProg)->
	
	A=ets:match(Matrix,{'$1',{LastProg,'$2','$3'}}) ++ ets:match(Matrix,{'$1',{'$2',LastProg,'$3'}}),
	
	%io:format("Vector previo MAp: ~p ~n",[A]),
	
	%lists:map(fun ([Id, P1, Inc1]) -> ((gb_sets:is_element({P1,1},Used)==false) and (gb_sets:is_element({1,P1},Used)== false)) end end , A).

	[H|T]= lists:filter(fun ([Id, P1, Inc1]) -> ((gb_sets:is_element({P1,LastProg},Used)==false) and (gb_sets:is_element({LastProg,P1},Used)== false)) end, A),
	
	%io:format("Vector post MAp: ~p ~n",[[H|T]]),
	
	[Id,P2,Inc]=lists:foldl(fun ([I1,P1,Inc1],[I2,P2,Inc2]) when Inc1 >= Inc2 -> [I1,P1,Inc1]; (_,[I2,P2,Inc2])-> [I2,P2,Inc2] end, H, T),
	
	{Id}.


			
addition_withMatrix(VarGlob)->

	LastProgram=ets:last(ets:lookup_element(VarGlob, programs, 2)),
	Programs=ets:lookup_element(VarGlob, programs, 2),
	Matrix=ets:lookup_element(VarGlob, matrixPrograms, 2),
	CardPos=ets:lookup_element(VarGlob, numPos, 2),
	CardNeg=ets:lookup_element(VarGlob, numNeg, 2),
	Used=ets:lookup_element(VarGlob, listUnions, 2),
	
	
	%{BestAdd}=maxMatrix_addition_notUsed(Matrix,Used,LastProgram,0,-100,ets:last(Matrix)),
	
	{BestAdd}= maxMatrix_addition_notUsed2(Matrix,Used,LastProgram),
	
	%io:format("------------------------------------------bestAddOLD-> ~p bestAdd -> ~p~n",[BestAdd,BestAdd2]),
	
	
	%io:format("Best: ~p ~n",[BestAdd]),
	%io:format("~p~n",[ets:match(Matrix,'$1')]),
	[{KeyMatrix,{P1,P2,Incnew}}]=ets:lookup(Matrix, BestAdd),
	%io:format("P1: ~w, P2: ~w,P3: ~w~n",[P1,P2,P3]),
	
	
	[{ID1,{Rules1,PosCov1,NegCov1,Opt1,Opers1}}]=ets:lookup(Programs, P1),
	[{ID2,{Rules2,PosCov2,NegCov2,Opt2,Opers2}}]=ets:lookup(Programs, P2),

  	%io:format("ID1: ~w, ID2: ~w",[ID1,ID2]),
	%RulesNew=sets:to_list(sets:union(sets:from_list(Rules1), sets:from_list(Rules2))),
	%PosCovNew=sets:to_list(sets:union(sets:from_list(PosCov1), sets:from_list(PosCov2))),	
	RulesNew=sets:union(Rules1,Rules2),
 
	case (testCovP:isProgRec(P1, VarGlob)) or (testCovP:isProgRec(P1, VarGlob)) of
		true ->
			{CobPos,RulesPos,CobNeg,RulesNeg} = testCovP:cobFunc_Progs(VarGlob, RulesNew,2),
			RulesPosCov=sets:from_list(RulesPos),
			RulesNegCov=sets:from_list(RulesNeg);
		
		false ->
			RulesPosCov=sets:union(PosCov1,PosCov2),
			RulesNegCov=sets:union(NegCov1,NegCov2),
			CobPos= sets:size(RulesPosCov),
			CobNeg=sets:size(RulesNegCov)
	end,
	
	
	
	CardOpers=(Opers1+Opers2),
	NumRulesNew= sets:size(RulesNew),
	OptProgram=sizeRules:optPrograms(VarGlob, CobPos, CobNeg, CardOpers, NumRulesNew, sets:to_list(RulesNew)),
	
	%OPtOpers = CardOpers/(ets:lookup_element(VarGlob, numOperators, 2)*sets:size(RulesNew)),
	
	%OptRule=(0.4*(Cob/NumPos)-0.3*(CobNeg/NumNeg)-0.1*OPtOpers-0.2*(1/NumPos))/(0.4),
	%OptProgram=(0.4*(CobPos/CardPos)-0.3*(CobNeg/CardNeg)-0.1*OPtOpers-0.2*(sets:size(RulesNew)/CardPos))/0.4, 
	%OptProgram=(1*(CobPos/CardPos)-0.4*(CobNeg/CardNeg)-0.2*OPtOpers-0.4*(sets:size(RulesNew)/CardPos)), 
	IndexNewP=ets:last(Programs)+1,
  	
	ets:insert(Programs, {IndexNewP,{RulesNew,RulesPosCov,RulesNegCov,OptProgram,CardOpers}}),
	ets:insert(ets:lookup_element(VarGlob, programsStep, 2), {IndexNewP,ets:lookup_element(VarGlob, steps, 2)-ets:lookup_element(VarGlob, stepAct, 2)}),
	
	case OptProgram > ets:lookup_element(VarGlob, bestProgramRatio, 2) of
				
				true ->
					ets:update_element(VarGlob, bestProgramRatio, {2,OptProgram}),
					ets:update_element(VarGlob, bestProgramId, {2,IndexNewP});
					
				
				_Else ->
					ok
			end,
	
	%io:format("Nuevo Programa Add: ~w ~w ~n",[P1,P2]),
	NewUsed=gb_sets:add_element({ID1,ID2},Used),			
	NewUsed2=gb_sets:add_element({ID2,ID1},NewUsed),
	
	case  (CobPos == CardPos) and (CobNeg == 0) of
		true ->
			%No permitimos que el programa se use mas (no es mejorable)
				NewBestProgs=sets:add_element(IndexNewP, ets:lookup_element(VarGlob, bestProgs, 2)),
				ets:update_element(VarGlob, bestProgs, {2,NewBestProgs}),
				{true,ets:last(Programs),ID1,ID2,NewUsed2,Incnew};
		false ->
				
				{false,ets:last(Programs),ID1,ID2,NewUsed2,Incnew}
	
	end.
											
	

optOpersProgram(Rules,TableRules)->	

	optOpersProgram2(Rules,TableRules,0,length(Rules)).

optOpersProgram2(_,_,Ops,0)->
	Ops;

optOpersProgram2(Rules,TableRules,Ops,Index)->
	
	%io:format("Rles: ~p~n",[Rules]),
	%io:format("-->: ~p~n",[ets:lookup(TableRules, lists:nth(Index, Rules))]),
	[{_,{_,_,PrevActs,_,_,_,_,_,_,_,_,_,_,_,_}}]=ets:lookup(TableRules, lists:nth(Index, Rules)),
	optOpersProgram2(Rules,TableRules,Ops+length(PrevActs),Index-1).

