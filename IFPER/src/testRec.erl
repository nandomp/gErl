-module(testRec).
-compile(export_all).

cobertura_Funcional(InstanciasP, InstanciasN,InstanciasPRec,InstanciasNRec,InstancesPosBase,InstancesNegBase, Newrule)->
	cobertura_Funcional(InstanciasP, InstanciasN,InstanciasPRec,InstanciasNRec,InstancesPosBase,InstancesNegBase, Newrule, false).

cobertura_Funcional(InstanciasP, InstanciasN,InstanciasPRec,InstanciasNRec,InstancesPosBase,InstancesNegBase, Newrule, PositiveConsistency)->
	
		%io:format("Rulecobertura: ~p~n",[Newrule]),
	Newrule1=lists:append(Newrule,"."),
	%io:format("Rule: ~p~n",[Newrule1]),
	io:format(" NEG \n",[]),
	{CobNeg,RulesNegCov}=cobertura_Funcional2(Newrule1,InstanciasN,InstanciasNRec,InstancesNegBase,0,[],ets:last(InstanciasN),PositiveConsistency),
	%io:format("----------> Neg= ~w ~n",[RulesNegCov]),
	io:format(" POS \n",[]),
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
	
	
			io:format("Rules transformed: ~p~n",[RulesTrans]),
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
					io:format("RecBase: ~p ~n",[NewRule2]),
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
					io:format("Code: ~p~n",[CodeFin]),
					
					{ok, P3} = smerl:add_func(P2,CodeFin),
					smerl:compile(P3),
					Sol=rule:cubre(),
					io:format("SOLUCION= ~w~n",[Sol]),
					case Sol of
						falseInf ->
							io:format("Entro en FALSEINF -> ~p ~n",[Cob]),
							%cobertura_Funcional3(P2,Newrule,InstanciasUso,Cob,Rules_Cov,Index-1);(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index)
							cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index-1,PositiveCons);
						falseOther -> %Positive Consistency
							io:format("Entro en FALSEOTHER -> ~p ~n",[Cob]),
							%cobertura_Funcional3(P2,Newrule,InstanciasUso,Cob,Rules_Cov,Index-1);(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index)
							case PositiveCons of
								true ->
									cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,[-1|Rules_Cov],Index-1,PositiveCons);
								false ->
									cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index-1,PositiveCons)
							end;
					
						falseMatch ->
							io:format("Entro en FALSEMATCH -> ~p ~n",[Cob]),
							%cobertura_Funcional3(P2,Newrule,InstanciasUso,Cob,Rules_Cov,Index-1);(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index)
							cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index-1,PositiveCons);

						Else ->
							io:format("Entro en ELSE:~p -> ~p ~n",[Else,Cob]),
							%cobertura_Funcional3(P2,Newrule,InstanciasUso,Cob+1,[Index|Rules_Cov],Index-1)
							cobertura_Funcional2(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob+1,[Index|Rules_Cov],Index-1,PositiveCons)

					end
		
			end
	end.
			

%%% Add the base cases (instances) to the rule tranasformed f(B) ==> {f(_,_,0) -> inf; f(A,B,Acc) -> f(A,B,Acc-1)} to calcule the extensional coverage %%%

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
			


%%% f(A,B) -> f(B) ==> {f(_,_,0) -> inf; f(A,B,Acc) -> f(A,B,Acc-1)} %%%

recRulesTransformation(Rule)->
	
	%SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(Rule),
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
		

%%% transform all instances adding an integer (widht counter) to them f(a,b) -> b ==> f(a,b,20) -> b %%%
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
	

%%% transform all instances to base cases adding the '_' : f(a,b)->b ==> f(a,b,'_')==>b

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



test()->
	
	InstanciasP= ets:new('InstanciasP',  [ordered_set] ),
	InstanciasN= ets:new('InstanciasN', [ordered_set] ),
	
	%ets:insert(InstanciasP, {1,{"member([a,v],a)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {2,{"member([b,v],b)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {3,{"member([b,a,v],b)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {4,{"member([a,a,v],a)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {5,{"member([a,b,v],b)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {6,{"member([b,a,b,v],b)->true",0.0,[]}}),
%	ets:insert(InstanciasP, {7,{"member([b,a,b,v],a)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {8,{"member([b,a,a,v],b)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {9,{"member([b,a,a,v],a)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {10,{"member([a,a,b,v],a)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {11,{"member([a,a,b,a,v],b)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {12,{"member([a,a,a,b,v],b)->true",0.0,[]}}),
	
	
	%ets:insert(InstanciasN, {1,{"member([a,a,v],b)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {2,{"member([v],a)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {3,{"member([a,v],b)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {4,{"member([b,v],a)->true",0.0,[]}}),	
	%ets:insert(InstanciasN, {5,{"member([b,b,v],a)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {6,{"member([v],b)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {7,{"member([b,b,b,v],a)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {8,{"member([a,a,a,v],b)->true",0.0,[]}}),

	
	
ets:insert(InstanciasP, {1,{"last([c])->c",0.0,[]}}),
	ets:insert(InstanciasP, {2,{"last([b])->b",0.0,[]}}),
	ets:insert(InstanciasP, {3,{"last([l])->l",0.0,[]}}),
	ets:insert(InstanciasP, {4,{"last([a,b,c])->c",0.0,[]}}),
	ets:insert(InstanciasP, {5,{"last([t,b,n,a,b])->b",0.0,[]}}),
	ets:insert(InstanciasP, {6,{"last([h,h,t,a,l])->l",0.0,[]}}),
	ets:insert(InstanciasP, {7,{"last([a,c,b])->b",0.0,[]}}),
	ets:insert(InstanciasP, {8,{"last([a,b,a,c])->c",0.0,[]}}),

	%ets:insert(InstanciasP, {1,{"member([a],a)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {2,{"member([b],b)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {3,{"member([f,a],f)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {4,{"member([k,k],k)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {5,{"member([u,h],h)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {6,{"member([i,g,i],i)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {7,{"member([r,j,e],j)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {8,{"member([l,a,a],l)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {9,{"member([x,n,n],n)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {10,{"member([g,g,c],g)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {11,{"member([t,y,b,g],b)->true",0.0,[]}}),
	%ets:insert(InstanciasP, {12,{"member([r,t,u,d],d)->true",0.0,[]}}),
	
	%ets:insert(InstanciasP, {1,{"member([a],a)->hola",0.0,[]}}),
%ets:insert(InstanciasP, {2,{"member([b],b)->true",0.0,[]}}),
%ets:insert(InstanciasP, {3,{"member([k],k)->true",0.0,[]}}),
%ets:insert(InstanciasP, {4,{"member([h],h)->true",0.0,[]}}),
%ets:insert(InstanciasP, {5,{"member([i],i)->true",0.0,[]}}),
%ets:insert(InstanciasP, {6,{"member([e],e)->true",0.0,[]}}),
%ets:insert(InstanciasP, {7,{"member([n],n)->true",0.0,[]}}),
%ets:insert(InstanciasP, {8,{"member([c],c)->true",0.0,[]}}),
%ets:insert(InstanciasP, {9,{"member([g],g)->true",0.0,[]}}),
%ets:insert(InstanciasP, {10,{"member([f],f)->true",0.0,[]}}),
%ets:insert(InstanciasP, {11,{"member([d],d)->true",0.0,[]}}),
%ets:insert(InstanciasP, {12,{"member([j],j)->true",0.0,[]}}),
%ets:insert(InstanciasP, {13,{"member([l],l)->true",0.0,[]}}),
%ets:insert(InstanciasP, {14,{"member([f,a],f)->true",0.0,[]}}),
%ets:insert(InstanciasP, {15,{"member([k,k],k)->true",0.0,[]}}),
%ets:insert(InstanciasP, {16,{"member([u,h],h)->true",0.0,[]}}),
%ets:insert(InstanciasP, {17,{"member([i,g,i],i)->true",0.0,[]}}),
%ets:insert(InstanciasP, {18,{"member([r,j,e],j)->true",0.0,[]}}),
%ets:insert(InstanciasP, {19,{"member([l,a,a],l)->true",0.0,[]}}),
%ets:insert(InstanciasP, {20,{"member([x,n,n],n)->true",0.0,[]}}),
%ets:insert(InstanciasP, {21,{"member([g,g,c],g)->true",0.0,[]}}),
%ets:insert(InstanciasP, {22,{"member([t,y,b,g],b)->true",0.0,[]}}),
%ets:insert(InstanciasP, {23,{"member([r,t,u,d],d)->true",0.0,[]}}),

	ets:insert(InstanciasN, {1,{"last([c])->b",0.0,[]}}),
	ets:insert(InstanciasN, {2,{"last([b])->l",0.0,[]}}),
	ets:insert(InstanciasN, {3,{"last([l])->c",0.0,[]}}),
	ets:insert(InstanciasN, {4,{"last([a,b,c])->a",0.0,[]}}),
	ets:insert(InstanciasN, {5,{"last([t,b,n,a,b])->t",0.0,[]}}),
	
	%ets:insert(InstanciasN, {1,{"member([g,g],j)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([],a)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([i],n)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([p],o)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([r,r],z)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([],e)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([t,t,t],s)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([k,k,k],y)->true",0.0,[]}}),
	
	%ets:insert(InstanciasN, {1,{"member([g,g],j)->true",0.0,[]}}),
%ets:insert(InstanciasN, {2,{"member([],a)->true",0.0,[]}}),
%ets:insert(InstanciasN, {3,{"member([i],n)->true",0.0,[]}}),
%ets:insert(InstanciasN, {4,{"member([p],o)->true",0.0,[]}}),
%ets:insert(InstanciasN, {5,{"member([r,r],z)->true",0.0,[]}}),
%ets:insert(InstanciasN, {6,{"member([],e)->true",0.0,[]}}),
%ets:insert(InstanciasN, {7,{"member([t,t,t],s)->true",0.0,[]}}),
%ets:insert(InstanciasN, {8,{"member([k,k,k],y)->true",0.0,[]}}),
	
	%NewRule="member([u|B],u)->member(B,VAR119986)",
	%NewRule="member([A],B)->true",
	NewRule="last([a|B])->last(B)",
	RecInstancesPos= ets:new('InstanciasPosRec', [ordered_set] ),
	RecInstancesNeg= ets:new('InstanciasNegRec', [ordered_set] ),
	RecInstancesPosBase= ets:new('InstanciasPosRecB', [ordered_set] ),
	RecInstancesNegBase = ets:new('InstanciasPosRecB', [ordered_set] ),
	
	recInstancesTransform(InstanciasP,RecInstancesPos),
	recInstancesTransform(InstanciasN,RecInstancesNeg),
	recInstancesBaseCases(InstanciasP,RecInstancesPosBase),
	recInstancesBaseCases(InstanciasN,RecInstancesNegBase),
	
	cobertura_Funcional(InstanciasP, InstanciasN,RecInstancesPos,RecInstancesNeg,RecInstancesPosBase,RecInstancesNegBase, NewRule).
	

	
instUsedAsProgs(Rules, NumPos)->	
		
		lists:foldl(fun (X, Acc) when X < (NumPos+1) -> Acc+1; (_,Acc) -> Acc end, 0, Rules).
		
	

		
		
testMatch()->
	Rules= ets:new('Rules', [ordered_set] ),
	
	%[{KeyR,{SelecRule,CobRuleSelected,PrevActs,CobPosRuleSelected,RulesPosRuleSelected,CobNegRuleSelected,RulesNegSelected,SizeSelected}}]=RowRule,

	 ets:insert(Rules,{12,{"last([A,b,b])->b",0.05238095238095238,[1],1,[6],0,[],1.0}}),
	 ets:insert(Rules,{13,{"last([A,b,a,a])->a",0.05238095238095238,[1],1,[7],0,[],1.0}}),
	 ets:insert(Rules, { 14,{"last([b|B])->a",-0.02761904761904764,[2],1,[3],1,[2],1.0}}),
	 ets:insert(Rules,{ 15,{"last([a|B])->b",0.0352380952380952,[2],2,[4,6],2,[1,5],1.0}}),
 	 ets:insert(Rules,{ 16,{"last([a|B])->a",0.33809523809523806,[2],3,[1,5,7],0,[],1.0}}),
	 ets:insert(Rules,{ 17,{"last([a])->last(a)",-0.09047619047619047,[3],0,[],0,[],10.0}}),
	 ets:insert(Rules,{ 18,{"last([b])->last(b)",-0.09047619047619047,[3],0,[],0,[],10.0}}),
	 ets:insert(Rules, {19,{"last([b,a])->last(b)",-0.09047619047619047,[3],0,[],0,[],10.0}}),

	%io:format("~p~n",[ets:match(Rules,'$1')]).

	%ets:match(Rules, {'$1',{'$2','$3','$4',0,'$5',0,'$6',10.0}}).
	ets:match(Rules, {'$1',{'_','_','_',0,'_',0,'_',1.0}}).
	



last([c],_)->c;
last([b],_)->b;
last([l],_)->l;
last([a,b,c],_)->c;
last([t,b,n,a,b],_)->b;
last([h,h,t,a,l],_)->l;
last([a,c,b],_)->b;
last(_,0)->inf;
%last([a|B],Acc)->io:format("~p",[Acc]),last(B,Acc-1).
last([a|B],Acc)->hola.
cubre()->
	try last([a,b,a,c],20) of 
		Val -> case Val==c of 
				   true -> true; 
				   false -> case Val==inf of 
								true -> falseInf; 
								false ->falseOther
							end
			   end 
	catch _:_ -> falseMatch 
	end.

playtennis(A,B,C,D) -> 
	case (A == overcast) of 
		true -> yes;
		false -> case ((A == rain) and (D == weak)) of
					 true -> yes;
					 false -> case ((A == sunny) and (C == normal)) of
								  true -> yes;
								  _else -> no
							  end
				 end
	end.
				





