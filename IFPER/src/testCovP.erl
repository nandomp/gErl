-module(testCovP).
-compile(export_all).

cobFunc_Progs(VarGlob,IndexP, 1)->
	
	InstanciasP=ets:lookup_element(VarGlob, instanciasP, 2),
	InstanciasN=ets:lookup_element(VarGlob, instanciasN, 2),
	InstanciasPRec=ets:lookup_element(VarGlob, instanciasPRec, 2),
	InstanciasNRec=ets:lookup_element(VarGlob, instanciasNRec, 2),
	TablaReglas=ets:lookup_element(VarGlob, tablaReglas, 2),
	Programs=ets:lookup_element(VarGlob, programs, 2),
	SortProg=sortProgramRules(Programs, TablaReglas, IndexP),
	cobFunc_Progs(InstanciasP, InstanciasN, InstanciasPRec, InstanciasNRec, TablaReglas,SortProg);

cobFunc_Progs(VarGlob, RulesProg, 2)->
	
	InstanciasP=ets:lookup_element(VarGlob, instanciasP, 2),
	InstanciasN=ets:lookup_element(VarGlob, instanciasN, 2),
	InstanciasPRec=ets:lookup_element(VarGlob, instanciasPRec, 2),
	InstanciasNRec=ets:lookup_element(VarGlob, instanciasNRec, 2),
	TablaReglas=ets:lookup_element(VarGlob, tablaReglas, 2),
	SortProg=sortProgramRules(RulesProg,TablaReglas),
	
	cobFunc_Progs(InstanciasP, InstanciasN, InstanciasPRec, InstanciasNRec, TablaReglas,SortProg).

cobFunc_Progs(InstanciasP, InstanciasN, InstanciasPRec, InstanciasNRec, TablaReglas,SortProg)->
	
	Prog=recRulesTransformation(TablaReglas,SortProg),
	%io:format("Prog-> ~p~n",[Prog]),
	
	{Prog1,_}= lists:split(length(Prog)-1,Prog),
	Prog3=lists:append(Prog1,"."),
	%io:format("Prog2-> ~p~n",[Prog2]),

	
	%io:format("Prog3-> ~p~n",[Prog3]),
	
	
	%Newrule1=lists:append(Newrule,"."),
	%io:format("Rule: ~p~n",[Newrule1]),
	%io:format(" NEG \n",[]),

	{CobNeg,RulesNegCov}=cobFunc_Progs2(Prog3,InstanciasN,InstanciasNRec,0,[],ets:last(InstanciasN)),
	%io:format("----------> Neg= ~w ~n",[CobNeg]),
	
	%io:format(" POS \n",[]),
	{CobPos,RulesPosCov}=cobFunc_Progs2(Prog3,InstanciasP,InstanciasPRec,0,[],ets:last(InstanciasP)),
	
	{CobPos,RulesPosCov,CobNeg,RulesNegCov}.
	
	

cobFunc_Progs2(Prog,Instancias,InstanciasRec,Cob,Rules_Cov,Index)->
	
	%io:format("Entro ~n)",[]),
	P1 = smerl:new(rule),
	%{ok, P2} = smerl:add_func(P1, Prog),
	
	
	case smerl:add_func(P1,Prog) of 
		{ok,P2}->
		
																		  
			case Index of
		
				0 ->
					{Cob,Rules_Cov};
			
				_else ->
			
					[{Index,{Regla,_,_}}]=ets:lookup(InstanciasRec, Index),
					Regla1=string:sub_word(Regla,1,$-),
					Rhs=string:sub_word(Regla,2,$>),			
					Code=lists:append("try ",Regla1),
					Code1=lists:append(Code, " of Val -> case Val=="),
					Code2=lists:append(Code1,Rhs),
					Code3=lists:append(Code2," of true -> true; false -> case Val==inf of true -> false; false ->false end end catch _:_ -> false end."),			
					CodeFin=lists:append("cubre()->",Code3),
					%io:format("Code: ~p~n",[CodeFin]),
			
					{ok, P3} = smerl:add_func(P2,CodeFin),
					smerl:compile(P3,[nowarn_unused_function]),
					Sol=rule:cubre(),
					%io:format("SOLUCION= ~w~n",[Sol]),
					case Sol of
						false ->
							%io:format("Entro en FALSE -> ~p ~n",[Cob]),
							%cobertura_Funcional3(P2,Newrule,InstanciasUso,Cob,Rules_Cov,Index-1);(Newrule,Instancias,InstanciasRec,InstanciasBase,Cob,Rules_Cov,Index)
							cobFunc_Progs2(Prog,Instancias,InstanciasRec,Cob,Rules_Cov,Index-1);

						Else ->
							%io:format("Entro en ELSE:~p -> ~p ~n",[Else,Cob]),
							%cobertura_Funcional3(P2,Newrule,InstanciasUso,Cob+1,[Index|Rules_Cov],Index-1)
							cobFunc_Progs2(Prog,Instancias,InstanciasRec,Cob+1,[Index|Rules_Cov],Index-1)

					end
		
			end;
		Error ->
			io:format("Error TestCovP: ~p ---  PROG: ~p~n Instancias: ~n~p~n Instancias REC: ~n~p~n",[Error,Prog,Instancias,InstanciasRec]),
			{0,[]}
	end.

	
	
	
	

recRulesTransformation(TablaReglas,SortProg)->
	
	recRulesTransformation2(TablaReglas,SortProg,"","",length(SortProg)).


recRulesTransformation2(_,_,Prog,Base,0)->
	
	%io:format("Base: ~p ~n, PRog: ~p ~n",[Base,Prog]),
	
	lists:append(Base,Prog);
	

	
recRulesTransformation2(TablaReglas,SortProg,Prog,Base,Index)->
	
	
	{IDRule,_}=lists:nth(Index, SortProg),
	[{IDRule,{Rule,_,_,_,_,_,_,_,_,_,_,_,_}}] = ets:lookup(TablaReglas, IDRule),
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(lists:flatten(SelectedRule)),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,Line,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	
	%% Comprobacion regla recursiva %%
	NewBody = util:addTailRecursionArgs(Body,Name),
	case NewBody == Body of
		true -> 
			%no recursividad f(a)->b ==> f(a,_)->b
			New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity+1,[{clause,1,Patterns++[{var,Line,'_'}],Guards,Body}]}])),
			%New2=re:replace(New, " ", "",[global,{return,list}]),
			New3=re:replace(New, "\n", "",[global,{return,list}]),
			New4=string:sub_word(New3,1,$.),
			New5=lists:append(New4,";"),
			recRulesTransformation2(TablaReglas,SortProg,New5++Prog,Base,Index-1);


		false ->
			
			New=erl_prettypr:format(erl_syntax:form_list([{function,Line,Name,Arity+1,[{clause,1,util:addTailRecursionPatt(Patterns),Guards,NewBody}]}])),
			%New2=re:replace(New, " ", "",[global,{return,list}]),
			New3=re:replace(New, "\n", "",[global,{return,list}]),
			New4=string:sub_word(New3,1,$.),
			New5=lists:append(New4,";"),
			
			
			
			B=util:getBase(Arity),
			%io:format("Base PRev: ~p~n",[B]),
			Base1=erl_prettypr:format(erl_syntax:form_list([{function,Line,Name,Arity+1,[{clause,1,B,Guards,[{atom,1,inf}]}]}])),
			%Base2=re:replace(Base1, " ", "",[global,{return,list}]),
			%io:format("Base PRev2: ~p~n",[Base1]),
			
			Base3=re:replace(Base1, "\n", "",[global,{return,list}]),
			Base4=string:sub_word(Base3,1,$.),
			Base5=lists:append(Base4,";"),
			
			recRulesTransformation2(TablaReglas,SortProg,New5++Prog,Base5,Index-1)
		
	end.
		


sizeOfRule(Rule)->
	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(lists:flatten(SelectedRule)),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,Line,Name,Arity,[{clause,Line,Patterns,Guards,Body}]}=Forms,
	sizeofTerms(Patterns,Name)+sizeofTerms(Guards,Name)+sizeofTerms(Body,Name).


sizeofTerms([],Name)->
	0.0;

sizeofTerms([Exp|Exps],Name)->
	
	sizeofTerm(Exp,Name)+sizeofTerms(Exps,Name);

sizeofTerms(Other,Name)->
	0.0.
	
  

sizeofTerm({match,LINE,E1,E2},Name)->
  	
  		sizeofTerm(E1,Name)+sizeofTerm(E2,Name);


sizeofTerm({tuple,LINE,Exps},Name)->
  	
  		sizeofTerms(Exps,Name);
		
		
sizeofTerm({cons,LINE,E1,E2},Name)->
  	
  		sizeofTerm(E1,Name)+sizeofTerm(E2,Name);
	

sizeofTerm({atom,LINE,Atom},Name)->
  	
  		0.0;		

sizeofTerm({integer,LINE,Int},Name)->
  	
  		0.0;

sizeofTerm({float,LINE,Float},Name)->
  	
  		0.0;

sizeofTerm({op,LINE,Op,E1,E2},Name)->
	
		sizeofTerm(E1,Name)+sizeofTerm(E2,Name);
	

sizeofTerm({op,LINE,Op,E},Name)->
	
	sizeofTerm(E,Name);


sizeofTerm({call,LINE,{remote,LINE,EM,EF},Exps},Name)->
	
	{atom,_,Fname}=EF,
	
	case Name == Fname of 
		true ->
			10 + sizeofTerms(Exps,Name);
		
		false ->
			1 + sizeofTerms(Exps,Name)
	end;


sizeofTerm({call,LINE,EF,Exps},Name)->
	
	{atom,_,Fname}=EF,
	
	case Name == Fname of 
		true ->
			10 + sizeofTerms(Exps,Name);
		
		false ->
			1 + sizeofTerms(Exps,Name)
	end;

sizeofTerm({var,LINE,Var},Name)->
	1.0;
	
sizeofTerm(Other,_)->
	0.0.


sortProgramRules(RulesProg,TablaReglas)->
	Rules=sets:to_list(RulesProg),
	%io:format("Rules: ~p ~n",[Rules]),
	ListTuplesSize=lists:foldl(fun (Index,List) ->[{Index,{Rule,_,_,_,_,_,_,_,_,_,_,_,_,_,_}}] = ets:lookup(TablaReglas, Index),List++[{Index,sizeOfRule(Rule)}] end, [], Rules),	
	lists:keysort(2, ListTuplesSize).

sortProgramRules(Programs, TablaReglas, Index)->
	
	[{Index,{SetRules,_,_,_,_}}] = ets:lookup(Programs, Index),
	Rules=sets:to_list(SetRules),
	%io:format("Rules: ~p ~n",[Rules]),
	ListTuplesSize=lists:foldl(fun (Index,List) ->[{Index,{Rule,_,_,_,_,_,_,_,_,_,_,_,_,_,_}}] = ets:lookup(TablaReglas, Index),List++[{Index,sizeOfRule(Rule)}] end, [], Rules),	
	lists:keysort(2, ListTuplesSize).
	

%%% IS PROG REC %%%


isProgRec(P1,VarGlob)->
	
	Programs=ets:lookup_element(VarGlob, programs, 2),
	[{ID1,{Rules1,_,_,_,_}}]=ets:lookup(Programs, P1),
	areRulesRec(sets:to_list(Rules1),VarGlob).

areRulesRec(Rules,VarGlob)->
	
	IsR= lists:foldl(fun (X,Acc)-> case isRec(X,VarGlob) of true -> Acc+1; false -> Acc end end, 0, Rules),
	IsR>0.

	
isRec(Rule,VarGlov)->
	
	TablaReglas=ets:lookup_element(VarGlov, tablaReglas, 2),
	[{Rule,{_,_,_,_,_,_,_,_,_,_,_,_,IsRec,_,_}}] = ets:lookup(TablaReglas, Rule),
	IsRec>0.
	
	







test()->
	
	InstanciasP= ets:new('InstanciasP',  [ordered_set] ),
	InstanciasN= ets:new('InstanciasN', [ordered_set] ),
	
	ets:insert(InstanciasP, {1,{"last([a])->a",0.0,[]}}),
	ets:insert(InstanciasP, {2,{"last([b])->b",0.0,[]}}),
	ets:insert(InstanciasP, {3,{"last([b,a])->a",0.0,[]}}),
	ets:insert(InstanciasP, {4,{"last([a,b])->b",0.0,[]}}),
	ets:insert(InstanciasP, {5,{"last([a,a])->a",0.0,[]}}),
	ets:insert(InstanciasP, {6,{"last([a,b,b])->b",0.0,[]}}),
	ets:insert(InstanciasP, {7,{"last([a,b,a,a])->a",0.0,[]}}),

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

	
	ets:insert(InstanciasN, {1,{"last([a])->b",0.0,[]}}),
	ets:insert(InstanciasN, {2,{"last([b])->a",0.0,[]}}),
	ets:insert(InstanciasN, {3,{"last([])->a",0.0,[]}}),
	ets:insert(InstanciasN, {4,{"last([b,a])->b",0.0,[]}}),
	ets:insert(InstanciasN, {5,{"last([a,b,a])->b",0.0,[]}}),

	
	%ets:insert(InstanciasN, {1,{"member([g,g],j)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([],a)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([i],n)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([p],o)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([r,r],z)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([],e)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([t,t,t],s)->true",0.0,[]}}),
	%ets:insert(InstanciasN, {1,{"member([k,k,k],y)->true",0.0,[]}}),
	
	
	RecInstancesPos= ets:new('InstanciasPosRec', [ordered_set] ),
	RecInstancesNeg= ets:new('InstanciasNegRec', [ordered_set] ),
	RecInstancesPosBase= ets:new('InstanciasPosRecB', [ordered_set] ),
	RecInstancesNegBase = ets:new('InstanciasPosRecB', [ordered_set] ),
	
	util:recInstancesTransform(InstanciasP,RecInstancesPos),
	util:recInstancesTransform(InstanciasN,RecInstancesNeg),
	util:recInstancesBaseCases(InstanciasP,RecInstancesPosBase),
	util:recInstancesBaseCases(InstanciasN,RecInstancesNegBase),
		
		TablaReglas = ets:new('ReglasGen',  [ordered_set] ),
		ets:insert(TablaReglas, {1,{"last([a])->a",0.08571428571428572,[],1,[1],0,[],0,0,0,0,0,0}}),
		ets:insert(TablaReglas, {10,{"last([A])->b",0.17238095238095236,[1],1,[2],1,[1],2,0,0,0,0,0}}),
 		ets:insert(TablaReglas, {11,{"last([A,b,b])->b",0.310877202453068,[1],1,[6],0,[],3,0,0,0,0,0}}),
 		ets:insert(TablaReglas, {8,{"last([A,a])->a",0.42743090472683143,[1],2,[3,5],0,[],0,0,0,0,0,0}}),
 		ets:insert(TablaReglas, {9,{"last([A,b,a,a])->a",0.3331164445867128,[1],1,[7],0,[],1,0,0,0,0,0}}),
		ets:insert(TablaReglas, {32,{"last([A|B])->last(B)",0.663086019006587,[4,1,2],5,[3,4,5,6,7],3,[2,4,5],41,0,0,0,0,0}}),
		
		%ets:insert(TablaReglas,{20,{"last([a|B])->last(B)",0.663086019006587,[4,1,2],5,[3,4,5,6,7],3,[2,4,5],41,0,0,0,0,0}}),
	
		NewRule="last([a|B])->last(B)",
		util:cobertura_Funcional(InstanciasP, InstanciasN,RecInstancesPos,RecInstancesNeg,RecInstancesPosBase,RecInstancesNegBase, NewRule),
		io:format("----------------------------------",[]).
	
	
	
		%----Programs = ets:new('Programs',[ordered_set]),
		
		%---ets:insert(Programs, {1,{sets:from_list([32,9,8,11,1,10]),sets:from_list([1,2,3]),sets:from_list([]),0.8,0.0}}),
		%ets:insert(Programs, {2,{sets:from_list([1,3,5]),sets:from_list([1,2,3,4,5]),sets:from_list([]),0.9}}),
		%ets:insert(Programs, {3,{sets:from_list([9,11]),sets:from_list([1,2,3,4,5,6,7]),sets:from_list([]),0.1}}),
		%ets:insert(Programs, {4,{sets:from_list([9,8]),sets:from_list([]),sets:from_list([]),0.5}}),
		

		%[{Index,{Rule,_,_,_,_,_,_,_}}] = ets:lookup(TablaReglas, 3),
		%----SortProg=sortProgramRules(Programs, TablaReglas, 1),
		%----io:format("Prog: ~p ~n",[SortProg]),
		%SortProg=sortProgramRules(Programs, TablaReglas, 3),
		%----cobFunc_Progs(InstanciasP, InstanciasN, RecInstancesPos, RecInstancesNeg, TablaReglas,SortProg).
		%sortProgramRules(sets:from_list([1,3,5]),TablaReglas),
		
		%ets:match(Programs,{'$1',{sets:from_list([3,1]),'$2','$3','$4','$5'}}).

		%	ListBP=lists:map(fun (X)-> [{X,{SetRules,_,_,Opt}}]=ets:lookup(Programs, X),{X,sets:to_list(SetRules),Opt} end, [2,3]),
		%	io:format("~p~n",[ListBP]),
		%	SortSetBP= lists:keysort(3,ListBP),
		%	io:format("~p~n",[SortSetBP]),
		%	{I,R,O}=lists:nth(length(SortSetBP), SortSetBP),
		%	io:format("~p~n",[R]),
		%	SecuencesOps= lists:map(fun (X) -> [{_,{_,_,PrevActs,_,_,_,_,_}}]=ets:lookup(TablaReglas, X), PrevActs end, R),
			
			
		%	EtsBP=ets:new('bestProgs',[ordered_set]),
			
		%	[ets:insert(EtsBP,{I,lists:nth(I,SecuencesOps)})||I<-lists:seq(1, length(SecuencesOps))],
		%	ets:match(EtsBP,'$1').
			
		%sizeOfRule(Rule).



instUsedAsProgs(Rules, NumPos)->	
		
		lists:foldl(fun (X, Acc) when X < (NumPos+1) -> Acc+1; (_,Acc) -> Acc end, 0, Rules).
