-module(sizeRules).
-compile(export_all).


optPrograms(VarGlob, CobPos, CobNeg, CardOpers, NumRulesNew, Rules)->
	
	CardPos=ets:lookup_element(VarGlob, numPos, 2),
	CardNeg= ets:lookup_element(VarGlob, numNeg, 2),
	OPtOpers = CardOpers/(ets:lookup_element(VarGlob, numOperators, 2)*NumRulesNew),
	SizeP= sizeProgLog(Rules,VarGlob),
	
	%Pow=math:pow(2, OPtOpers)/CardOpers,
	
	%(1*(CobPos/CardPos)-0.4*(CobNeg/CardNeg)-0.2*OPtOpers-0.4*(NumRulesNew/CardPos)+0.1*SizeP).
	(1*((CobPos/CardPos)-(CobNeg/CardNeg))-0.2*OPtOpers+0.1*SizeP).
	%(1*(CobPos/CardPos)-0.4*(CobNeg/CardNeg)-0.2*OPtOpers-0.4*(NumRulesNew/CardPos)).
	
optRules(VarGlob, -1, Cob, CobNeg, SizeRule)->
		NumPos=ets:lookup_element(VarGlob, numPos, 2),
		NumNeg= ets:lookup_element(VarGlob, numNeg, 2),
		OPtOpers = 0,
		%Pow=1/ets:lookup_element(VarGlob, numOperators, 2),
	
		%io:format("Cob: ~p, CobNeg: ~p, NumPos: ~p, NumNeg: ~p~n",[Cob,CobNeg,NumPos,NumNeg]),
		%OptRule=(0.4*(Cob/NumPos)-0.3*(CobNeg/NumNeg)-0.1*OPtOpers-0.2*(1/NumPos))/(0.4),

		
		case (SizeRule == 0) or (SizeRule == 0.0) of
			true ->
				%(1*(Cob/NumPos)-0.4*(CobNeg/NumNeg)-0.2*OPtOpers-0.4*(1/NumPos)+0.1*0);
				(1*(Cob/NumPos)-(CobNeg/NumNeg)-0.2*OPtOpers);
			false ->
				LogSizeR=math:log(SizeRule)/math:log(2),
				%(1*(Cob/NumPos)-0.4*(CobNeg/NumNeg)-0.2*OPtOpers-0.4*(1/NumPos)+0.1*LogSizeR)
				(1*(Cob/NumPos)-(CobNeg/NumNeg)-0.2*OPtOpers+0.1*LogSizeR)
		end;


optRules(VarGlob, PrevActs, Cob, CobNeg, SizeRule)->
		NumPos=ets:lookup_element(VarGlob, numPos, 2),
		NumNeg= ets:lookup_element(VarGlob, numNeg, 2),
		OPtOpers = (length(PrevActs)+1)/ets:lookup_element(VarGlob, numOperators, 2),
		
		%Pow=math:pow(2, OPtOpers)/ets:lookup_element(VarGlob, numOperators, 2),
		%OptRule=(0.4*(Cob/NumPos)-0.3*(CobNeg/NumNeg)-0.1*OPtOpers-0.2*(1/NumPos))/(0.4),

		case (SizeRule == 0) or (SizeRule == 0.0) of
			true ->
				%(1*(Cob/NumPos)-0.4*(CobNeg/NumNeg)-0.2*OPtOpers-0.4*(1/NumPos));
				(1*(Cob/NumPos)-(CobNeg/NumNeg)-0.2*OPtOpers);
			false ->
				LogSizeR=math:log(SizeRule)/math:log(2),
				%(1*(Cob/NumPos)-0.4*(CobNeg/NumNeg)-0.2*OPtOpers-0.4*(1/NumPos)+0.1*LogSizeR)
				(1*(Cob/NumPos)-(CobNeg/NumNeg)-0.2*OPtOpers+0.1*LogSizeR)
		end.
			
	


sizeProgLog(Rules,VarGlob)->
	%io:format("Rules: ~p~n",[Rules]),
	Value=lists:foldl(fun (X,Acc)-> [{KeyR,{_,_,_,_,_,_,_,Size,_,_,_,_,_,_,_}}]=ets:lookup(ets:lookup_element(VarGlob, tablaReglas, 2),X),
									case (Size == 0) or (Size == 0.0) of true -> Acc; false -> Temp= math:log(Size)/math:log(2), Acc+Temp end end,0, Rules),
	Value/length(Rules).


	
sizeOfRuleDecomposed(Rule)->
	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(lists:flatten(SelectedRule)),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,Line,Name,Arity,[{clause,Line,Patterns,Guards,Body}]}=Forms,
	{Size,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerms(Patterns,Name,0,0,0,0,0),
	{Size2,Vars2,Cons2,Func2,Struc2,Rec2}=sizeofTerms(Guards,Name,0,0,0,0,0),
	{Size3,Vars3,Cons3,Func3,Struc3,Rec3}=sizeofTerms(Body,Name,0,0,0,0,0),
	
	SizeGeneralise= 4*(Vars1+Vars2+Vars3)+2*(Func1+Func2+Func3)+(Cons1+Cons2+Cons3),
	%SizeGeneralise= (Cons1+Cons2+Cons3)*(Vars1+Vars2+Vars3)+2*(Func1+Func2+Func3)+(Cons1+Cons2+Cons3),
	%SizeGeneralise= 3*(Vars1+Vars2+Vars3)+2*(Func1+Func2+Func3),	
	
	%SizeGeneralise= 3*(Vars2+Vars3)+2*(Func2+Func3)+(Cons2+Cons3),
	
	{Vars1,SizeGeneralise+0.0,Vars1+Vars2+Vars3+0.0,Cons1+Cons2+Cons3+0.0,Func1+Func2+Func3+0.0,Struc1+Struc2+Struc3+0.0,Rec1+Rec2+Rec3+0.0}.
	%{0.0,Vars1+Vars2+Vars3+0.0,Cons1+Cons2+Cons3+0.0,Func1+Func2+Func3+0.0,Struc1+Struc2+Struc3+0.0,Rec1+Rec2+Rec3+0.0}.


sizeofTerms([],Name,Vars,Cons,Func,Struc,Rec)->
	{0,Vars,Cons,Func,Struc,Rec};

sizeofTerms([Exp|Exps],Name,Vars,Cons,Func,Struc,Rec)->
	
	{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerm(Exp,Name,Vars,Cons,Func,Struc,Rec),
	{Size2,Vars2,Cons2,Func2,Struc2,Rec2}=sizeofTerms(Exps,Name,0,0,0,0,0),
	{Size1+Size2,Vars1+Vars2,Cons1+Cons2,Func1+Func2,Struc1+Struc2,Rec1+Rec2};

sizeofTerms(Other,Name,Vars,Cons,Func,Struc,Rec)->
	{0,Vars,Cons,Func,Struc,Rec}.
	
  

sizeofTerm({match,LINE,E1,E2},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerm(E1,Name,Vars,Cons,Func,Struc,Rec),
		{Size2,Vars2,Cons2,Func2,Struc2,Rec2}=sizeofTerm(E2,Name,0,0,0,0,0),
		{Size1+Size2,Vars1+Vars2,Cons1+Cons2,1+Func1+Func2,Struc1+Struc2,Rec1+Rec2};


sizeofTerm({tuple,LINE,Exps},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		sizeofTerms(Exps,Name,Vars,Cons,Func,Struc+1,Rec);
		
		
sizeofTerm({nil,LINE},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{0,Vars,Cons,Func,1+Struc,Rec};
		
	
sizeofTerm({cons,LINE,E1,E2},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerm(E1,Name,Vars,Cons,Func,Struc,Rec),
		{Size2,Vars2,Cons2,Func2,Struc2,Rec2}=sizeofTerm(E2,Name,0,0,0,0,0),
		{Size1+Size2,Vars1+Vars2,Cons1+Cons2,Func1+Func2,Struc1+Struc2,Rec1+Rec2};

sizeofTerm({atom,LINE,Atom},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{0,Vars,Cons+1,Func,Struc,Rec};		

sizeofTerm({integer,LINE,Int},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{0,Vars,Cons+1,Func,Struc,Rec};

sizeofTerm({float,LINE,Float},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{0,Vars,Cons+1,Func,Struc,Rec};

sizeofTerm({op,LINE,Op,E1,E2},Name,Vars,Cons,Func,Struc,Rec)->
	
		{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerm(E1,Name,Vars,Cons,Func,Struc,Rec),
		{Size2,Vars2,Cons2,Func2,Struc2,Rec2}=sizeofTerm(E2,Name,0,0,0,0,0),
		{Size1+Size2,Vars1+Vars2,Cons1+Cons2,1+Func1+Func2,Struc1+Struc2,Rec1+Rec2};

sizeofTerm({op,LINE,Op,E},Name,Vars,Cons,Func,Struc,Rec)->
	
	sizeofTerm(E,Name,Vars,Cons,Func+1,Struc,Rec);


sizeofTerm({call,LINE,{remote,LINE,EM,EF},Exps},Name,Vars,Cons,Func,Struc,Rec)->
	
	{atom,_,Fname}=EF,
	
	case Name == Fname of 
		true ->
			{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerms(Exps,Name,Vars,Cons,Func+1,Struc,Rec+1),
			{Size1+10,Vars1,Cons1,Func1,Struc1,Rec1};
		
		false ->
			{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerms(Exps,Name,Vars,Cons,Func+1,Struc,Rec),
			{Size1+1,Vars1,Cons1,Func1,Struc1,Rec1}
	end;


sizeofTerm({call,LINE,EF,Exps},Name,Vars,Cons,Func,Struc,Rec)->
	
	{atom,_,Fname}=EF,
	
	case Name == Fname of 
		true ->
			{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerms(Exps,Name,Vars,Cons,Func+1,Struc,Rec+1),
			{Size1+10,Vars1,Cons1,Func1,Struc1,Rec1};
		
		false ->
			{Size1,Vars1,Cons1,Func1,Struc1,Rec1}= sizeofTerms(Exps,Name,Vars,Cons,Func+1,Struc,Rec),
			{Size1+1,Vars1,Cons1,Func1,Struc1,Rec1}
	end;

sizeofTerm({var,LINE,Var},Name,Vars,Cons,Func,Struc,Rec)->
	{1,Vars+1,Cons,Func,Struc,Rec};
	
sizeofTerm(Other,_,Vars,Cons,Func,Struc,Rec)->
	{0,Vars,Cons,Func,Struc,Rec}.







stopCriterium(IdP, VarGlob) ->

	NumProgs=20,
	case (IdP > ets:lookup_element(VarGlob, numPos, 2) + NumProgs) of 
		true ->
			Programs=ets:lookup_element(VarGlob, programs, 2),
			Epsilon = 0.025,
	
			ListsOpts=lists:map(fun(X)-> [{ID1,{_,_,_,Opt,_}}]=ets:lookup(Programs, IdP-X),Opt end,lists:seq(0,NumProgs-1)),
			SDprograms=myMath:std_deviation(ListsOpts),
			SDprograms =< Epsilon;
		false ->
			false
	end.
			  
	














