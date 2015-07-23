-module(mml).
-compile(export_all).

decom_Ne(VarGlob)->
	
	
	Pos=ets:lookup_element(VarGlob, instanciasP, 2),
	NumPos=ets:last(Pos),
	PosList=ets:tab2list(Pos),
	Decomposed=lists:map(fun({Id,{Rule,A,B}})-> mmlDecomposed(Rule) end ,PosList),
	
	Temp={V,C,F,Strucs,NumStrucs}=lists:foldl(fun({Vars,Cons,Funcs,Strucs,Rec},{Vars2,Cons2,Funcs2,Strucs2,Rec2})-> 
					  {sets:to_list(sets:union(sets:from_list(Vars),sets:from_list(Vars2))),
					   sets:to_list(sets:union(sets:from_list(Cons),sets:from_list(Cons2))),
					   sets:to_list(sets:union(sets:from_list(Funcs),sets:from_list(Funcs2))),
					   case Strucs2 > Strucs of true -> Strucs2; false -> Strucs end,
					   case Rec2 > Rec of true -> Rec2; false -> Rec end} end,{[],[],[],[],0},Decomposed),
	
	io:format("--> ~p ~p ~p ~p ~p ~n",[V,C,F,Strucs,NumStrucs]),
	
	Instances_mml = ets:new('instances_mml',  [ordered_set] ),
	ets:insert(Instances_mml, {1,V}),
	ets:insert(Instances_mml, {2,C}),
	ets:insert(Instances_mml, {3,F}),
	ets:insert(Instances_mml, {4,Strucs}),
	ets:insert(Instances_mml, {5,NumStrucs}),
	
	Instances_mml.
	%ets:insert(VarGlob, {instances_mml,Instances_mml}).

	
	%io:format("~p~n",[ets:tab2list(Instances_mml)]).
	%[{1,[]},{2,[c,d]},{3,[]},{4,[list]},{5,1}]
	

getMML(VarGlob,Rule)->

	[{1,V},{2,C},{3,F},{4,S},{5,NS}]=ets:tab2list(ets:lookup_element(VarGlob, instances_mml, 2)),
	{Ve,Ce,Fe,Se,NSe}=mmlDecomposed(Rule),
	
	FS=F++S,
	FeSe=Fe++Se,
	
	Vnew=sets:size(sets:union(sets:from_list(V),sets:from_list(Ve))) - sets:size(sets:from_list(V)),
	Cnew=sets:size(sets:union(sets:from_list(C),sets:from_list(Ce))) - sets:size(sets:from_list(C)),
	Fnew=sets:size(sets:union(sets:from_list(F),sets:from_list(Fe))) - sets:size(sets:from_list(F)),
	Snew=sets:size(sets:union(sets:from_list(S),sets:from_list(Se))) - sets:size(sets:from_list(S)),
	
	length(Ve)*(math:log(length(V)+Vnew+1)/math:log(2)) + 
		length(Ce)*(math:log(length(C)+Cnew+1)/math:log(2)) + 
		(2*NSe+length(Fe))*(math:log(length(F)+2*NS+Fnew+2*Snew+1+1)/math:log(2)).
		
	
	

update_mml_e(VarGlob)->
	
	[{1,V},{2,C},{3,F},{4,Strucs},{5,NumStrucs}]=ets:tab2list(ets:lookup_element(VarGlob,instances_mml,2)),
	
	Pos=ets:lookup_element(VarGlob, instanciasP, 2),
	PosList=ets:tab2list(Pos),
	lists:map(fun({Id,{Rule,A,B}})-> MML=getMML(VarGlob,Rule), ets:update_element(Pos, Id, {2,{Rule,MML,B}}) end,PosList),
	
	Neg=ets:lookup_element(VarGlob, instanciasN, 2),
	NegList=ets:tab2list(Neg),
	lists:map(fun({Id,{Rule,A,B}})-> MML=getMML(VarGlob,Rule), ets:update_element(Neg, Id, {2,{Rule,MML,B}}) end,NegList).
		

update_mmm_rules_ini(VarGlob)->
	
	[{1,V},{2,C},{3,F},{4,Strucs},{5,NumStrucs}]=ets:tab2list(ets:lookup_element(VarGlob, instances_mml,2)),
	Rules=ets:lookup_element(VarGlob, tablaReglas, 2),
	NumPos=ets:lookup_element(VarGlob, numPos, 2),
	
	lists:map(fun(X)-> 
			[{Index,{Rule,OptRule,Operators,CobPos,RulesPosCov,CobNeg,RulesNegCov,Size,Vars,Cons,Func,Struc,Rec,_,_}}]=ets:lookup(Rules, X),
			ets:update_element(Rules, X, 
   				 {2,{Rule,mml:getOptMML(VarGlob, RulesPosCov, RulesNegCov, Rule,0),Operators,CobPos,RulesPosCov,CobNeg,RulesNegCov,Size,Vars,Cons,Func,Struc,Rec,
										mml:getMML(VarGlob, Rule),mml:getCovMML(VarGlob, RulesPosCov, RulesNegCov, Rule)}}) end,lists:seq(1, NumPos)).


getCovMML(VarGlob, PosCov, NegCov, Rule )->
	
	NumPos= ets:lookup_element(VarGlob, numPos, 2),
	NumNeg= ets:lookup_element(VarGlob, numNeg, 2),
	NotCovPos= sets:to_list(sets:subtract(sets:from_list(lists:seq(1,NumPos)), sets:from_list(PosCov))),
	
	Pos=ets:lookup_element(VarGlob, instanciasP, 2),
	Neg=ets:lookup_element(VarGlob, instanciasN, 2),
	
	MMLPosNo=lists:foldl(fun(X,Acc)-> [{Id,{_,MML,_}}]= ets:lookup(Pos, X), Acc+MML end, 0.0, NotCovPos),
	MMLNeg=lists:foldl(fun(X,Acc)-> [{Id,{_,MML,_}}]= ets:lookup(Neg, X), Acc+MML end, 0.0, NegCov),
	
	getMML(VarGlob,Rule)+MMLPosNo+MMLNeg.
	
getMaxMML(VarGlob)->
	NumPos= ets:lookup_element(VarGlob, numPos, 2),
	NumNeg= ets:lookup_element(VarGlob, numNeg, 2),
	Pos=ets:lookup_element(VarGlob, instanciasP, 2),
	Neg=ets:lookup_element(VarGlob, instanciasN, 2),
	
	MaxMMLPos=lists:foldl(fun(X,Acc)-> [{Id,{_,MML,_}}]= ets:lookup(Pos, X), case MML>Acc of true -> MML; false-> Acc end end, -1000.0, lists:seq(1,NumPos)),
	MaxMMLNeg=lists:foldl(fun(X,Acc)-> [{Id,{_,MML,_}}]= ets:lookup(Neg, X), case MML>Acc of true -> MML; false-> Acc end end, MaxMMLPos, lists:seq(1,NumNeg)).
	
	
	
	
getOptMML(VarGlob, PosCov, NegCov, Rule, Ops )->
	
	MaxMML= getMaxMML(VarGlob),
	NumPos= ets:lookup_element(VarGlob, numPos, 2),
	NumNeg= ets:lookup_element(VarGlob, numNeg, 2),
	NotCovPos= sets:to_list(sets:subtract(sets:from_list(lists:seq(1,NumPos)), sets:from_list(PosCov))),
	
	Pos=ets:lookup_element(VarGlob, instanciasP, 2),
	Neg=ets:lookup_element(VarGlob, instanciasN, 2),
	
	MMLPosNo=lists:foldl(fun(X,Acc)-> [{Id,{_,MML,_}}]= ets:lookup(Pos, X), Acc+MML end, 0.0, NotCovPos),
	MMLNeg=lists:foldl(fun(X,Acc)-> [{Id,{_,MML,_}}]= ets:lookup(Neg, X), Acc+MML end, 0.0, NegCov),
	
	%Operators=Ops*(math:log(ets:lookup_element(VarGlob, numOperators, 2)+1)/math:log(2)),
	
	Opt= 0.5*(getMML(VarGlob,Rule)/MaxMML)+0.3*(MMLPosNo/(NumPos*MaxMML))+0.2*(MMLNeg/(NumNeg*MaxMML)),%+0.1*Operators,
	
	
	1-Opt. 

	
mmlDecomposed(Rule)->
	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(lists:flatten(SelectedRule)),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,Line,Name,Arity,[{clause,Line,Patterns,Guards,Body}]}=Forms,
	{Size,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerms(Patterns,Name,[],[],[],[],0),
	{Size2,Vars2,Cons2,Func2,Struc2,Rec2}=sizeofTerms(Guards,Name,[],[],[],[],0),
	{Size3,Vars3,Cons3,Func3,Struc3,Rec3}=sizeofTerms(Body,Name,[],[],[],[],0),
	
	%SizeGeneralise= 4*(Vars1+Vars2+Vars3)+2*(Func1+Func2+Func3)+(Cons1+Cons2+Cons3),
	%SizeGeneralise= (Cons1+Cons2+Cons3)*(Vars1+Vars2+Vars3)+2*(Func1+Func2+Func3)+(Cons1+Cons2+Cons3),
	%SizeGeneralise= 3*(Vars1+Vars2+Vars3)+2*(Func1+Func2+Func3),	
	
	%SizeGeneralise= 3*(Vars2+Vars3)+2*(Func2+Func3)+(Cons2+Cons3),
	
	{Vars1++Vars2++Vars3,Cons1++Cons2++Cons3,Func1++Func2++Func3,Struc1++Struc2++Struc3,Rec1+Rec2+Rec3}.
	%{0.0,Vars1+Vars2+Vars3+0.0,Cons1+Cons2+Cons3+0.0,Func1+Func2+Func3+0.0,Struc1+Struc2+Struc3+0.0,Rec1+Rec2+Rec3+0.0}.


sizeofTerms([],Name,Vars,Cons,Func,Struc,Rec)->
	{0,Vars,Cons,Func,Struc,Rec};

sizeofTerms([Exp|Exps],Name,Vars,Cons,Func,Struc,Rec)->
	
	{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerm(Exp,Name,Vars,Cons,Func,Struc,Rec),
	{Size2,Vars2,Cons2,Func2,Struc2,Rec2}=sizeofTerms(Exps,Name,[],[],[],[],0),
	{Size1+Size2,Vars1++Vars2,Cons1++Cons2,Func1++Func2,Struc1++Struc2,Rec1+Rec2};

sizeofTerms(Other,Name,Vars,Cons,Func,Struc,Rec)->
	{0,Vars,Cons,Func,Struc,Rec}.
	
  

sizeofTerm({match,LINE,E1,E2},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerm(E1,Name,Vars,Cons,Func,Struc,Rec),
		{Size2,Vars2,Cons2,Func2,Struc2,Rec2}=sizeofTerm(E2,Name,[],[],[],[],0),
		{Size1+Size2,Vars1++Vars2,Cons1++Cons2,(Func1++Func2)++[match],Struc1++Struc2,Rec1+Rec2};


sizeofTerm({tuple,LINE,Exps},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		sizeofTerms(Exps,Name,Vars,Cons,Func,Struc++[tuple],Rec+1);
		
		
sizeofTerm({nil,LINE},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{0,Vars,Cons,Func,Struc++[list],Rec+1};
		
	
sizeofTerm({cons,LINE,E1,E2},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerm(E1,Name,Vars,Cons,Func,Struc,Rec),
		{Size2,Vars2,Cons2,Func2,Struc2,Rec2}=sizeofTerm(E2,Name,[],[],[],[],0),
		{Size1+Size2,Vars1++Vars2,Cons1++Cons2,Func1++Func2,Struc1++Struc2,Rec1+Rec2};

sizeofTerm({atom,LINE,Atom},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{0,Vars,Cons++[Atom],Func,Struc,Rec};		

sizeofTerm({integer,LINE,Int},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{0,Vars,Cons++[Int],Func,Struc,Rec};

sizeofTerm({float,LINE,Float},Name,Vars,Cons,Func,Struc,Rec)->
  	
  		{0,Vars,Cons++[Float],Func,Struc,Rec};

sizeofTerm({op,LINE,Op,E1,E2},Name,Vars,Cons,Func,Struc,Rec)->
	
		{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerm(E1,Name,Vars,Cons,Func,Struc,Rec),
		{Size2,Vars2,Cons2,Func2,Struc2,Rec2}=sizeofTerm(E2,Name,[],[],[],[],0),
		{Size1+Size2,Vars1++Vars2,Cons1++Cons2,(Func1++Func2)++[Op],Struc1++Struc2,Rec1+Rec2};

sizeofTerm({op,LINE,{_,_,Op},E},Name,Vars,Cons,Func,Struc,Rec)->
	
	sizeofTerm(E,Name,Vars,Cons,Func++[Op],Struc,Rec);


sizeofTerm({call,LINE,{remote,LINE,EM,{_,_,EF}},Exps},Name,Vars,Cons,Func,Struc,Rec)->
	
	
	
	case Name == EF of 
		true ->
			{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerms(Exps,Name,Vars,Cons,Func++[EF],Struc,Rec+1),
			{Size1+10,Vars1,Cons1,Func1,Struc1,Rec1};
		
		false ->
			{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerms(Exps,Name,Vars,Cons,Func++[EF],Struc,Rec),
			{Size1+1,Vars1,Cons1,Func1,Struc1,Rec1}
	end;


sizeofTerm({call,LINE,{_,_,EF},Exps},Name,Vars,Cons,Func,Struc,Rec)->
	

	case Name == EF of 
		true ->
			{Size1,Vars1,Cons1,Func1,Struc1,Rec1}=sizeofTerms(Exps,Name,Vars,Cons,Func++[EF],Struc,Rec+1),
			{Size1+10,Vars1,Cons1,Func1,Struc1,Rec1};
		
		false ->
			{Size1,Vars1,Cons1,Func1,Struc1,Rec1}= sizeofTerms(Exps,Name,Vars,Cons,Func++[EF],Struc,Rec),
			{Size1+1,Vars1,Cons1,Func1,Struc1,Rec1}
	end;

sizeofTerm({var,LINE,Var},Name,Vars,Cons,Func,Struc,Rec)->
	{1,Vars++[Var],Cons,Func,Struc,Rec};
	
sizeofTerm(Other,_,Vars,Cons,Func,Struc,Rec)->
	{0,Vars,Cons,Func,Struc,Rec}.
