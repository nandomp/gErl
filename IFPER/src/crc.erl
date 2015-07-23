-module(crc).
-compile(export_all).


crg(Rule)->
	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,Line,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	io:format("FORMS: ~p~n",[Forms]),
	EtsEC = ets:new('EtsEC',[ordered_set] ),
	EtsVars = ets:new('EtsVars',[ordered_set] ),
	EtsOcTree =  ets:new('EtsOcTree',[ordered_set] ),
	
	%io:format("Vars: ~p~n",[ets:match(EtsVars,'$1')]),
	fillEtsVars(Body, Patterns,EtsVars),
	%io:format("Vars: ~p~n",[ets:match(EtsVars,'$1')]),
	equivalenceClassesArgs(Patterns, EtsEC,EtsVars,EtsOcTree),
	equivalenceClassesArgs(Body, EtsEC,EtsVars,EtsOcTree),
	
	io:format("EQ: ~p~n",[ets:match(EtsEC,'$1')]),
	io:format("Vars: ~p~n",[ets:match(EtsVars,'$1')]),

	%% Rellenadas la tabla de clases de Equivalencia y de variables existentes, creo tabla de reglas a devolver con la primera regla %.

	EtsRules = ets:new('EtsRules',[ordered_set]),
	IndexOT=1,
	ets:insert(EtsRules,{1,Patterns,Guards,Body,ets:tab2list(EtsEC),IndexOT}),
	io:format("Oc Tree ~p~n",[ets:match(EtsOcTree,'$1')]),
	io:format("Rules: ~p~n",[ets:match(EtsRules,'$1')]),	
	
	crg2(EtsRules, EtsEC, EtsVars,EtsOcTree),
	
	io:format("RulesAfter: ~p~n",[ets:match(EtsRules,'$1')]),
	
	io:format("~p~n",[prettyprintRules(EtsRules,Line, Name, Arity,1)]),
	
	ets:delete(EtsEC),
		ets:delete(EtsVars),
		ets:delete(EtsOcTree),
	ets:delete(EtsRules).
	
	

crg2(EtsRules, EtsEC, EtsVars,EtsOcTree)->
		
	[{_,Patterns,Guards,Body,ListEQVars,IOT}]=ets:lookup(EtsRules, 1),
	PGBorig=Patterns++Guards++Body,	
	
	crg3(PGBorig,EtsRules, EtsEC, EtsVars,EtsOcTree,1,1,ets:last(EtsRules)).

				

		

crg3(PGBorig,EtsRules, EtsEC, EtsVars,EtsOcTree,IndexOT,IndexR,LastRule)->

io:format("*************~n",[]),
io:format("IndexOT ~p~n",[IndexOT]),
io:format("IndexR ~p~n",[IndexR]),
io:format("LastRule ~p~n",[LastRule]),
io:format("Last OCTree ~p~n",[ets:last(EtsOcTree)]),
io:format("*************~n",[]),

case IndexOT > ets:last(EtsOcTree)	of

	false ->
		
		case IndexR =< LastRule of
		
			true ->
				
				[{_,Patterns,Guards,Body,ListEQVars,IOT}]=ets:lookup(EtsRules, IndexR),
				PGB=Patterns++Guards++Body,
				%io:format("Antigua1 EQVArs ~p~n",[ListEQVars]),
				[{_,ExpOT,Ocu}]=ets:lookup(EtsOcTree, IndexOT),
		
				%Nueva Variable para nueva regla		
				NewVar=freevar3(PGB),
				NewListEQVars=ListEQVars,
		
				{NewPGB,NewListEQVars2,N}=matchPGBExps(PGBorig,PGB,NewListEQVars,NewVar,ExpOT,Ocu,1,true),		
				%PGB en Patters, Guards y Body
		
				case NewPGB =/= PGB of
			
					true->
						%Hemos creado nueva regla
						{NewPatterns, GB}=lists:split(length(Patterns), NewPGB),
						{NewGuards,NewBody}=lists:split(length(Guards), GB),	
						io:format(" NUEVA VARIABLE REGLA ~n",[]),
						io:format("- Antiguo PGB ~p~n",[PGB]),	
						io:format("- Nuevo PGB ~p~n",[NewPGB]),		
						io:format("- Antigua EQVArs ~p~n",[ListEQVars]),
						io:format("- Nueva EQVArs ~p~n",[NewListEQVars2]),
		
						ets:insert(EtsRules,{ets:last(EtsRules)+1,NewPatterns,NewGuards,NewBody,NewListEQVars2,1}),
				
						%% Reutilizo las variables utilizadas con dicha regla.
						{value,{KeyListQV,Exp,VarsUsed}}=lists:keysearch(ExpOT,2,ListEQVars),
						case length(VarsUsed) of

							0 -> 
								nohaynuevas;

							Length ->						
								crg4(PGBorig,EtsRules,ListEQVars,VarsUsed,EtsOcTree,IndexOT,IndexR,Length)
						end,
						
						crg3(PGBorig,EtsRules, EtsEC, EtsVars,EtsOcTree,IndexOT,IndexR+1,LastRule);

					false ->
						crg3(PGBorig,EtsRules, EtsEC, EtsVars,EtsOcTree,IndexOT,IndexR+1,LastRule)
				end;
		
			false ->
			crg3(PGBorig,EtsRules, EtsEC, EtsVars,EtsOcTree,IndexOT+1,1,ets:last(EtsRules))
			
		end;
	
	true->
		ok
end.

		
crg4(PGBorig,EtsRules,NewListEQVars,VarsUsed,EtsOcTree,IndexOT,IndexR,0)->

		ok;

crg4(PGBorig,EtsRules,NewListEQVars,VarsUsed,EtsOcTree,IndexOT,IndexR,IndexListEQVArs)->
	
	[{_,Patterns,Guards,Body,ListEQVars,IOT}]=ets:lookup(EtsRules, IndexR),
	PGB=Patterns++Guards++Body,
	[{_,ExpOT,Ocu}]=ets:lookup(EtsOcTree, IndexOT),	
	NewVar=lists:nth(IndexListEQVArs,VarsUsed),
	{NewPGB,NewListEQVars2,N}=matchPGBExps(PGBorig,PGB,NewListEQVars,NewVar,ExpOT,Ocu,1,false),
	{NewPatterns, GB}=lists:split(length(Patterns), NewPGB),
	{NewGuards,NewBody}=lists:split(length(Guards), GB),
	ets:insert(EtsRules,{ets:last(EtsRules)+1,NewPatterns,NewGuards,NewBody,NewListEQVars2,1}),
	io:format(" USADA VARIABLE REGLA ~n",[]),
	io:format("- Nuevo PGB ~p~n",[NewPGB]),		
	io:format("- Antigua EQVArs ~p~n",[ListEQVars]),
	io:format("- Nueva EQVArs ~p~n",[NewListEQVars2]),
	crg4(PGBorig,EtsRules,NewListEQVars,VarsUsed,EtsOcTree,IndexOT,IndexR,IndexListEQVArs-1).
		

	
	
	
	
%% match entre expresion actual del arbol de ocurrencias con las expresiones de la regla %%


matchPGBExps([ExpORIG|ExpsORIG],[Exp|Exps],NewListEQVars, NewVar, ExpOT, Ocu, Num, Nueva)->
	
	%io:format("ExpORIG: ~p, Exp: ~p, ExpOT: ~p, Ocu: ~p, Num: ~p ~n",[ExpORIG,Exp,ExpOT,Ocu, Num]),
	
	io:format("Match:  ~p ~n",[matchPGBExp(ExpORIG,Exp,NewListEQVars, NewVar, ExpOT, Ocu, Num, Nueva)]),
	
	{NewPGB,NewListEQVars2,NewNum}=matchPGBExp(ExpORIG,Exp,NewListEQVars, NewVar, ExpOT, Ocu, Num, Nueva),
	
	{NewPGB2,NewListEQVars3,NewNum2}=matchPGBExps(ExpsORIG,Exps,NewListEQVars2, NewVar, ExpOT, Ocu, NewNum, Nueva),
	
	{[NewPGB|NewPGB2],NewListEQVars3,NewNum2};


matchPGBExps(Other,Other1,NewListEQVars, NewVar, ExpOT, Ocu, Num, Nueva)->
	
	matchPGBExp(Other,Other1,NewListEQVars, NewVar, ExpOT, Ocu, Num, Nueva).
	
	
	
%********************************************************************************	
	

%%MATCH

matchPGBExp({match,LINEo,E1o,E2o},{match,LINE,E1,E2},NewListEQVars,NewVar,{match,LINEo,E1o,E2o},Ocu,Ocu,Nueva)->
	io:format("match1~n",[]),
	matchPGBExps2({match,LINEo,E1o,E2o},{match,LINE,E1,E2},NewListEQVars,NewVar,{match,LINE,E1,E2},Ocu,Ocu,Nueva);

matchPGBExp({match,LINEo,E1o,E2o},{match,LINE,E1,E2},NewListEQVars,NewVar,{match,LINEo,E1o,E2o},Ocu,Num,Nueva)->
	io:format("match2~n",[]),
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(E1o,E1,NewListEQVars,NewVar,{match,LINEo,E1o,E2o},Ocu,Num+1,Nueva),
	{NewPGB2,NewListEQVars2,NewNum2}=matchPGBExps(E2o,E2,NewListEQVars1,NewVar,{match,LINEo,E1o,E2o},Ocu,NewNum,Nueva),
	
	{{match,LINEo,NewPGB1,NewPGB2},NewListEQVars2,NewNum2};

matchPGBExp({match,LINEo,E1o,E2o},{match,LINE,E1,E2},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva)->
	io:format("match3~n",[]),
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(E1o,E1,NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva),
	{NewPGB2,NewListEQVars2,NewNum2}=matchPGBExps(E2o,E2,NewListEQVars1,NewVar,ExpOT,Ocu,NewNum,Nueva),
	
	{{match,LINEo,NewPGB1,NewPGB2},NewListEQVars2,NewNum2};

%%TUPLE

matchPGBExp({tuple,LINEo,ExpsO},{tuple,LINE,Exps},NewListEQVars,NewVar,{tuple,LINEo,ExpsO},Ocu,Ocu,Nueva)->
	
	io:format("tuple1~n",[]),
	matchPGBExps2({tuple,LINEo,ExpsO},{tuple,LINE,Exps},NewListEQVars,NewVar,{tuple,LINEo,ExpsO},Ocu,Ocu,Nueva);
	

matchPGBExp({tuple,LINEo,ExpsO},{tuple,LINE,Exps},NewListEQVars,NewVar,{tuple,LINEo,ExpsO},Ocu,Num,Nueva)->
	io:format("tuple2~n",[]),
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(ExpsO,Exps,NewListEQVars,NewVar,{tuple,LINEo,ExpsO},Ocu,Num+1,Nueva),
	{{tuple,LINEo,NewPGB1},NewListEQVars1,NewNum};
	
	
matchPGBExp({tuple,LINEo,ExpsO},{tuple,LINE,Exps},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva)->
	io:format("tuple3~n",[]),
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(ExpsO,Exps,NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva),	
	{{tuple,LINEo,NewPGB1},NewListEQVars1,NewNum};

%%CONS

matchPGBExp({cons,LINEo,E1o,E2o},{cons,LINE,E1,E2},NewListEQVars,NewVar,{cons,LINEo,E1o,E2o},Ocu,Ocu,Nueva)->
	io:format("cons1~n",[]),
	matchPGBExps2({cons,LINEo,E1o,E2o},{cons,LINE,E1,E2},NewListEQVars,NewVar,{cons,LINEo,E1o,E2o},Ocu,Ocu,Nueva);

matchPGBExp({cons,LINEo,E1o,E2o},{cons,LINE,E1,E2},NewListEQVars,NewVar,{cons,LINEo,E1o,E2o},Ocu,Num,Nueva)->
	io:format("cons2~n",[]),
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(E1o,E1,NewListEQVars,NewVar,{cons,LINEo,E1o,E2o},Ocu,Num+1,Nueva),
	{NewPGB2,NewListEQVars2,NewNum2}=matchPGBExps(E2o,E2,NewListEQVars1,NewVar,{cons,LINEo,E1o,E2o},Ocu,NewNum,Nueva),
	
	{{cons,LINEo,NewPGB1,NewPGB2},NewListEQVars2,NewNum2};

matchPGBExp({cons,LINEo,E1o,E2o},{cons,LINE,E1,E2},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva)->
	io:format("cons3~n",[]),
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(E1o,E1,NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva),
	{NewPGB2,NewListEQVars2,NewNum2}=matchPGBExps(E2o,E2,NewListEQVars1,NewVar,ExpOT,Ocu,NewNum,Nueva),	
	{{cons,LINEo,NewPGB1,NewPGB2},NewListEQVars2,NewNum2};

%%ATOM

%matchPGBExp({atom,LINEo,ExpsO},{atom,LINE,Exps},NewListEQVars,NewVar,{atom,LINEo,ExpsO},Ocu,Ocu,Nueva)->
		
%	matchPGBExps2({atom,LINEo,ExpsO},{atom,LINE,Exps},NewListEQVars,NewVar,{atom,LINE,Exps},Ocu,Ocu,Nueva);
	

%matchPGBExp({atom,LINEo,ExpsO},{atom,LINE,Exps},NewListEQVars,NewVar,{atom,LINEo,ExpsO},Ocu,Num,Nueva)->
	
%	matchPGBExps2({atom,LINEo,ExpsO},{atom,LINE,Exps},NewListEQVars,NewVar,{atom,LINEo,ExpsO},Ocu,Num,Nueva);
	
	
matchPGBExp({atom,LINEo,ExpsO},{atom,LINE,Exps},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva)->
	io:format("atom~n",[]),
	matchPGBExps2({atom,LINEo,ExpsO},{atom,LINE,Exps},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva);

%%INTEGER


%matchPGBExp({integer,LINEo,ExpsO},{integer,LINE,Exps},NewListEQVars,NewVar,{integer,LINEo,ExpsO},Ocu,Ocu,Nueva)->
		
%	matchPGBExps2({integer,LINEo,ExpsO},{integer,LINE,Exps},NewListEQVars,NewVar,{integer,LINE,Exps},Ocu,Ocu,Nueva);
	

%matchPGBExp({integer,LINEo,ExpsO},{integer,LINE,Exps},NewListEQVars,NewVar,{integer,LINEo,ExpsO},Ocu,Num,Nueva)->
	
%	matchPGBExps2({integer,LINEo,ExpsO},{integer,LINE,Exps},NewListEQVars,NewVar,{integer,LINEo,ExpsO},Ocu,Num,Nueva);
	%{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(ExpsO,Exps,NewListEQVars,NewVar,{atom,LINEo,ExpsO},Ocu,Num+1,Nueva),
	%{{integer,LINE,NewPGB1},NewListEQVars1,NewNum+1};
	
	
matchPGBExp({integer,LINEo,ExpsO},{integer,LINE,Exps},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva)->
	io:format("integer~n",[]),
	matchPGBExps2({integer,LINEo,ExpsO},{integer,LINE,Exps},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva);
	


%%OP5

matchPGBExp({op,LINEo,Opo,E1o,E2o},{op,LINE,Op,E1,E2},NewListEQVars,NewVar,{op,LINEo,Opo,E1o,E2o},Ocu,Ocu,Nueva)->
		
	matchPGBExps2({op,LINEo,Opo,E1o,E2o},{op,LINE,Op,E1,E2},NewListEQVars,NewVar,{op,LINEo,Opo,E1o,E2o},Ocu,Ocu,Nueva);
	

matchPGBExp({op,LINEo,Opo,E1o,E2o},{op,LINE,Op,E1,E2},NewListEQVars,NewVar,{op,LINEo,Opo,E1o,E2o},Ocu,Num,Nueva)->
	
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(E1o,E1,NewListEQVars,NewVar,{op,LINEo,Opo,E1o,E2o},Ocu,Num+1,Nueva),
	{NewPGB2,NewListEQVars22,NewNum2}=matchPGBExps(E2o,E2,NewListEQVars1,NewVar,{op,LINEo,Opo,E1o,E2o},Ocu,NewNum,Nueva),									
	
	{{op,LINE,Opo,NewPGB1,NewPGB2},NewListEQVars22,NewNum2};
	
	
matchPGBExp({op,LINEo,Opo,E1o,E2o},{op,LINE,Op,E1,E2},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva)->
	
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(E1o,E1,NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva),
	{NewPGB2,NewListEQVars22,NewNum2}=matchPGBExps(E2o,E2,NewListEQVars1,NewVar,ExpOT,Ocu,NewNum,Nueva),									
	
	{{op,LINE,Opo,NewPGB1,NewPGB2},NewListEQVars22,NewNum2};

%%OP4

matchPGBExp({op,LINEo,Opo,E1o},{op,LINE,Op,E1},NewListEQVars,NewVar,{op,LINEo,Opo,E1o},Ocu,Ocu,Nueva)->
		
	matchPGBExps2({op,LINEo,Opo,E1o},{op,LINE,Op,E1},NewListEQVars,NewVar,{op,LINEo,Opo,E1o},Ocu,Ocu,Nueva);
	

matchPGBExp({op,LINEo,Opo,E1o},{op,LINE,Op,E1},NewListEQVars,NewVar,{op,LINEo,Opo,E1o},Ocu,Num,Nueva)->
	
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(E1o,E1,NewListEQVars,NewVar,{op,LINEo,Opo,E1o},Ocu,Num+1,Nueva),
							
	
	{{op,LINE,Opo,NewPGB1},NewListEQVars1,NewNum};
	
	
matchPGBExp({op,LINEo,Opo,E1o},{op,LINE,Op,E1},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva)->
	
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(E1o,E1,NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva),
	{{op,LINE,Opo,NewPGB1},NewListEQVars1,NewNum};


%%CALL

matchPGBExp({call,LINEo,{remote,LINEo,EMo,EFo},ExpsO},{call,LINE,{remote,LINE,EM,EF},Exps},NewListEQVars,NewVar,{call,LINEo,{remote,LINEo,EMo,EFo},ExpsO},Ocu,Ocu,Nueva)->
		
	matchPGBExps2({call,LINEo,{remote,LINEo,EMo,EFo},ExpsO},{call,LINE,{remote,LINE,EM,EF},Exps},NewListEQVars,NewVar,{call,LINEo,{remote,LINEo,EMo,EFo},ExpsO},Ocu,Ocu,Nueva);
	

matchPGBExp({call,LINEo,{remote,LINEo,EMo,EFo},ExpsO},{call,LINE,{remote,LINE,EM,EF},Exps},NewListEQVars,NewVar,{call,LINEo,{remote,LINEo,EMo,EFo},ExpsO},Ocu,Num,Nueva)->
	
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(ExpsO,Exps,NewListEQVars,NewVar,{call,LINEo,{remote,LINEo,EMo,EFo},ExpsO},Ocu,Num+1,Nueva),
							
	
	{{call,LINE,{remote,LINE,EM,EF},NewPGB1},NewListEQVars1,NewNum};
	
	
matchPGBExp({call,LINEo,{remote,LINEo,EMo,EFo},ExpsO},{call,LINE,{remote,LINE,EM,EF},Exps},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva)->
	
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(ExpsO,Exps,NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva),

	{{call,LINE,{remote,LINE,EM,EF},NewPGB1},NewListEQVars1,NewNum};

%%CALL2

matchPGBExp({call,LINEo,EFo,ExpsO},{call,LINE,EF,Exps},NewListEQVars,NewVar,{call,LINEo,EFo,ExpsO},Ocu,Ocu,Nueva)->
		
	matchPGBExps2({call,LINEo,EFo,ExpsO},{call,LINE,EF,Exps},NewListEQVars,NewVar,{call,LINEo,EFo,ExpsO},Ocu,Ocu,Nueva);
	

matchPGBExp({call,LINEo,EFo,ExpsO},{call,LINE,EF,Exps},NewListEQVars,NewVar,{call,LINEo,EFo,ExpsO},Ocu,Num,Nueva)->
	
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(ExpsO,Exps,NewListEQVars,NewVar,{call,LINEo,EFo,ExpsO},Ocu,Num+1,Nueva),
								
	{{call,LINE,EF,NewPGB1},NewListEQVars1,NewNum};
	
	
matchPGBExp({call,LINEo,EFo,ExpsO},{call,LINE,EF,Exps},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva)->
	
	{NewPGB1,NewListEQVars1,NewNum}=matchPGBExps(ExpsO,Exps,NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva),

	{{call,LINE,EF,NewPGB1},NewListEQVars1,NewNum};

%%VAR

matchPGBExp(ExpO,{var,LINE,VAR},NewListEQVars,NewVar,ExpO,Ocu,Num,Nueva)->
	io:format("var1~n",[]),
	{{var,LINE,VAR},NewListEQVars,Num+1};

matchPGBExp(ExpO,{var,LINE,VAR},NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva)->
	io:format("var2~n",[]),
	{{var,LINE,VAR},NewListEQVars,Num};




matchPGBExp(Other1,Other2,NewListEQVars,NewVar,ExpO,Ocu,Num,Nueva)->
	io:format("other1: ~p Other2: ~p Num: ~p~n",[Other1,Other2,Num]),
	{Other1,NewListEQVars,Num}.
%%%%%




%matchPGBExp([ExpORIG|ExpsORIG],[{var,LINE,VAR}|Exps],NewListEQVars,NewVar,ExpORIG,Ocu,Num,Nueva)->
	
%	{NewPGB,NewListEQVars2}=matchPGBExps(ExpsORIG,Exps,NewListEQVars,NewVar,ExpORIG,Ocu,Num+1,Nueva),
%	{[{var,LINE,VAR}|NewPGB],NewListEQVars2};





matchPGBExps2(ExpORIG,Exp,NewListEQVars, NewVar, ExpOT, Ocu, Num, Nueva)->
	
		io:format("-----GEN REGLA---- ~n",[]),
		io:format("-----ExpORIG: ~p~n",[ExpORIG]),
		io:format("-----Exp: ~p~n",[Exp]),
		io:format("-----ExpOT: ~p~n",[ExpOT]),
		io:format("-----Ocu: ~p~n",[Ocu]),
		io:format("-----Vez: ~p~n",[Num]),
		io:format("-----NewVar: ~p~n",[NewVar]),
		io:format("-----NewListEQVars: ~p~n",[NewListEQVars]),
		
		case equal(ExpORIG,ExpOT)  of 
			
			true -> 
				
				case Ocu==Num of
					
					
					true ->
						
						io:format("Ocu==Num~n",[]),
						%case ets:match(NewEtsEQVars, {'$1',ExpOT,'_'}) of
						%	[[KeyEQVars]] ->
						%		[{_,Exp,VarsUsed}]=ets:lookup(NewEtsEQVars, KeyEQVars),
						%		NewVarsUsed=VarsUsed++[NewVar],
						%		ets:update_element(NewEtsEQVars, KeyEQVars, {3,NewVarsUsed}),
						%		[NewVar|matchPGBExps(Exps,NewEtsEQVars,NewVar,ExpOT,Ocu,Num+1)];%Variables ya usadas y una nueva %[freeVariable()|Exps];
						%		
						%	[] -> 
						%		error
						%end;	
						
						case lists:keysearch(ExpOT,2,NewListEQVars) of
							
							{value,{Key,ExpEQ,VarsUsed}} ->
									io:format("Lista con variables ~n",[]),
									case Nueva of
						
										true->
											io:format("Nueva ~n",[]),
											%nueva variable a lista de variables
											NewListEQVars2=lists:keyreplace(ExpEQ,2,NewListEQVars,{Key,ExpEQ,VarsUsed++[NewVar]}),
											%{NewPGB,NewListEQVars3}=matchPGBExps(ExpsORIG,Exps,NewListEQVars2,NewVar,ExpOT,Ocu,Num+1,Nueva),
											{NewVar,NewListEQVars2,Num+1};%Variables ya usadas y una nueva %[freeVariable()|Exps];

										false ->
											io:format("Usada ~n",[]),
											%{NewPGB,NewListEQVars2}=matchPGBExps(ExpsORIG,Exps,NewListEQVars,NewVar,ExpOT,Ocu,Num+1,Nueva),
											{NewVar,NewListEQVars,Num+1}%Variables ya usadas y una nueva %[freeVariable()|Exps];		
									end;								

							false ->
								error
						end;
							
						
						
					false ->
						io:format("Ocu=/=Num~n",[]),
						%{NewPGB,NewListEQVars2}=matchPGBExps(ExpsORIG,Exps,NewListEQVars,NewVar,ExpOT,Ocu,Num+1,Nueva),
						{Exp,NewListEQVars,Num+1}
				end;
			
			false ->
				io:format("No equal",[]),
				%{NewPGB,NewListEQVars2}=matchPGBExps(ExpsORIG,Exps,NewListEQVars,NewVar,ExpOT,Ocu,Num,Nueva),
				{Exp,NewListEQVars,Num}
		end;


matchPGBExps2(_,Other,NewListEQVars,_,_,_,Num,_)->		
	{Other,NewListEQVars,Num}.


%%%Inprimir reglas a partir de AST %%%


prettyprintRules(EtsRules,Line, Name, Arity,Index)->
	
	prettyprintRules2(EtsRules,Line, Name, Arity, [],Index).
	
prettyprintRules2(EtsRules,Line, Name, Arity, Rules, Index)->
		
	case Index==ets:last(EtsRules)+1 of 
		
		false ->
			[{_,Patterns,Guards,Body,ListEQVars,_}]=ets:lookup(EtsRules, Index),
			New = erl_prettypr:format(erl_syntax:form_list([{function,Line,Name,Arity,[{clause,1,Patterns,Guards,Body}]}])),
			New2=re:replace(New, " ", "",[global,{return,list}]),
			prettyprintRules2(EtsRules,Line, Name, Arity, Rules++[string:sub_word(New2,1,$.)],Index+1);
		true ->
			Rules
	end.




%%% Igualdad de expresiones %%%

equal(Exp1,Exp2)->
	
	
	io:format("Exp1s ~p~n",[Exp1]),
	io:format("Exp2s ~p~n",[Exp2]),
	case erlang:is_list(Exp1) of
					
				true -> 
					
					case erlang:is_list(Exp2) of
							
						false ->  
							
							false;
						
						true ->
		  					case (length(Exp1) >1) of
								
								true ->
		   							[E1,Es1]=Exp1,
									case (length(Exp2) >1) of
										
										true ->
											[E2,Es2]=Exp2,
											equal(E1,E2) and equal(Es1,Es2);
										
										false ->
											false
									end;
								
								false ->
									case (length(Exp2) == length(Exp1)) of
										
										true ->
											[E1]=Exp1,
											[E2]=Exp2,
											equal(E1,E2);
										
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
												
												equal2(Exp1,Exp2);
											
											false ->
													
												false
									end;
								
								false ->
										
									case erlang:is_tuple(Exp2) of
										
										true ->
												
												false;
											
										false ->
													
												equal3(Exp1,Exp2)
									end
							
							end;							
								
						
						true ->
		  
		   					false
					end
	
	end.
		
		  


equal2(Exp1, Exp2)->
		
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
					
					case (Type==Type2) and equal(E,E2) of
						true ->
								true;
						false ->
								false
					end;
					
				3 ->
					
					{Type,_,E}=Exp1,
					{Type2,_,E2}=Exp2,
								
					case (Type==var) of
						
						true -> 
								true;
						false ->
								case (Type2==var) of
										
										true ->
												true;
										false ->
												case (Type==Type2) and equal(E,E2) of
													true ->
														true;
													false ->
														false
													end
								end
					end;
										
				4 ->
			
					{Type,_,E,Es}=Exp1,
					{Type2,_,E2,Es2}=Exp2,
					case (Type==Type2) and equal(E,E2) and equal(Es,Es2) of
						true ->
							true;
						false ->
							false
					end;
			
				5 -> 
				
					{Type,_,OP,E1,E2}=Exp1,
					{Type2,_,OP2,E12,E22}=Exp2,
					case (Type==Type2) and equal(OP,OP2) and equal(E1,E12) and equal(E2,E22) of
						true ->
							true;
						false ->
							false
					end
			end;
				
		false ->	
			false
	end.									 
												 
												 
												 
equal3(E1,E2)->
	
 	E1==E2.
												 
												 
												 
%******************equivalenceArgs******************%


equivalenceClassesArgs([Arg|Args], EtsEC, EtsVars,EtsOcTree)->
		
		equivalenceClassesArg(Arg, EtsEC, EtsVars,EtsOcTree),
		equivalenceClassesArgs(Args, EtsEC, EtsVars,EtsOcTree);

equivalenceClassesArgs(Other, EtsEC, EtsVars,EtsOcTree)->
		
		equivalenceClassesArg(Other,EtsEC, EtsVars,EtsOcTree).


%******************equivalenceArg******************%

equivalenceClassesArg({match,LINE,E1,E2},EtsEC, EtsVars,EtsOcTree)->
  	
  		insertEQ({match,LINE,E1,E2},EtsEC, EtsVars,EtsOcTree),
		equivalenceClassesArgs(E1, EtsEC, EtsVars,EtsOcTree),
		equivalenceClassesArgs(E2, EtsEC, EtsVars,EtsOcTree);

equivalenceClassesArg({tuple,LINE,Exps},EtsEC, EtsVars,EtsOcTree)->
  	
  		insertEQ({tuple,LINE,Exps},EtsEC, EtsVars,EtsOcTree),
		equivalenceClassesArgs(Exps, EtsEC, EtsVars,EtsOcTree);
		
		
equivalenceClassesArg({cons,LINE,E1,E2},EtsEC, EtsVars,EtsOcTree)->
  	
  		insertEQ({cons,LINE,E1,E2},EtsEC, EtsVars,EtsOcTree),
		equivalenceClassesArgs(E1, EtsEC, EtsVars,EtsOcTree),
		equivalenceClassesArgsCONS(E2, EtsEC, EtsVars,EtsOcTree);
	
equivalenceClassesArg({atom,LINE,Atom},EtsEC, EtsVars,EtsOcTree)->
  	
  		insertEQ({atom,LINE,Atom},EtsEC, EtsVars,EtsOcTree);
		

equivalenceClassesArg({integer,LINE,Int},EtsEC, EtsVars,EtsOcTree)->
  	
  		insertEQ({integer,LINE,Int},EtsEC, EtsVars,EtsOcTree);


equivalenceClassesArg({op,LINE,Op,E1,E2},EtsEC, EtsVars,EtsOcTree)->
	
	insertEQ({op,LINE,Op,E1,E2},EtsEC, EtsVars,EtsOcTree),
	equivalenceClassesArgs(E1, EtsEC, EtsVars,EtsOcTree),
	equivalenceClassesArgs(E2, EtsEC, EtsVars,EtsOcTree);
	

equivalenceClassesArg({op,LINE,Op,E},EtsEC, EtsVars,EtsOcTree)->
	
	insertEQ({op,LINE,Op,E},EtsEC, EtsVars,EtsOcTree),
	equivalenceClassesArgs(E,EtsEC, EtsVars,EtsOcTree);

equivalenceClassesArg({call,LINE,{remote,LINE,EM,EF},Exps},EtsEC, EtsVars,EtsOcTree)->
	
	insertEQ({call,LINE,{remote,LINE,EM,EF},Exps},EtsEC, EtsVars,EtsOcTree),
	%equivalenceClassesArgs(EM,EtsEC),
	%equivalenceClassesArgs(EF,EtsEC),
	equivalenceClassesArgs(Exps,EtsEC, EtsVars,EtsOcTree);

equivalenceClassesArg({call,LINE,EF,Exps},EtsEC, EtsVars,EtsOcTree)->
	
	insertEQ({call,LINE,EF,Exps},EtsEC, EtsVars,EtsOcTree),
	%equivalenceClassesArg(EF,EtsEC),
	equivalenceClassesArgs(Exps,EtsEC, EtsVars,EtsOcTree);

equivalenceClassesArg(Other, EtsEC, EtsVars,EtsOcTree)->
	Other.


equivalenceClassesArgsCONS({cons,LINE,E1,E2},EtsEC, EtsVars,EtsOcTree)->
		
		equivalenceClassesArgs(E1, EtsEC, EtsVars,EtsOcTree),
		equivalenceClassesArgsCONS(E2, EtsEC, EtsVars,EtsOcTree);

equivalenceClassesArgsCONS(Other,EtsEC, EtsVars,EtsOcTree)->
	Other.

%******************Insert EC******************%

insertEQ(Exp,EtsEC,EtsVars,EtsOcTree)->
	
	case ets:last(EtsEC) of
		'$end_of_table'-> 
			Key=1;
		K-> 
			Key=K+1
	end,
	case ets:last(EtsOcTree) of
		'$end_of_table'-> 
			KeyET=1;
		KET-> 
			KeyET=KET+1
	end,
	case size(Exp) of
		3 ->
			%NewVar=freeVar2(EtsVars,0),
			{Type,LINE,E}=Exp,
			%io:format("Expresion: ~p ",[Exp]),
			io:format("Match: ~p~n",[ets:match(EtsEC,{'$1',{Type,'_',E},'_'})]),
			%io:format("EQ: ~p~n",[ets:match(EtsEC,'$1')]),
			case ets:match(EtsEC,{'$1',{Type,'_',E},'_'}) of 
				[] -> 
					io:format("[]~n",[]),
					%ets:insert(EtsEC,{Key,Exp,NewVar});
					ets:insert(EtsEC,{Key,Exp,[]}),
					
					case ets:match(EtsOcTree,{'$1',{Type,'_',E},'_'}) of
						[] ->
							ets:insert(EtsOcTree,{KeyET,Exp,1});
						
						Matchs ->
							ets:insert(EtsOcTree,{KeyET,Exp,length(Matchs)+1})
					end;


					
				[[KeyRet]] ->
					
					case ets:match(EtsOcTree,{'$1',{Type,'_',E},'_'}) of
						[] ->
							ets:insert(EtsOcTree,{KeyET,Exp,1});
						
						Matchs ->
							ets:insert(EtsOcTree,{KeyET,Exp,length(Matchs)+1})
					end

					%[{_,Ex}]=ets:lookup(EtsEC, KeyRet),
					%io:format("Expresion: ~p Vars: ~p~n ",[Exp,Vars])
					%ets:update_element(EtsEC, KeyRet, {3,Vars++NewVar})
			end;
			
		4 ->
			
			%NewVar=freeVar2(EtsVars,0),
			{Type,LINE,E,Es}=Exp,
			%io:format("Expresion: ~p ",[Exp]),
			io:format("Match: ~p~n",[ets:match(EtsEC,{'$1',{Type,'_',E,Es},'_'})]),
			%io:format("EQ: ~p~n",[ets:match(EtsEC,'$1')]),
			case ets:match(EtsEC,{'$1',{Type,'_',E,Es},'_'}) of 
				[] -> 
					io:format("[]~n",[]),
					ets:insert(EtsEC,{Key,Exp,[]}),
					%ets:insert(EtsOcTree,{KeyET,Exp}),
					case ets:match(EtsOcTree,{'$1',{Type,'_',E,Es},'_'}) of
						[] ->
							ets:insert(EtsOcTree,{KeyET,Exp,1});
						
						Matchs ->
							ets:insert(EtsOcTree,{KeyET,Exp,length(Matchs)+1})
					end;
				[[KeyRet]] -> 
					case ets:match(EtsOcTree,{'$1',{Type,'_',E,Es},'_'}) of
						[] ->
							ets:insert(EtsOcTree,{KeyET,Exp,1});
						
						Matchs ->
							ets:insert(EtsOcTree,{KeyET,Exp,length(Matchs)+1})
					end
					%[{_,Ex}]=ets:lookup(EtsEC, KeyRet),
					%io:format("Expresion: ~p Vars: ~p~n",[Exp,Vars])
					%ets:update_element(EtsEC, KeyRet, {3,Vars++NewVar})
			end;
		5 -> 
			%NewVar=freeVar2(EtsVars,0),
			{Type,LINE,OP,E1,E2}=Exp,
			%io:format("Expresion: ~p ",[Exp]),
			io:format("Match: ~p~n",[ets:match(EtsEC,{'$1',{Type,'_',OP,E1,E2},'_'})]),
			%io:format("EQ: ~p~n",[ets:match(EtsEC,'$1')]),
			case ets:match(EtsEC,{'$1',{Type,'_',OP,E1,E2},'_'}) of 
				[] -> 
					io:format("[]~n",[]),
					%ets:insert(EtsEC,{Key,Exp,NewVar});
					ets:insert(EtsEC,{Key,Exp,[]}),
					case ets:match(EtsOcTree,{'$1',{Type,'_',OP,E1,E2},'_'}) of
						[] ->
							ets:insert(EtsOcTree,{KeyET,Exp,1});
						
						Matchs ->
							ets:insert(EtsOcTree,{KeyET,Exp,length(Matchs)+1})
					end;
					
				[[KeyRet]] -> 

					case ets:match(EtsOcTree,{'$1',{Type,'_',OP,E1,E2},'_'}) of
						[] ->
							ets:insert(EtsOcTree,{KeyET,Exp,1});
						
						Matchs ->
							ets:insert(EtsOcTree,{KeyET,Exp,length(Matchs)+1})
					end
			
					%[{_,Ex}]=ets:lookup(EtsEC, KeyRet),
					%io:format("Expresion: ~p Vars: ~p~n",[Exp,Vars])
					%ets:update_element(EtsEC, KeyRet, {3,Vars++NewVar})
			end
	end.	
	

%******************Existent Variables******************%


extractVars([Exp|ExpsPatterns], Vars)->
	
	extractVars2(Exp,Vars),extractVars(ExpsPatterns, Vars);

extractVars(Other,Vars)->
	ok.

	
extractVars2({var,LINE,Var},Vars)->

	%Vars ++ [{var,LINE,Var}];
	case ets:last(Vars) of
		'$end_of_table'-> Key=1;
		K-> Key=K+1
	end,
				
	case ets:match(Vars,{'_',{'_','_',Var}}) of
		[]->
			ets:insert(Vars,{Key,{var,LINE,Var}});
		_Else ->
			ok
	end;

extractVars2({cons,LINE,E1,E2},Vars) ->
	
	extractVars2(E1,Vars),extractVars2(E2,Vars);
	
extractVars2({tuple,LINE,Exps},Vars) ->
	
	extractVars(Exps,Vars);

extractVars2({match,LINE,E1,E2},Vars) ->
	
	extractVars2(E1,Vars),
	extractVars2(E2,Vars);

extractVars2({op,LINE,Op,E1,E2},Vars)->
	
	extractVars2(E1,Vars),
	extractVars2(E2,Vars);

extractVars2({op,LINE,Op,E},Vars)->
	
	extractVars2(E,Vars);

extractVars2({call,LINE,{remote,LINE,EM,EF},Exps},Vars)->
	
	extractVars2(EM,Vars),
	extractVars2(EF,Vars),
	extractVars2(Exps,Vars);

extractVars2({call,LINE,EF,Exps},Vars)->
	
	extractVars2(EF,Vars),
	extractVars2(Exps,Vars);



extractVars2(Other, Vars)->
	ok.


fillEtsVars(Patterns,Body,EtsVars)->
	extractVars(Patterns,EtsVars),
	%io:format("VarsP: ~p~n",[ets:match(EtsVars,'$1')])
	extractVars(Body,EtsVars).

freeVar(Patterns,Body,EtsVars)->
	
	%extractVars(Patterns,EtsVars),
	%io:format("VarsP: ~p~n",[ets:match(EtsVars,'$1')]),
	%extractVars(Body,EtsVars),
	%io:format("VarsUsed ~p~n",[VarsUsed]),
	%io:format("VarsPB: ~p~n",[ets:match(EtsVars,'$1')]),
	freeVar2(EtsVars,0).


freeVar2(EtsVars,Index)->
	
		
		{ok,[NewVar],_}=erl_scan:string([65+Index]),
		{var,LINE,Var}=NewVar,
		
		case ets:match(EtsVars,{'_',{'_','_',Var}}) of 
			[] -> 
				case ets:last(EtsVars) of
					'$end_of_table'-> 
							Key=1;
					 K-> Key=K+1
				end,
				ets:insert(EtsVars,{Key,NewVar}),
				[NewVar];
			_Else ->
				freeVar2(EtsVars,Index+1)
		end.

freevar3(PGB)->
	
 		 EtsVarsTemp = ets:new('EtsVars',[ordered_set] ),
  		 extractVars(PGB,EtsVarsTemp),
 		 [Newvar]=freeVar2(EtsVarsTemp,0),
		 ets:delete(EtsVarsTemp),
		 Newvar.
		 
	
  
pruebaEV(Rule)->
	
	
	SelectedRule=Rule++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,	
	
	EtsVars = ets:new('EtsVars',[ordered_set] ),
	fillEtsVars(Patterns,Body,EtsVars),
	
	io:format("Last: ~p~n",[ets:last(EtsVars)]),
	io:format("Vars: ~p~n",[ets:match(EtsVars,'$1')]),
	extractVars(Patterns,EtsVars),
	extractVars(Body,EtsVars),
	freeVar(Patterns,Body,EtsVars).
	
