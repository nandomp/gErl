-module(actions).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%     ACTION: INSTANCE(+/-) TO RULE (ETS)     %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instance2rule(Pos,Neg,InstanciasP,InstanciasN,TablaReglas,TablaReglasStep,VarGlob)->
	 %InstanciasP = ets:new('InstanciasP',  [ordered_set] ),
	 %InstanciasN = ets:new('InstanciasN',  [ordered_set] ),
	 %TablaReglas = ets:new('ReglasGen',  [ordered_set] ),
	 ejemplos2ets(Pos,InstanciasP),	 
	 ejemplos2ets(Neg,InstanciasN),
	
	ets:insert(VarGlob, {numPos,ets:last(InstanciasP)}),
	ets:insert(VarGlob, {numNeg,ets:last(InstanciasN)}),
	 
	 ejemplos2etsRules(Pos,TablaReglas,TablaReglasStep,InstanciasP,InstanciasN,VarGlob).
	 %show_next_key( InstanciasP, 0),
	 %io:format("---~n"),
	 %show_next_key( InstanciasN, 0 ).
	 


ejemplos2ets(File,Tabla)->
	case file:open(File, read) of
		{ok,Fd} ->
			ejemplos2ets_linea(Fd,Tabla,1),
			file:close(Fd),
			{ok};
		{error,Motivo} ->
			{error,Motivo}
	end.

ejemplos2ets_linea(Fd,Tabla,Index)->
		case io:get_line(Fd,'') of
			eof ->
			%	io:format("Fin de fichero ~n"),
				ok;
				
			{error, Motivo} ->
				{error, Motivo};
			Texto ->
				%io:format("Texto leido: ~p~n", [Texto]),
				Texto1=string:sub_word(Texto,1,$;),
				Texto2=string:sub_word(Texto1,1,$.),
				Texto3=re:replace(Texto2, " ", "",[global,{return,list}]),
				%io:format("Texto transformado: ~p~n", [Texto2]),			
				ets:insert(Tabla, {Index,{Texto3,0.0,[]}}),
				ejemplos2ets_linea(Fd,Tabla,Index+1)
		end.


ejemplos2etsRules(File,TablaReglas,TablaReglasStep,InstanciasP,InstanciasN,VarGlob)->
	case file:open(File, read) of
		{ok,Fd} ->
			ejemplos2ets_linea_rules(Fd,TablaReglas,TablaReglasStep,InstanciasP,InstanciasN,1,VarGlob),
			file:close(Fd),
			{ok};
		{error,Motivo} ->
			{error,Motivo}
	end.

ejemplos2ets_linea_rules(Fd,Tabla,TablaReglasStep,InstanciasP,InstanciasN,Index,VarGlob)->
		case io:get_line(Fd,'') of
		eof ->
		%	io:format("Fin de fichero ~n"),
			ok;
			
		{error, Motivo} ->
			{error, Motivo};
		Texto ->
			%io:format("--Texto leido: ~p~n", [Texto]),
			Texto1=string:sub_word(Texto,1,$;),
			Texto3=string:sub_word(Texto1,1,$.),
			
			
			
			
			%Texto3=re:replace(Texto2, " ", "",[global,{return,list}]),
			%io:format("Texto transformado: ~p~n", [Texto2]),
			%{Cob,_}=util:cobertura_Funcional(InstanciasP, InstanciasN, Texto3),
			%{CobPos,RulesPosCov,CobNeg,RulesNegCov}=util:cobertura_Funcional(VarGlob, Texto3),
			{CobPos,RulesPosCov,CobNeg,RulesNegCov}={1,[Index],0,[]},
			NumPos=ets:last(InstanciasP),
			{VarsPat,Size,Vars,Cons,Func,Struc,Rec}=sizeRules:sizeOfRuleDecomposed(Texto3),
			OptRule=0.0,
			
			
			
			ets:insert(Tabla, {Index,{Texto3,OptRule,[],CobPos,RulesPosCov,CobNeg,RulesNegCov,Size,Vars,Cons,Func,Struc,Rec,0.0,0.0}}),
			ets:insert(TablaReglasStep,{Index,0}),
			ejemplos2ets_linea_rules(Fd,Tabla,TablaReglasStep,InstanciasP,InstanciasN,Index+1,VarGlob)
	end.


show_next_key( _Instancias, '$end_of_table' ) -> done;

show_next_key( Instancias,  Key) ->
        Next = ets:next( Instancias, Key ),
        io:format( " next ~w ~n ", [ Next ] ),
		io:format("Encontrado: ~p~n",[ets:match( Instancias,  {Next,'$1'})]),
        show_next_key( Instancias, Next ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                             %%
%%        ACTION: ATOM (ANY) TO VARIABLE       %%
%%                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

atom2var(Regla,Pos)->
	SelectedRule=Regla++".",
	{ok,String,_}=erl_scan:string(SelectedRule),
	{ok,Forms}=erl_parse:parse_form(String),
	{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
	New=erl_prettypr:format(erl_syntax:form_list([{function,1,Name,Arity,[{clause,1,atom2varPatterns(Patterns,[],Pos,Arity),Guards,Body}]}])),
	New2=re:replace(New, " ", "",[global,{return,list}]),
	string:sub_word(New2,1,$.).

atom2varPatterns(_,NewPat,_,0)->
	NewPat;

atom2varPatterns(Patterns,NewPat,Pos,Index)->
	
	case Index =/= Pos of
		true ->
			atom2varPatterns(Patterns,[lists:nth(Index,Patterns)]++NewPat,Pos,Index-1);
		false ->
			{ok,NewVar,1}=erl_scan:string([65+Pos-1]),
			atom2varPatterns(Patterns,NewVar++NewPat,Pos,Index-1)
	end.
	

	
	
	 
	 
	 