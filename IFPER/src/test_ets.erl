-module(test_ets).
-compile(export_all).



start() ->
        Instancias = ets:new( 'magic',  [ordered_set] ),
        % note: table is not square
        populate( Instancias, [{1,"playtennis(a,b,c)"},{2,"playtennis(a,z,c)"},{3,"playtennis(a,z,y)"},{4,"playtennis(A,z,y)"}] ),
        Member = ets:member( Instancias, 2 ),
        io:format( " member ~w ~n ", [ Member ] ),
        show_next_key( Instancias, 2 ),
        find_animal( Instancias, "playtennis(a,z,y)" ).
        
show_next_key( _Instancias, '$end_of_table' ) -> done;

show_next_key( Instancias,  Key) ->
        Next = ets:next( Instancias, Key ),
        io:format( " next ~w ~n ", [ Next ] ),
        show_next_key( Instancias, Next ).

populate( _Instancias, [] ) -> {done,start};
populate( Instancias, [H | T] ) ->
                ets:insert( Instancias, H ),
                populate( Instancias, T ).
        
find_animal( Instancias, Play ) ->
        ets:match( Instancias, { '$1', Play } ).



mod()->
Actions = ets:new('Actions',  [ordered_set] ),
	ets:insert(Actions, {1,{"atom1var",1,0.7,34}}),
	ets:insert(Actions, {2,{"atom2var",1,0.5,56}}),
	ets:insert(Actions, {3,{"atom3var",1,0.6,44}}),
	ets:insert(Actions, {4,{"atom4var",1,0.99,77}}),

	io:format("~p~n",[ets:match(Actions,'$1')]),
	ets:tab2file(Actions, "EtsSAVED"),
	%ets:update_element(Actions, 4, {2,{"atom4var",222,2}}),
	%Match=ets:match( TabNeg,  {Actions,{1,{"atom2var",Rew,Veces}}),
	{ok,TAB}=ets:file2tab( "EtsSAVED"),
	io:format("Actions ~p~n",[ets:match(Actions,'$1')]),
	io:format("TAB ~p~n",[ets:match(TAB,'$1')]),
	ListTaB=ets:tab2list(TAB),
	Ordered=lists:sort(fun(A,B)-> element(3,element(2,A))>=element(3,element(2,B)) end, ListTaB),

	OpsOrd = ets:new('OpsOrd',  [set] ),
	io:format("Ordered ~p~n",[Ordered]),
	[ets:insert(OpsOrd,I)|| I<-Ordered],	
	io:format("OpsOrd ~p~n",[ets:match(OpsOrd,'$1')]),

	ets:lookup(OpsOrd, 1).
		



mod2()->
Reglas = ets:new('Reglas',  [ordered_set] ),
	ets:insert(Reglas, {1,{"playtennis(overcast,hot,high,weak) -> yes",1,0}}),
	ets:insert(Reglas, {2,{"playtennis(rain,mild, high,weak) -> yes",1,0}}),
	ets:insert(Reglas, {3,{"playtennis(rain,cool,normal,weak) -> yes",2,0}}),
	ets:insert(Reglas, {4,{"playtennis(overcast,cool,normal,strong) -> yes",1,0}}).
	%P="playtennis(overcast" ++ '$2',
%	io:format("~p~n",[ets:match(Reglas,{'$1',{"playtennis("++ '$1'++ ",cool,normal,strong) -> yes",1,0}})]),

	%ets:update_element(Reglas, 4, {2,{"playtennis(overcast,cool,normal,strong) -> yes",222,2}}).

	%Match=ets:match( TabNeg,  {Actions,{1,{"atom2var",Rew,Veces}}),
	
	%io:format("~p~n",[ets:match(Reglas,'$1')]).

	%Miregla="playtennis(a,b,C,f(d))->yes".
	%{ok,T,_}=erl_scan:string(Miregla).
	%parser_reglas(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eliminarcomas(Regla,0)->
	Regla;
  
eliminarcomas(Regla,Index)->
	case lists:nth(Index,Regla) of
		{',',1} ->
			ReglaNueva=lists:delete({',',1},Regla),
			eliminarcomas(ReglaNueva,Index-1);
		_else ->
			eliminarcomas(Regla,Index-1)
	end.
			

aridad(Regla,Ini,Parentesis,Aridad,Index)->
	%io:format("Index: ~p~n",[Index]),
	%io:format("length: ~p~n",[length(Regla)]),
	%io:format("parentesis: ~p~n",[Parentesis]),
	case Index == (length(Regla)+1) of
		false ->
			%io:format("nth: ~p~n",[lists:nth(Index,Regla)]),
			case lists:nth(Index,Regla) of
				{')',_} ->
					%io:format("parentesis ) ~n",[]),
					case Parentesis==0 of
						true ->
							%io:format("Devuelvo aridad ~p~n",[Aridad]),
							{Index,Aridad};
						false ->
							aridad(Regla,Ini,Parentesis-1,Aridad,Index+1)
					end;
		
				{'(',_} ->
						%io:format("parentesis (  ~n",[]),
						{LastIndex,OtraAridad}=aridad(Regla,Index+1,0,0,Index+1),
						%io:format("OtraAridad:  ~w~n",[OtraAridad]),
						aridad(Regla,Ini,Parentesis+1,Aridad,LastIndex);
		
				_else -> 
						%io:format("Otro ~n",[]),
						aridad(Regla,Ini,Parentesis,Aridad+1,Index+1)
			end;
		true ->
			errrrrorrrr
	end.
		
				
atributos(Regla,Ini,Parentesis,Atributos,AtribParcial,Index)->
	
	
	case Index == (length(Regla)+1) of
		false ->
			%io:format("nth: ~p~n",[lists:nth(Index,Regla)]),
			case lists:nth(Index,Regla) of
				{')',_} ->
					%io:format("parentesis ) ~n",[]),
					case Parentesis==1 of
						true ->
							Atrib=lists:append(Atributos,AtribParcial),
							%io:format("Devuelvo atributos ~p~n",[Atrib]),
							{Index,Atrib};
						false ->
							Atrib=lists:append(Atributos,AtribParcial),
							%io:format("Atrib: ~w~n",[Atrib]),
							atributos(Regla,Ini,Parentesis-1,Atrib,[],Index+1)
					end;
				
				
				{var,_,Var} ->
					%io:format("variable ~n",[]),
						
						AtribP=lists:append(AtribParcial,[{var,Var}]),
						%io:format("AtribP: ~w~n",[AtribP]),
						atributos(Regla,Ini,Parentesis,Atributos,AtribP,Index+1);
									
		
				{atom,_,Atom} ->
					
					case lists:nth(Index+1,Regla) of 
						
				  		{'(',_} ->
							%io:format("parentesis (  ~n",[]),
							{_,Arid}=aridad(Regla,Index+2,0,0,Index+2),
							{LastIndex,AtribFun}=atributos(Regla,Index+1,1,[],[],Index+1),
							AtribFun2=[{func,Arid,Atom,AtribFun}],
							%io:format("Atribfun: ~w~n",[AtribFun2]),
							AtribPar=lists:append(AtribParcial,AtribFun2),
							%io:format("AtribPar: ~w~n",[AtribPar]),
							atributos(Regla,Ini,Parentesis+1,Atributos,AtribPar,LastIndex);
			
		
						_else -> 
							%io:format("Otro ~n",[]),
							AtribP=lists:append(AtribParcial,[{atom,Atom}]),
							%io:format("AtribP: ~w~n",[AtribP]),
							atributos(Regla,Ini,Parentesis,Atributos,AtribP,Index+1)
					end;
			
				_else ->
							atributos(Regla,Ini,Parentesis,Atributos,AtribParcial,Index+1)


			end;
		true ->
			{errorrrr,errrorrrrrrr}
	end.



coberturaFunc(A,B)->
	
	FuncA=element(3,A),
	FuncB=element(3,B),
	AridadA=element(2,A),
	AridadB=element(2,B),
	
	case (FuncA==FuncB) and (AridadA==AridadB) of
		true ->
			ArgsA=element(4,A),
			ArgsB=element(4,B),
			coberturaArgs(ArgsA,ArgsB,AridadA,1);	
	
		false ->
			false
	end.
	
	


coberturaArgs(ArgsA,ArgsB,Aridad,Index)->
	case Index == (Aridad +1) of
		false->
			%io:format("ArgsA: ~w~n",[ArgsA]),
			%io:format("elemento: ~w~n",[lists:nth(Index,ArgsA)]),
			case lists:nth(Index,ArgsA) of	
				{atom,AtomA}->
					
					case lists:nth(Index,ArgsB) of	
						{atom,AtomB} -> 
							case AtomA==AtomB of
								true->
									coberturaArgs(ArgsA,ArgsB,Aridad,Index+1);
								false ->
									false					
							end;
						{var,_} ->
								coberturaArgs(ArgsA,ArgsB,Aridad,Index+1);
						_else ->
								false
					end;
				{var,_}->
					case lists:nth(Index,ArgsB) of	
						{atom,_} -> 
							coberturaArgs(ArgsA,ArgsB,Aridad,Index+1);
						{var,_} ->
							coberturaArgs(ArgsA,ArgsB,Aridad,Index+1);
						{func,_,_,_} ->
							coberturaArgs(ArgsA,ArgsB,Aridad,Index+1);
						_else ->
							false
					end;
				{func,_,_,_} ->
					case lists:nth(Index,ArgsB) of
						{func,_,_,_} ->
							coberturaFunc(lists:nth(Index,ArgsA),lists:nth(Index,ArgsB));
						{var,_}->
							coberturaArgs(ArgsA,ArgsB,Aridad,Index+1);
						_else ->
							false
					end
			end;
		true->
			true
	end.

%% Regla 1 debe ser MAS GENERAL que regla 2 %%
cobertura(Regla1,Regla2)->
	{ok,Terms1,_}=erl_scan:string(Regla1),
	{ok,Terms2,_}=erl_scan:string(Regla2),
	ReglaLimpia1=eliminarcomas(Terms1,length(Terms1)),
	ReglaLimpia2=eliminarcomas(Terms2,length(Terms2)),
	io:format("Regla 1 ~w~n",[ReglaLimpia1]),
	io:format("Regla 2 ~w~n",[ReglaLimpia2]),
	io:format("Aridad1 ~w~n",[aridad(ReglaLimpia1,3,0,0,3)]),
	io:format("Aridad2 ~w~n",[aridad(ReglaLimpia2,3,0,0,3)]),
	
	%% La funcion Atributos devuelve tupla {lenght,atributos}
	R1=element(2,atributos(ReglaLimpia1,1,0,[],[],1)),
	R2=element(2,atributos(ReglaLimpia2,1,0,[],[],1)),
	io:format("R1:  ~w~n",[R1]),
	io:format("R2:  ~w~n",[R2]),

	Cabeza1=lists:nth(1,R1),
	Cabeza2=lists:nth(1,R2),
	io:format("Cabeza1 ~w~n",[Cabeza1]),
	io:format("Cabeza2 ~w~n",[Cabeza2]),
	coberturaFunc(Cabeza1,Cabeza2).


	
	
		
	
	
	
	testPrograms()->
	
	Programs2 = ets:new('Programs2',[ordered_set]),
		ets:insert(Programs2, {1,{1,[1,2,dsad3],0}}),
		ets:insert(Programs2, {2,{[2],[dasd4,5,6],0}}),
		ets:insert(Programs2, {3,{[3],[3,dasdasd,1],0}}),

	
		Programs = ets:new('Programs',[ordered_set]),
		%ets:insert(Programs, {1,{1,[1,2,3],0}}),
		%ets:insert(Programs, {2,{[2],[4,5,6],0}}),
		%ets:insert(Programs, {3,{[3],[3,2,1],0}}),
		%ets:insert(Programs, {4,{[4],[2,1,5],0}}),
		%ets:insert(Programs, {5,{[5],[4,7,1],0}}),
		ets:insert(Programs, {a,123}),
		ets:insert(Programs, {b,fdfdfsad}),
		ets:insert(Programs, {c,[a,v,sd]}),
		ets:insert(Programs, {programs2,Programs2}),
		
		io:format("~p~n",[ets:match(Programs,'$1')]),
		ets:lookup(Programs, 123),
		ets:lookup_element(Programs, c, 2),
			
		io:format("~p~n",[ets:lookup(ets:lookup_element(Programs, programs2, 2), 2)]),
		
		io:format("~p~n",[ets:match(ets:lookup_element(Programs, programs2, 2),'$1')]),
		%Tabla=ets:lookup_element(Programs, programs2, 2),
		ets:update_element(ets:lookup_element(Programs, programs2, 2), 2, {2,holaaaa}),
		
		ets:insert(ets:lookup_element(Programs, programs2, 2),{5,{[5],[4,7,1],0}}),
		io:format("~p~n",[ets:match(ets:lookup_element(Programs, programs2, 2),'$1')]),
			
		ets:update_element(Programs, c, {2,yanoeslista}),
		io:format("~p~n",[ets:match(Programs,'$1')]),
		
		func(ets:lookup_element(Programs, programs2, 2)),
		io:format("~p~n",[ets:match(ets:lookup_element(Programs, programs2, 2),'$1')]).
		
		
		%io:format("~p~n",[ets:match(Programs,{'$1',{'$2',[2,1,3],'$3'}})]).
	
	
func(Tabla)->
		ets:insert(Tabla,{6,{[nuevafuncion],[4,7,1],0}}).	   
			   
first([])->[];	
	
first([A|B])->A.



ets_id()->
	Tabla = ets:new('Tabla',[ordered_set]),
	ets:insert(Tabla, {a,b,3,{hola, que, tal, 4}}),
	ets:insert(Tabla, {c,3,4,{hola, que, tal, 5}}),
	ets:insert(Tabla, {2,3,9,{hola, que, tal, 6}}),
	ets:insert(Tabla, {c,4,45,{hola, que, tal, 7}}),

	io:format("~p~n~n~n",[ets:lookup(Tabla, {c,3,4})]),
	
	io:format("~p~n",[ets:match(Tabla,'$1')]),

	ets:match(Tabla, {c,'$1','$2','_'}).
	
	

t()->
	A= sets:new(),
	A2=sets:add_element(4, A),
	B= ets:new('global', [ordered_set]),
	ets:insert(B, {a,A2}),
	A3=sets:add_element(3, ets:lookup_element(B, a, 2)),
	%ets:update_element(VarGlob, listUnions, {2,NewUnions}),
	ets:update_element(B, a, {2,A3}),
	sets:to_list(ets:lookup_element(B, a, 2)).
