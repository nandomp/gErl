-module(suma).
-compile(export_all).


%suma([0],X) -> io:format("Caso Base ~n",[]), X;

suma([s,s,0],[s,s,0]) -> [s]++suma([s,0],[s,s,0]);
  
%suma([s|T1],X)-> io:format("General ~n",[]),[s]++suma(T1,X);

suma([0],[0]) -> io:format("Especifica ([0],[0]) ~n",[]),[0];

suma([s,0],[s,0]) -> io:format("Especifica ([s,0],[s,0]) ~n",[]),[s,s,0];

suma([0],[s,0]) -> io:format("Especifica ([0],[s,0]) ~n",[]),[s,0];

suma([s,s,0],[0]) -> io:format("Especifica ([s,s,0],[0])~n",[]),[s,s,0];

suma([s,0],[0]) -> io:format("Especifica ([s,0],[0])~n",[]),[s,0];

%suma([s,0],[s,s,0]) -> io:format("Especifica ([s,0],[s,s,0])~n",[]),[s,s,s,0];

suma([s,s,0],[s,s,0]) -> io:format("Especifica ([s,s,0],[s,s,0]) ~n",[]),[s,s,s,s,0];

suma([s,0],[0]) -> io:format("Especifica ([s,0],[0])~n",[]),[s,0];

suma([s,s,s,0],[s,0]) -> io:format("Especifica ([s,s,s,0],[s,0])~n",[]),[s,s,s,s,0];

suma([s,s,s,0],[s,s,0]) -> io:format("Especifica ([s,s,s,0],[s,s,0])~n",[]),[s,s,s,s,s,0].
	

%suma([],X) -> X;

%suma(X,[]) -> X;

%suma([s|T1],[s|T2])->
	
%	[s,s]++suma(T1,T2);

%suma([0|_],[s|T2]) ->
	
%	[s]++T2;

%suma([s|T1],[0|_]) ->
	
%	[s]++T1;

%suma([0|_],[0|_]) ->
	
%	[0].

prueba()->
	
	io:format("suma([0],[0])= ~w~n",[suma([0],[0])]),
	io:format("suma([s,0],[s,0])= ~w~n",[suma([s,0],[s,0])]),
	io:format("suma([0],[s,0])= ~w~n",[suma([0],[s,0])]),
	io:format("suma([s,s,0],[0])= ~w~n",[suma([s,s,0],[0])]),
	io:format("suma([s,0],[0])= ~w~n",[suma([s,0],[0])]),
	io:format("suma([s,0],[s,s,0])= ~w~n",[suma([s,0],[s,s,0])]),
	io:format("suma([s,s,0],[s,s,0])= ~w~n",[suma([s,s,0],[s,s,0])]),
	io:format("suma([s,0],[0])= ~w~n",[suma([s,0],[0])]),
	io:format("suma([s,s,s,0],[s,0])= ~w~n",[suma([s,s,s,0],[s,0])]),
	io:format("suma([s,s,s,0],[s,s,0])= ~w~n",[suma([s,s,s,0],[s,s,0])]).
	



	