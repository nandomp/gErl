-module(operators).
-compile(export_all).

new_operators(File)->
	{ok,OperatorsModule} = compile:file(File),
	%io:format("Compilado ~n",[]),
	{ok,Meta_model} = smerl:for_module(OperatorsModule),
	Op=smerl:get_exports(Meta_model),
	io:format("Exports: ~p~n",[Op]),
	Num=length(Op),
	TableOperators=ets:new('Operators',  [ordered_set] ),
	
	[ets:insert(TableOperators, {Index,{element(1,lists:nth(Index,Op)),element(2,lists:nth(Index,Op)),1.0,1}})||Index<-lists:seq(1,Num) ],
																				 
	{TableOperators,Num,OperatorsModule}.




	
	
	

