-module(rand).
-compile(export_all).

f()->
	random:seed(1,2,3),
	Value= random:uniform(),
	io:format("Value: ~p~n", [Value]),
	g().

g()->
	Value2=random:uniform(),
	io:format("Value en func: ~p~n", [Value2]).

