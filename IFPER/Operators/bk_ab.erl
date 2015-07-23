-module(bk_ab).
-export([bk_1/1,bk_2/1,bk_3/1]).



bk_1(X)->
	f(X).

bk_2(X)->
	g(X).

bk_3(X)->
	genericfunc(X).



f(X)-> case X of 'a'-> 'b'; X -> X end.

g(X)-> case X of 'b'-> 'a'; X -> X end.

genericfunc(X)->X.







