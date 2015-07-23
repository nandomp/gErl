-module(testsmerl).
-compile(export_all).


test()->
A="playtennis(a,b,c,d)->yes",

V=lists:append("cubre()->","try p(a,b,c) of Val -> Val catch _:_ -> io:format(\"false\",[]) end."),
P1 = smerl:new(rule1),
{ok, P2} = smerl:add_func(P1, "p(a,B,c)->todoway."),
{ok, P3} = smerl:add_func(P2,V),
smerl:compile(P3),

Sol=rule1:cubre(),

io:format("La sol ha sido: ~w~n",[Sol]).

%P3 = smerl:new(rule2),
%{ok, P4} = smerl:add_func(P3, "p(a,B,c)->true."),
%smerl:compile(P4),

%P5 = smerl:new(rule3),
%{ok, P6} = smerl:add_func(P5, "p(a,B,c)->true."),
%smerl:compile(P6),



%% 'false' indicates to not export the function
%{ok, P3} = smerl:add_func(P2, "p(a,b,d)->true."),

%{ok, P4} = smerl:add_func(P3, "p(a,B,c)->true."),
%smerl:compile(P2),
%Bump=fun(X) -> X+1 end,

%V=list_to_atom("rule1:p(a,b,c)"),


%try V of
%	Val -> io:format("true: ~w~n",[Val])
%catch
%	_:_ -> io:format("false",[])
%end.

%try rule2:p(f,b,c) of
%	Val2 -> io:format("true: ~w~n",Val2)
%catch
%	_:_ -> io:format("false~n",[])
%end,

%try rule3:p(f,b,c) of
%	Val3 -> io:format("true: ~w~n",[Val3])
%catch
%	_:_ -> io:format("false~n",[])
%end.



test2()->
	
P1 = smerl:new(parent),
{ok, P2} = smerl:add_func(P1, "add(A, B) -> A+B."),

%% 'false' indicates to not export the function
{ok, P3} = smerl:add_func(P2, "subtract(A, B) -> A-B.", false),

{ok, P4} = smerl:add_func(P3, "multiply(A,B) -> A*B."),
smerl:compile(P4),

C1 = smerl:new(child),
{ok, C2} = smerl:add_func(C1,
   "multiply(A,B) -> throw(not_allowed)."),

C3 = smerl:extend(P4, C2),
smerl:compile(C3),

child:add(3,4).

test3()->
	
	{ok,Meta_model} = smerl:for_module(fact),
	Form = smerl:get_forms(Meta_model),
	io:format("~p~n",[Form]).


test4()->
	{ok,Meta_model} = smerl:for_module(fact),
	[Form||Form = {funtion,_,_,_,_} <- smerl:get_forms(Meta_model)].
	%io:format("~p~n",[Form]).


class() -> 
			A= [{circle,1},{triangle,1},{{square,circle},1}],
			
			lists:foldr(fun({X,_},Class)-> if X=={triangle,circle} -> positive; true -> Class end end, negative, A).
				
			


	

