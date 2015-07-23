-module(trees).
-compile(export_all).
-type branch() :: {Key::term(), Value::term(), Tree::term()}.
-type path() :: {Start::pos_integer(), branch()}.
-type tree() :: [branch()].
-spec miarbol(tree()) -> tree().
-spec foo(pos_integer()) -> pos_integer().
%main()->
	
	
	%Tree=gb_trees:empty(),
	%Tree2=gb_trees:insert(1,a,Tree),
	%Tree3=gb_trees:insert(2,b,Tree2),
%	gb_trees:to_list(Tree3).

	%EtsEC = ets:new('EtsEC',[ordered_set]),
	%ets:insert(EtsEC,{1,{integer,1,0},[{var,1,'A'},{var,1,'E'}]}),
	%%ets:insert(EtsEC,{2,{cons,1,{atom,1,s},{cons,1,{integer,1,0},{nil,1}}},[{var,1,'B'}]}),
	%ets:insert(EtsEC,{3,{atom,1,s},[{var,1,'C'}]}),
	%ets:insert(EtsEC,{4,{cons,1,{integer,1,0},{nil,1}},[{var,1,'D'}]}),
	%io:format("EQ: ~p~n",[ets:match(EtsEC,'$1')]),
	
	%ets:match(EtsEC,{'$1',{cons,1,{integer,1,0},{nil,1}},'_'}),
	%ets:match(EtsEC,{'$1',{cons,1,{atom,1,s},{cons,1,{integer,1,0},{nil,1}}},'_'}).
	%ets:match(EtsEC,{'$1',{atom,1,s},'_'}).
	
	
	miarbol(A)->A.
		
	foo(-9)->-1.	

	
	


	compose(F,G) -> fun(X) -> F(G(X)) end.
 
	multicompose(Fs) -> 
   		 lists :foldl(fun compose/2, fun(X) -> X end, Fs).


	f(List)-> lists:map(fun(X) when X=='a'-> b; (X) -> X end, List).

	g(List)-> lists:map(fun(X) when X=='b'-> a; (X) -> X end, List).

	

	a(X)->f(X);
	a([a,b,b])->[b,b,b];
	a([b,a,a,a,b])->[b,b,b,b,b].

	