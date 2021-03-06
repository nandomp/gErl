-module(testProg).
-compile(export_all).


testOld()->
	
	Progs= ets:new('Progs', [ordered_set] ),
	ets:insert(Progs, {1,{sets:from_list([1,2,3]),sets:from_list([1,2,3]),sets:from_list([]),1}}),
	ets:insert(Progs, {2,{sets:from_list([1,2]),sets:from_list([1,2]),sets:from_list([]),1}}),
	ets:insert(Progs, {3,{sets:from_list([5]),sets:from_list([5]),sets:from_list([]),1}}),
	ets:insert(Progs, {4,{sets:from_list([2,5]),sets:from_list([2,5]),sets:from_list([]),1}}),
	MatrixPrograms = ets:new('MatrixProgs',  [ordered_set] ),
	
	
	
	
	programs:gen_matrix_overlap(Progs, 5, MatrixPrograms).
		


test()->
	MatrixPrograms = ets:new('MatrixProgs',  [ordered_set] ),
	
	ets:insert(MatrixPrograms,[{1,{1,2,0.17142857142857143}},
 {2,{1,3,0.17142857142857143}},
 {3,{1,4,0.17142857142857143}},
 {4,{1,5,0.17142857142857143}},
 {5,{1,6,0.17142857142857143}},
 {6,{1,7,0.17142857142857143}},
 {7,{2,3,0.17142857142857143}},
 {8,{2,4,0.17142857142857143}},
 {9,{2,5,0.17142857142857143}},
 {10,{2,6,0.17142857142857143}},
 {11,{2,7,0.17142857142857143}},
 {12,{3,4,0.17142857142857143}},
 {13,{3,5,0.17142857142857143}},
 {14,{3,6,0.17142857142857143}},
 {15,{3,7,0.17142857142857143}},
 {16,{4,5,0.17142857142857143}},
 {17,{4,6,0.17142857142857143}},
 {18,{4,7,0.17142857142857143}},
 {19,{5,6,0.17142857142857143}},
 {20,{5,7,0.17142857142857143}},
 {21,{6,7,0.17142857142857143}},
 {22,{7,8,0.058095238095238075}},
 {23,{6,8,0.058095238095238075}},
 {24,{5,8,0.058095238095238075}},
 {25,{4,8,0.058095238095238075}},
 {26,{3,8,0.058095238095238075}},
 {27,{2,8,0.058095238095238075}},
 {28,{1,8,-0.08476190476190479}},
 {29,{8,9,0.058095238095238075}},
 {30,{7,9,0.07476190476190474}},
 {31,{6,9,0.16047619047619044}},
 {32,{5,9,0.16047619047619044}},
 {33,{4,9,0.16047619047619044}},
 {34,{3,9,0.16047619047619044}},
 {35,{2,9,0.16047619047619044}},
 {36,{1,9,0.01761904761904759}},
 {37,{9,10,0.16047619047619044}},
 {38,{8,10,0.16047619047619044}},
 {39,{7,10,0.17142857142857143}},
 {40,{6,10,0.17142857142857143}},
 {41,{5,10,0.2571428571428571}},
 {42,{4,10,0.2571428571428571}},
 {43,{3,10,0.2571428571428571}},
 {44,{42,1,0.2571428571428571}},
 {45,{1,10,0.2571428571428571}},
 {46,{10,11,0.14380952380952378}},
 {47,{9,11,0.030476190476190435}},
 {48,{8,11,-0.05523809523809528}},
 {49,{7,11,0.058095238095238075}},
 {50,{6,11,0.058095238095238075}},
 {51,{5,11,0.058095238095238075}},
 {52,{4,11,0.058095238095238075}},
 {53,{3,11,0.058095238095238075}},
 {54,{2,11,-0.08476190476190479}},
 {55,{1,11,0.058095238095238075}},
 {56,{11,12,0.1549206349206349}},
 {57,{10,12,0.16603174603174597}},
 {58,{9,12,0.1606349206349206}},
 {59,{18,1,0.1606349206349206}},
 {60,{7,12,0.16603174603174597}}]),
	
	Used= gb_sets:from_list([{1,2},{42,1},{4,3},{2,3},{2,4},{5,2}]),
	
	A= ets:tab2list(MatrixPrograms),
	
	%A=ets:match(MatrixPrograms,{'$1',{1,'$2','$3'}}) ++ ets:match(MatrixPrograms,{'$1',{'$2',1,'$3'}}),
	
	io:format("Vector previo Filter: ~p ~n",[A]),
	
	%lists:map(fun ([Id, P1, Inc1]) -> ((gb_sets:is_element({P1,1},Used)==false) and (gb_sets:is_element({1,P1},Used)== false)) end end , A).

	[H|T]= lists:filter(fun ({Idf,{ P1f, P2f, Inc}}) -> ((gb_sets:is_element({P1f,P2f},Used)==false) and (gb_sets:is_element({P2f,P1f},Used)== false)) end, A),
	io:format("Vector post MAp: ~p ~n",[[H|T]]),
	
	{IdRet,{P1Ret,P2Ret,IncRet}}=lists:foldl(fun ({Id1,{P11,P21,Inc1}},{_,{_,_,Inc2}}) when Inc1 >= Inc2 -> {Id1,{P11,P21,Inc1}}; (_,{Id2,{P12,P22,Inc2}})->{Id2,{P12,P22,Inc2}} end, H, T).
	
	