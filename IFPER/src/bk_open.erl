-module(bk_open).
-export([bk_1/2]).
%-compile(export_all).


bk_1(Key,Bunch)->
	set_exists(Key,Bunch).

set_exists(KeyAtts,Bunch) ->
	
	set_exists2(KeyAtts,Bunch,length(Bunch)).

set_exists2(_,_,0)->
	
	false;

set_exists2(KeyAtts,Bunch,Elem)->
	
	KeyBunch=lists:nth(Elem, Bunch),
	Patts= length(KeyAtts),
	
	case existsKinB(KeyAtts,KeyBunch) of
		
		Patts ->
			
			true;
		
		 _Else->
			
			set_exists2(KeyAtts,Bunch,Elem-1)
	end.


existsKinB(KeyAtts,KeyBunch)->
  
  lists:foldl(fun (X,Sum) -> Y=temp(KeyBunch,X), Sum+Y end, 0, KeyAtts).


		
temp(KeyBunch,X)->
	
	temp2 (KeyBunch,X,size(KeyBunch),0).

temp2(_,_,0,Res)->
		
	Res;

temp2 (KeyBunch,X,Elem,Res)->

	case erlang:element(Elem, KeyBunch)==X of
		
		true ->
				temp2(KeyBunch,X,0,1);

		false ->
				temp2(KeyBunch,X,Elem-1,0)
	end.
	
		
	
	
equal(Exp1,Exp2)->
	
	
	%io:format("Exp1s ~p~n",[Exp1]),
	%io:format("Exp2s ~p~n",[Exp2]),
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
												 
								
		
	
	
	


