-module(bk_web).
-export([bk_1/2,bk_2/2]).
%-compile(export_all).


bk_1(Key,Bunch)->
	set_exists(Key,Bunch).


bk_2(Node,Graph)->
	graph_exists(Node,Graph).






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
	
		
graph_exists(Node,Graph)->
	
	graph_exists2(Node,Graph,size(Graph)).

graph_exists2(_,_,0)->
	
	false;

graph_exists2(Node,Graph,Elem)->
	
	{X,Y}=Node,
	
	case (X==[]) or (Y==[]) of
		
		true ->
				false;
			
		false ->
				NodeGraph=element(Elem, Graph),
				{X2,Y2}=NodeGraph,
				BigX=case length(X)>length(X2) of true->BigX=X;false->BigX=X2 end,
				BigY=case length(Y)>length(Y2) of true->BigY=Y;false->BigY=Y2 end,
				case (sets:size(sets:intersection(sets:from_list(X),sets:from_list(X2)))==length(BigX)) and (sets:size(sets:intersection(sets:from_list(Y),sets:from_list(Y2)))==length(BigY)) of 
						
						true->
								
							
								case  (not(sets:is_disjoint(sets:from_list(X),sets:from_list(X2)))) and (not(sets:is_disjoint(sets:from_list(Y),sets:from_list(Y2)))) of
									
									true ->
											true;
									
									false ->
											false
								
								end;
						
						false ->
								graph_exists2(Node,Graph,Elem-1)
				end
	end.
		


	
