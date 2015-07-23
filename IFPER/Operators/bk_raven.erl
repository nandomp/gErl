-module(bk_raven).
-compile(export_all).


fig_names(Cell)->
	lists:map(fun ([X|_])-> X end, Cell).
  

atts_names(Cell,Att_id)->
	lists:map(fun (X)-> lists:nth(Att_id+1,X) end, Cell).



identity_obj([H|T])->
	
	[Cell1,Cell2,Cell3]=H,
	Figs_Cell1=fig_names(Cell1),
	Figs_Cell2=fig_names(Cell2),
	Figs_Cell3=fig_names(Cell3),
	case (Figs_Cell1 == Figs_Cell2) and (Figs_Cell2 == Figs_Cell3) of
		true ->
			[H2|T2]=T,
			[Cell12,Cell22]=H2,
			Figs_Cell12=fig_names(Cell12),
			Figs_Cell22=fig_names(Cell22),
			
			case (Figs_Cell12 == Figs_Cell22) of
				true ->
					Figs_Cell12;
				false -> 
					false
			end;
		false ->
			false
	end.

			


missing_obj([H|T])->
	[Cell1,Cell2,Cell3]=H,
	Figs_Cell1=fig_names(Cell1),
	Figs_Cell2=fig_names(Cell2),
	Figs_Cell3=fig_names(Cell3),
	case (Figs_Cell1 =/= Figs_Cell2) and (Figs_Cell2 =/= Figs_Cell3) of
		true ->
			
			[H2|T2]=T,
			[Cell12,Cell22]=H2,
			Figs_Cell12=fig_names(Cell12),
			Figs_Cell22=fig_names(Cell22),
			
			AllFigs=[Figs_Cell1,Figs_Cell2,Figs_Cell3],		
			Figs=[Figs_Cell12,Figs_Cell22],
			
			lists:foldl(fun(X,Acc)-> case lists:member(X, Figs) of true -> Acc; false -> X end end, 0, AllFigs);
		false ->
			false
	end.
		
	
or_figs([H|T]) ->
	 
 	[Cell1,Cell2,Cell3]=H,
	Figs_Cell1=fig_names(Cell1),
	Figs_Cell2=fig_names(Cell2),
	Figs_Cell3=fig_names(Cell3),
	

	Sum= Figs_Cell1 ++ Figs_Cell2,
	case Figs_Cell3 == Sum of 
		true ->
			[H2|T2]=T,
			[Cell12,Cell22]=H2,
			Figs_Cell12=fig_names(Cell12),
			Figs_Cell22=fig_names(Cell22),
			lists:sort(Figs_Cell12++Figs_Cell22);
		false ->
			false
	end.

	
xor_figs([H|T]) ->
	 
 	[Cell1,Cell2,Cell3]=H,
	Figs_Cell1=fig_names(Cell1),
	Figs_Cell2=fig_names(Cell2),
	Figs_Cell3=fig_names(Cell3),
	

	A=sets:from_list(Figs_Cell1),
	B=sets:from_list(Figs_Cell2),
	Union=sets:union(A, B),
	Inter=sets:intersection(A, B),
	XOR=sets:subtract(Union,Inter),
	
	
	case sets:from_list(Figs_Cell3) == XOR of 
		true ->
			[H2|T2]=T,
			[Cell12,Cell22]=H2,
			Figs_Cell12=fig_names(Cell12),
			Figs_Cell22=fig_names(Cell22),
			A2=sets:from_list(Figs_Cell12),
			B2=sets:from_list(Figs_Cell22),
			Union2=sets:union(A2, B2),
			Inter2=sets:intersection(A2, B2),
			lists:sort(sets:to_list(sets:subtract(Union2,Inter2)));
			%sets:subtract(Union2,Inter2);
			
		false ->
			false
	end.


	
inter_figs([H|T]) ->
	 
 	[Cell1,Cell2,Cell3]=H,
	Figs_Cell1=fig_names(Cell1),
	Figs_Cell2=fig_names(Cell2),
	Figs_Cell3=fig_names(Cell3),
	

	A=sets:from_list(Figs_Cell1),
	B=sets:from_list(Figs_Cell2),
	
	Inter=sets:to_list(sets:intersection(A, B)),
		
	case (Figs_Cell3 == Inter) and (Figs_Cell1 =/= Figs_Cell2) of 
		true ->
			[H2|T2]=T,
			[Cell12,Cell22]=H2,
			Figs_Cell12=fig_names(Cell12),
			Figs_Cell22=fig_names(Cell22),
			A2=sets:from_list(Figs_Cell12),
			B2=sets:from_list(Figs_Cell22),			
			lists:sort(sets:to_list(sets:intersection(A2, B2)));
			%sets:intersection(A2, B2);
			
		false ->
			false
	end.


			
			
			
%%%%%%%%%%%%%%%%% Attributes %%%%%%%%%%%%%%%%%%


			
identity_att([H|T],Id)->
	
	[Cell1,Cell2,Cell3]=H,
	Figs_Cell1=atts_names(Cell1,Id),
	Figs_Cell2=atts_names(Cell2,Id),
	Figs_Cell3=atts_names(Cell3,Id),
	case (equal(Figs_Cell1,Figs_Cell2)) and (equal(Figs_Cell2,Figs_Cell3)) of
		true ->
			[H2|T2]=T,
			[Cell12,Cell22]=H2,
			Figs_Cell12=atts_names(Cell12,Id),
			Figs_Cell22=atts_names(Cell22,Id),
			
			case (equal(Figs_Cell12,Figs_Cell22)) of
				true ->
					Figs_Cell12;
				false -> 
					false
			end;
		false ->
			case (equal((Figs_Cell1++Figs_Cell2),Figs_Cell3)) of 
				true ->
					[H2|T2]=T,
					[Cell12,Cell22]=H2,
					Figs_Cell12=atts_names(Cell12,Id),
					Figs_Cell22=atts_names(Cell22,Id),
					lists:sort(Figs_Cell12++Figs_Cell22);
				false->
					false
			end
	end.


	

missing_att([H|T],Id)->
	[Cell1,Cell2,Cell3]=H,
	Figs_Cell1=atts_names(Cell1,Id),
	Figs_Cell2=atts_names(Cell2,Id),
	Figs_Cell3=atts_names(Cell3,Id),
	case (Figs_Cell1 =/= Figs_Cell2) and (Figs_Cell2 =/= Figs_Cell3) of
		true ->
			
			[H2|T2]=T,
			[Cell12,Cell22]=H2,
			Figs_Cell12=atts_names(Cell12,Id),
			Figs_Cell22=atts_names(Cell22,Id),
			
			AllFigs=[Figs_Cell1,Figs_Cell2,Figs_Cell3],		
			Figs=[Figs_Cell12,Figs_Cell22],
			
			lists:foldl(fun(X,Acc)-> case lists:member(X, Figs) of true -> Acc; false -> X end end, 0, AllFigs);
		false ->
			false
	end.
	
	
incr_att([H|T],Id)->
	
	[Cell1,Cell2,Cell3]=H,
	Figs_Cell1=atts_names(Cell1,Id),
	Figs_Cell2=atts_names(Cell2,Id),
	Figs_Cell3=atts_names(Cell3,Id),
	
	NumberAtt=lists:foldr(fun (X,Acc)->case  erlang:is_number(lists:nth(X,Figs_Cell1)) of true -> X; false-> Acc end end, 0, lists:seq(1,length(Figs_Cell1))),
	
	case NumberAtt =/= 0 of
		true ->
				
			Diff1= lists:nth(NumberAtt,Figs_Cell2)-lists:nth(NumberAtt,Figs_Cell1),
			Diff2= lists:nth(NumberAtt,Figs_Cell3)-lists:nth(NumberAtt,Figs_Cell2),
	
			
			case (Diff1 == Diff2) and (Diff1 =/= 0) of
				true->
						[H2|T2]=T,
						[Cell12,Cell22]=H2,
						Figs_Cell12=atts_names(Cell12,Id),
						Figs_Cell22=atts_names(Cell22,Id),
						Nth_elem= lists:nth(NumberAtt,Figs_Cell22),
					
						lists:map(fun(X)-> case X==Nth_elem of true -> Nth_elem + Diff1; false -> X end end ,Figs_Cell22);
				false ->
					false								
			
			end;
		false ->
			false
	end.

										
					
equal(L1,L2)->
	case length(L1)==length(L2) of
		true->
			L1==L2;
		false->
			case length(L1)<length(L2)of
				true ->
					(lists:suffix(L1, L2) or lists:prefix(L1, L2));
				false ->
					(lists:suffix(L2, L1) or lists:prefix(L2, L1))
			end
	end.
	
	
	
	
	
	
	
	
	
	
	