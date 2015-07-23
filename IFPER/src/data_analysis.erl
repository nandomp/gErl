-module(data_analysis).
-compile(export_all).

analysis(Pos, Neg)->
	
	InstanciasP = ets:new('InstanciasP',  [ordered_set] ),
	InstanciasN = ets:new('InstanciasN',  [ordered_set] ),
 	ejemplos2ets(Pos,InstanciasP),	
	ejemplos2ets(Neg,InstanciasN),
  	io:format("~p~n",[ets:match(InstanciasP,'$1')]),
	io:format("~p~n",[ets:match(InstanciasN,'$1')]),
	
	stats_attribs(InstanciasP,InstanciasN).

	
	
	

ejemplos2ets(File,Tabla)->
	case file:open(File, read) of
		{ok,Fd} ->
			ejemplos2ets_linea(Fd,Tabla,1),
			file:close(Fd),
			{ok};
		{error,Motivo} ->
			{error,Motivo}
	end.

ejemplos2ets_linea(Fd,Tabla,Index)->
		case io:get_line(Fd,'') of
			eof ->
			%	io:format("Fin de fichero ~n"),
				ok;
				
			{error, Motivo} ->
				{error, Motivo};
			Texto ->
				%io:format("Texto leido: ~p~n", [Texto]),
				Texto1=string:sub_word(Texto,1,$;),
				Texto2=string:sub_word(Texto1,1,$.),
				Texto3=re:replace(Texto2, " ", "",[global,{return,list}]),
				
				SelectedRule=Texto3++".",
				{ok,String,_}=erl_scan:string(SelectedRule),
				{ok,Forms}=erl_parse:parse_form(String),
				{function,1,Name,Arity,[{clause,1,Patterns,Guards,Body}]}=Forms,
							
				%io:format("Texto transformado: ~p~n", [Texto2]),			
				ets:insert(Tabla, {Index,{Texto3,Arity,Patterns,Guards,Body}}),
				ejemplos2ets_linea(Fd,Tabla,Index+1)
		
		end.


stats_attribs(InstanciasP,InstanciasN)->v.
	
	

















