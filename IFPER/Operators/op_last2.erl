-module(op_last2).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1,oper_5/1,oper_6/1]).

oper_1(Regla)->		
	operatorsSet:head2varLists(Regla,1).

oper_2(Regla)->	
	operatorsSet:tail2varLists(Regla,1).

oper_3(Regla)->	
	operatorsSet:recAddHead(Regla,1).

oper_4(Regla)->	
	operatorsSet:recAddTail(Regla,1).

oper_5(Regla)->
	operatorsSet:head2rhs(Regla,1).	

oper_6(Regla)->	
 	operatorsSet:tail2rhs(Regla,1).	