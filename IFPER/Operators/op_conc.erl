-module(op_conc).
-export([oper_1/1,oper_2/1,oper_3/1,oper_4/1]).



oper_1(Regla)->
	operatorsSet:att2var(Regla,1).

oper_2(Regla)->
	operatorsSet:att2var(Regla,2).

oper_3(Regla)->
	operatorsSet:att2rhs_del(Regla,operatorsSet:giveNatt_lhs(Regla,1)).

%oper_4(Regla)->
%	operatorsSet:att2rhs_del(Regla,operatorsSet:giveNatt_lhs(Regla,2)).

%oper_5(Regla)->
%	operatorsSet:conc2rhs(Regla,operatorsSet:giveNatt_lhs(Regla,1)).

oper_4(Regla)->
	operatorsSet:conc2rhs(Regla,operatorsSet:giveNatt_lhs(Regla,2)).