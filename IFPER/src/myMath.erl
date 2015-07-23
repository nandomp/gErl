-module(myMath).
-compile(export_all).


arithmetic_mean(List) when is_list(List) ->
    lists:sum(List) / length(List).

std_deviation(Values) when is_list(Values) ->
    Mean = arithmetic_mean(Values),
    math:sqrt(arithmetic_mean([ (Val-Mean)*(Val-Mean) || Val <- Values ])).