-module(tsv).

-export([header/1, data/1, record/1]).

header([]) -> $\n;
header([E]) -> [E, $\n];
header([E|Es]) -> [E, $\t | header(Es)].

data([]) -> [];
data([T|Ts]) when is_tuple(T) ->
    Record = record(tuple_to_list(T)),[element(E) || E <- tuple_to_list(T)],
    [Record, $\n | data(Ts)].
    
record([]) -> [];
record([E]) -> [element(E)];
record([E|Es]) -> [element(E), $\t | record(Es)].

element(B) when is_binary(B) -> B;
element(F) when is_float(F) -> float_to_binary(F);
element(I) when is_integer(I) -> integer_to_binary(I).
