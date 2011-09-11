-module(kirby_test_util).

-export([random_binary/1]).

random_binary(Size) ->
    {T1, T2, T3} = erlang:now(),
    random:seed(T1, T2, T3),
    generate_binary(Size, []).

generate_binary(0, Accum) ->
    list_to_binary(Accum);
generate_binary(Size, Accum) ->
    generate_binary(Size - 1, [random:uniform(255)|Accum]).
