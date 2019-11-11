-module(sicp).
-export([fact/1, min_divisor/1, is_prime/1, primes_over/2]).

fact(1)->
    1;
fact(X)->
    X*fact(X-1).


smlst_div(N, D) when N<D*D ->
    N;
smlst_div(N, D) when (N rem D) == 0 ->
    D;
smlst_div(N, D) ->
    smlst_div(N, (D + 1)).

min_divisor(N)->
    smlst_div(N, 2).

is_prime(N)->
    N==min_divisor(N).

timed_prime(N)->
    timed_prime(N,erlang:system_time()).

timed_prime(N,Start) ->
    case is_prime(N) of
        true ->
            io:format("elapsed:~B~n",[ erlang:system_time()-Start]),
            true;
        false ->
            false
    end.


                      

primes_over(N, C) when N rem 2 ==0 ->
    primes_over(N+1,C);

primes_over(N, C) ->
    case {C, timed_prime(N)} of
        {1, true} -> [N];
        {C, true} -> [N| primes_over(N+2,C-1)];
        _Else -> primes_over(N+2, C)
    end
.





