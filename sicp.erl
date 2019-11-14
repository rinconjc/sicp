-module(sicp).
-export([fact/1, min_divisor/1, is_prime/1, primes_over/2, integral/4, sum/4, cont_frac/3]).

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
    end .

sum(Term, A, Next, B) ->
    sum(Term, A, Next, B, 0).

sum(Term, A, Next, B, Result) ->
    if A>B ->
            Result;
       true ->
            sum(Term, Next(A), Next, B, Result+Term(A))
    end.

integral(F,A,B,N)->
    H=(B-A)/N,
    Coef=fun(0) ->1;
            (K) when K==N->1;
            (K) -> if (K rem 2)==0 -> 4;
                      true -> 2
                   end
         end,
    TermFn=fun(K)-> F(A+K*H)*Coef(K)
           end,
    H/3 * sum(TermFn, 0, fun(X)->X+1 end, N).


cont_frac(N,D,K)->
    cont_frac(N,D,K,K,0).

cont_frac(N,D,K,I,R)-> 
    if I==0 ->
            R;
       true ->
            cont_frac(N,D,K,I-1,N(I)/(D(I)+R))
    end.


 
