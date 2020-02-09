-module(sicp).
-export([fact/1, min_divisor/1, is_prime/1, primes_over/2, integral/4, sum/4, cont_frac/3, euler_e/1, tan_cf/2, fixed_point/2, newtons_method/2, cubic/3, compose/2, repeated/2,
smoothed/1, cons/2, car/1, cdr/1, mk_interval/2, add_interval/2,
sub_interval/2, mul_interval/2, div_interval/2,
reverse/1, pairs/1, queens/1]).
-define(TOLERANCE, 0.000001).

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
    if I==1 ->
            R;
       true ->
            cont_frac(N,D,K,I-1,N(I)/(D(I)+R))
    end.

euler_e(K)->
    cont_frac(fun(_)->1.0 end,
              fun(X)->if (X-2) rem 3 == 0 ->
                              2+2*(X-2) div 3;
                         true -> 1 end
                      end,
              K).

tan_cf(X,K)->
    cont_frac(fun(I)-> 
                     if I>1 -> X*X;
                        true -> X end
                     end,
             fun(I)-> 2*I-1 end,
             K).

fixed_point(F,Guess)->
    Is_close_enough=fun(V1,V2)-> abs(V1-V2)<?TOLERANCE end,
    Try=fun(Try,G)->
                N=F(G),
                case Is_close_enough(N,G) of
                    true ->
                        G;
                    false ->
                        Try(Try,N)
                end
        end,
    Try(Try, Guess).

deriv(G)->
    DX=0.000001,
    fun(X)-> (G(X+DX)-G(X))/DX end.

newton_transform(G)->
    fun(X)-> X-G(X)/((deriv(G))(X)) end.

newtons_method(F, Guess)->
    fixed_point(newton_transform(F), Guess).

cubic(A,B,C)->
    fun(X)-> X*X*X+A*X*X+B*X+C end.

compose(F,G)->
    fun(X)-> F(G(X)) end.

repeated(F,N)->
    if N=<1 ->
            F;
       true ->
            compose(F,repeated(F,N-1))
    end.

smoothed(F)->
    DX=0.0001,
    fun(X)-> (F(X)+F(X-DX)+F(X+DX))/3 end.

cons(X,Y)->
    fun(F)->
            F(X,Y)
    end.

car(Z)->
    Z(fun(P,_)->P end).

cdr(Z)->
    Z(fun(_,Q)->Q end).

mk_interval(X,Y)->
    [X,Y].

upper_bound([_,Y])->
    Y.
lower_bound([X,_])->
    X.

add_interval(A,B)->
    mk_interval(lower_bound(A)+lower_bound(B),
                upper_bound(A)+upper_bound(B)).

mul_interval(A,B)->
    P1=lower_bound(A)*lower_bound(B),
    P2=lower_bound(A)*upper_bound(B),
    P3=upper_bound(A)*lower_bound(B),
    P4=upper_bound(A)*upper_bound(B),
    mk_interval(lists:min([P1,P2,P3,P4]), lists:max([P1,P2,P3,P4])).

div_interval(A,B)->
    mul_interval(A,mk_interval(1/upper_bound(B),1/lower_bound(B))).
sub_interval(A,B)->
    mk_interval(lower_bound(A)-upper_bound(B),
                 upper_bound(A)-lower_bound(B)).

mk_center_width(C,W)->    
    mk_interval(C-W,C+W).

reverse(R,[])->
    R;
reverse(R,[H|T]) ->
   reverse([H|R],T).

reverse(L)->
    reverse([], L).


   
pairs(N)->
    lists:flatmap(fun(I)->
                          lists:map(fun(J)->[I,J] end, 
                                    lists:seq(1,I)) end,lists:seq(1,N+1)).

empty_board()->
    [].

adjoin_pos(R, K, RestQs)->
    [[K,R]|RestQs].

safe_pos([[K,R]|Ps], K)->
    lists:all(
      fun([J,M])-> (M /= R) and ((K-J) /= abs(R-M))
      end,
      Ps).

queens(BoardSize)->
    QueenCols=
        fun(K, QC)->
                if K==0 ->
                        [empty_board()];
                   true ->
                        lists:filter(
                          fun(Ps)->
                                  safe_pos(Ps, K)
                          end,
                          lists:flatmap(
                            fun(RestQs)->
                                    lists:map(
                                      fun(NewRow)->
                                              adjoin_pos(NewRow, K, RestQs)
                                      end,
                                      lists:seq(1, BoardSize))
                            end,
                            QC(K-1, QC))) 
                   end
        end,
    QueenCols(BoardSize, QueenCols).

