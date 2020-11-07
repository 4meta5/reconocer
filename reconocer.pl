/*
* Recognize (some) Recurrence Relations
* arithmetic, geometric sequences
*/

arithmetic([X,Y],Q) :-
    Q is Y-X.
arithmetic([X,Y|T],Q) :-
    arithmetic([X,Y],Q),
    arithmetic([Y|T],Q).

geometric([X,Y],Q) :-
    Q is Y/X.
geometric([X,Y|T],Q) :-
    geometric([X,Y],Q),
    geometric([Y|T],Q).

/*Checks if the sequence can be recognized*/
known(L) :-
    arithmetic(L,_);geometric(L,_).

nth([H0|_],0,H0).
/*
?- nth([1,2,3,4],1,R).
ERROR: Stack limit (1.0Gb) exceeded
*/
nth([H0|Seq],N,R) :-
    known([H0|Seq]),
    arithmetic([H0|Seq],Q) -> X is N*Q, R is H0+X,
    geometric([H0|Seq],Q) -> X is Q**N, R is H0*X.

partial_sums_arithmetic([H0|_],0,H0).
partial_sums_arithmetic([H0|Seq],N,R) :-
    arithmetic([H0|Seq],Q),
    X is (N+1)*H0,
    Y is Q*N*(N+1)*0.5,
    R is X+Y.

partial_sums_geometric([H0|_],0,H0).
partial_sums_geometric([H0|Seq],N,R) :-
    geometric([H0|Seq],Q),
    Q = 1 -> R is (N+1)*H0,
    Q \= 1 -> R is (((Q**(N+1))-1)*H0)/(Q-1).

/*
Special Sequences to Recognize
* fibonacci
* TODO: derangement numbers, catalan numbers
*/
fib(0,0) :- !.
fib(1,1) :- !.
fib(N,R) :-
    N1 is N-1,
    N2 is N-2,
    fib(N1,R1),
    fib(N2,R2),
    R is R1 + R2.