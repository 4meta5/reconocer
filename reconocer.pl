/*
* Recognize (some) Recurrence Relations
* arithmetic, geometric sequences
*/

arithmetic([X,Y],Q) :-
    Y is Q+X.
arithmetic([X,Y|T],Q) :-
    arithmetic([X,Y],Q),
    arithmetic([Y|T],Q).

geometric([X,Y],Q) :-
    Y is Q*X.
geometric([X,Y|T],Q) :-
    geometric([X,Y],Q),
    geometric([Y|T],Q).

/*TODO: merge these with `->` once debugged indpenedently*/
/*ERROR: Arguments are not sufficiently instantiated*/
nth_arithmetic([H0|_],0,H0).
nth_arithmetic([H0|Seq],N,R) :-
    arithmetic([H0|Seq],Q),
    X is N*Q,
    R is H0+X.
/*ERROR: Arguments are not sufficiently instantiated*/
nth_geometric([H0|_],0,H0).
nth_geometric([H0|Seq],N,R) :-
    geometric([H0|Seq],Q),
    X is Q**N,
    R is H0*X.

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