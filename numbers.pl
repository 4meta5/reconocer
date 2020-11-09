/*Special Numbers and Sequences*/


maplist(C, [], []).
maplist(C, [X|Xs], [Y|Ys]) :-
   call(C, X, Y),
   maplist( C_2, Xs, Ys).
/*TODO: write memoized version that adds them to a list*/
fib_seq(N,L) :-
    findall(X,between(1,N,X),List),
    maplist(fib(_,R),List,L).

fib(0,0) :- !.
fib(1,1) :- !.
fib(N,R) :-
    N1 is N-1,
    N2 is N-2,
    fib(N1,R1),
    fib(N2,R2),
    R is R1 + R2.
/*Derangement Numbers*/
der(1,0).
der(2,1).
der(N, R) :-
    N1 is N-1,
    N2 is N-2,
    der(N1,R1),
    der(N2,R2),
    R is (N1 * (R1 + R2)).
/*Catalan Numbers
* C_n = \sum_{k=0}{n-1} C_k * C_{n-1-k}
* TODO: debug, not working
cat(0,1).
cat(N,R) :- 
    foreach(between(1,N,X),(cat(X,R1),X2 is N-1-X,cat(X2,R2),R3 is R1*R2,member(R3,List))),
    foldl(plus,List,0,R).
*/