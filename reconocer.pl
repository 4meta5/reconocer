/*
* Recognize (some) Recurrence Relations
* arithmetic, geometric sequences
* special sequences i.e. fibonacci numbers
*/
:- use_module(library(clpfd)).

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

/*TODO: arithmetico-geometric 
* https://en.wikipedia.org/wiki/Arithmetico%E2%80%93geometric_sequence 
*/

/*Checks if the sequence can be recognized
TODO: add special sequences*/
known(L) :-
    arithmetic(L,_);geometric(L,_).
/*TODO: combine into one method based on which relationship is recognized
-->QUESTION: are there any sequences that are arithmetic and geometric?*/
nth_arithmetic([H0|Seq],N,R) :-
    arithmetic([H0|Seq],Q) -> X is N*Q, R is H0+X.
nth_geometric([H0|Seq],N,R) :-
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

/*Factorial*/
fact(X,1) :- X =< 1.
fact(X,R) :- 
    X1 is X - 1,
    fact(X1, R1),
    R is R1*X.
/*Binomial Coefficient; N choose R where order doesn't matter*/
binomial_co(N,R,Result) :-
    permutation(N,R,R1),
    fact(R,R2),
    Result is R1/R2.
/*Permutation Count; N choose R where order matters*/
permutation(N,R,Result) :-
    fact(N,R1),
    fact(N-R,R2),
    Result is R1/R2.

/*Catalan Numbers
* C_n = \sum_{k=0}{n-1} C_k * C_{n-1-k}
*/
cat(_,_,[]).
cat(A,B,[H|T]) :-
    B1 is B+1,
    H is 2 * (2 * B - 1) * A / B1,
    cat(H,B1,T).
/*Derangement Numbers*/
der(1,0).
der(2,1).
der(N,R) :-
    N1 is N-1,
    N2 is N-2,
    der(N1,R1),
    der(N2,R2),
    R is (N1 * (R1 + R2)).
/*Fibonacci Numbers*/
fib(0,0) :- !.
fib(1,1) :- !.
fib(N,R) :-
    N1 is N-1,
    N2 is N-2,
    fib(N1,R1),
    fib(N2,R2),
    R is R1 + R2.
/*Lucas Numbers, Fibonacci w different starting values*/
lucas(0,2) :- !.
lucas(1,1) :- !.
lucas(N,R) :-
    N1 is N-1,
    N2 is N-2,
    lucas(N1,R1),
    lucas(N2,R2),
    R is R1 + R2.

/*
Pascal's Recurrence
*/
pascal(X,X,1).
pascal(X,0,R) :-
    X>0 -> R is 0.
pascal(P,K,Result) :-
    K >= 1,
    P1 is P-1, P1 >= K,
    pascal(P1,K,R1),
    K1 is K-1,
    pascal(P1,K1,R2),
    Result is R1 + R2.
/*Second Stirling Numbers*/
second_stir(X,X,1).
second_stir(X,0,R) :-
    X>0 -> R is 0.
second_stir(P,K,Result) :-
    K >= 1,
    P1 is P-1, P1 >= K,
    second_stir(P1,K,R1),
    K1 is K-1,
    second_stir(P1,K1,R2),
    Result is ((K*R1) + R2).
/*First Stirling Numbers*/
first_stir(X,X,1).
first_stir(X,0,R) :-
    X>0 -> R is 0.
first_stir(P,K,Result) :-
    K >= 1,
    P1 is P-1, P1 >= K,
    first_stir(P1,K,R1),
    K1 is K-1,
    first_stir(P1,K1,R2),
    Result is ((P1*R1) + R2).

/*SEQUENCES
* TODO: meta-predicate to recognize any of them of length N
* TODO: sequence extensions for 
* (1)first stirling (2)second stirling (3)pascal
*/
binomial_seq(N,List) :-
    length(List,N),
    length(L1,N),
    maplist(=(N),L1),
    findall(X,between(1,N,X),L2),
    maplist(binomial_co,L1,L2,List).
cat_seq(0,[1]).
cat_seq(N,List) :- 
    length(L,N),
    List = [1 | L],
    cat(1,1,List).
der_seq(N,List) :-
    length(List,N),
    findall(X,between(1,N,X),L),
    maplist(der,L,List).
fib_seq(N,List) :-
    length(List,N),
    findall(X,between(1,N,X),L),
    maplist(fib,L,List).
lucas_seq(N,List) :-
    length(List,N),
    findall(X,between(1,N,X),L),
    maplist(lucas,L,List).
