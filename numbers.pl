/*Special Numbers and Sequences*/
:- use_module(library(clpfd)).

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

/*SEQUENCES
TODO: merge into one method that recognizes all of them*/
binomial_seq(N,L) :-
    length(L,N),
    length(L2,N),
    maplist(=(N),L2),
    findall(X,between(1,N,X),List),
    maplist(binomial_co,L2,List,L).
fib_seq(N,L) :-
    length(L,N),
    findall(X,between(1,N,X),List),
    maplist(fib,List,L).
der_seq(N,L) :-
    length(L,N),
    findall(X,between(1,N,X),List),
    maplist(der,List,L).

fib(0,0) :- !.
fib(1,1) :- !.
fib(N,R) :-
    N1 is N-1,
    N2 is N-2,
    fib(N1,R1),
    fib(N2,R2),
    R is R1 + R2.
/*Lucas Numbers is Fibonacci w different starting values*/
lucas(0,2) :- !.
lucas(1,1) :- !.
lucas(N,R) :-
    N1 is N-1,
    N2 is N-2,
    lucas(N1,R1),
    lucas(N2,R2),
    R is R1 + R2.
/*Derangement Numbers*/
der(1,0).
der(2,1).
der(N,R) :-
    N1 is N-1,
    N2 is N-2,
    der(N1,R1),
    der(N2,R2),
    R is (N1 * (R1 + R2)).
/*Catalan Numbers
* C_n = \sum_{k=0}{n-1} C_k * C_{n-1-k}
*/
caterm(N,X,R) :-
    cat(X,R1),
    X2 is N-1-X,
    cat(X2,R2),
    R is R1*R2.
/*NOT WORKING
How can we specify how `maplist` calls its first argument for every element of the list?
i.e. maplist(fn(x,y,z),ListInput,ResultList) like is every call fn(X,_,_) or what are the default call semantics?
ERROR: Unknown procedure: caterm/2
ERROR:   However, there are definitions for:
ERROR:         caterm/3
ERROR: 
ERROR: In:
ERROR:   [13] caterm(1,_13078)
ERROR:   [12] apply:maplist_([1],[_13122|_13124],user:caterm) at /usr/local/Cellar/swi-prolog/8.2.1/libexec/lib/swipl/library/apply.pl:212
ERROR:   [10] cat(1,_13156) at /Users/4meta5/skool/4610/prolog/reconocer/numbers.pl:62
ERROR:    [9] <user>
ERROR: 
ERROR: Note: some frames are missing due to last-call optimization.
ERROR: Re-run your program in debug mode (:- debug.) to get more detail.
   Exception: (13) caterm(1, _4054) ? creep
*/
cat(0,1).
cat(N,R) :- 
    findall(X,between(1,N,X),List),
    maplist(caterm,List,L),
    foldl(plus,L,0,R).

/*Pascal's Recurrence
TODO Debug: what is base case
?- pascal(10,5,R).
false.
*/
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