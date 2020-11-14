/*
* Recognize Integer Relations
* arithmetic, geometric sequences
* special sequences i.e. fibonacci numbers
*/
:- use_module(library(clpfd)).

/* 
Quadratic Formula
src: https://stackoverflow.com/questions/25356094/quadratic-equation-solving-in-prolog
?- quadratic(1,-1,-2,R).
R = [-1.0, 2.0].
?- quadratic(1,-3,-10,R).
R = [-2.0, 5.0].
*/
sqroot(X,R) :- X*X#=R,X#>=0.
quadratic(A,B,C,R) :-
    D is (B^2)-(4*A*C),
    (D<0 -> R = [];
    D#=0 -> X is (-B)/(2*A), R = [X];
    sqroot(S,D), X1 is (-B-S)/(2*A), 
      X2 is (-B+S)/(2*A), R = [X1,X2]).
/* Solve system of linear equations for X,Y \in 1..10
*  C1*X + C2*Y = S1
*  D1*X + D2*Y = S2
?- linear_eq(1,1,6,-3,1,2,X,Y).
X = 1,
Y = 5.
?- linear_eq(2,1,5,-1,1,2,X,Y).
X = 1,
Y = 3.
*/
linear_sys(C1,C2,S1,D1,D2,S2,Y,X) :-
    X in 1..10,Y in 1..10,
    A1 #= X*C1,
    A2 #= Y*C2,
    S1 #= A1+A2,
    B1 #= X*D1,
    B2 #= Y*D2,
    S2 #= B1+B2.
/* Recognize linear recurrences to ascertain parameterization
?- linear_par([1,3,5,11],R,X).
R = [1, 2],
X = [1, 3].
?- linear_par([1,3,5,11,21],R,X).
R = [1, 2],
X = [1, 3].
?- linear_par([1,3,5,11,22],R,X).
false.
?- linear_par([2,7,11,25,47],R,X).
R = [1, 2],
X = [2, 7].
*/
linear_par(L,_) :- length(L,N),N=<3,!. %Cut to end recursion
linear_par([H1,H2,H3,H4|T],[B,C]) :-
    /*solves for B, C from H3 = H1*B + H2*C, H4 = H2*B + H3*C*/
    linear_sys(H1,H2,H3,H2,H3,H4,B,C),
    linear_par([H2,H3,H4|T],[B,C]).
linear_par([H1,H2|T],[B,C],[A0,A1]) :-
    length([H1,H2|T],N),N>=4,
    A0 is H1,
    A1 is H2,
    linear_par([H1,H2|T],[B,C]).
/* Linear Homogenous Equation Degree 2
* input equation a_n = (B)a_n-1 + (C)a_n-2 
* A0, A1 are initial conditions when n = 0,1 respectively
* Result yields n-term formula: A_n = C1*X1^n + C2*X2^n in the form [[X1,C1],[X2,C2]]
?- linear_rec([2,7,11,25,47],R).
R = [[-1, -1], [2, 3]].
*/
linear_rec(Seq,[[X1,C1],[X2,C2]]) :-
    linear_par(Seq,[B,C],[A0,A1]),
    NB is -B,NC is -C,quadratic(1,NB,NC,[X1,X2]),
    C1 in -10..10,C2 in -10..10,
    A0 #= C1+C2,
    C3 #= C1*X1,
    C4 #= C2*X2,
    A1 #= C3+C4.
/*A_n = C1*X1^n + C2*X2^n*/
linear_nth([[X1,C1],[X2,C2]],N,R) :-
    T1 is C1*(X1**N),
    T2 is C2*(X2**N),
    R is T1+T2.

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

/*Recognize Sequence from List of Integers*/
known(L) :-
    arithmetic(L,_);
    geometric(L,_);
    linear_rec(L,_).

/*Nth Term Formulas
INCORRECT
?- nth_linear([1,3,5,11],10,R).
R = 6149.000569198215.
*/
nth_linear(Seq,N,R) :-
    linear_rec(Seq,C,I),
    linear(C,I,[[X1,C1],[NegX2,C2]]),
    R is ((C1 * (X1**N))-(C2*(NegX2**N))).
nth_arithmetic([H0|Seq],N,R) :-
    arithmetic([H0|Seq],Q) -> X is N*Q, R is H0+X.
nth_geometric([H0|Seq],N,R) :-
    geometric([H0|Seq],Q) -> X is Q**N, R is H0*X.

/*Partial Sum Formulas*/
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
/*Binomial Coefficient, N choose R where order doesn't matter*/
binomial_co(N,R,Result) :-
    permutation(N,R,R1),
    fact(R,R2),
    Result is R1/R2.
/*Permutation Count, N choose R where order matters*/
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
/*Lucas Numbers (Fibonacci w different initial values)*/
lucas(0,2) :- !.
lucas(1,1) :- !.
lucas(N,R) :-
    N1 is N-1,
    N2 is N-2,
    lucas(N1,R1),
    lucas(N2,R2),
    R is R1 + R2.

/*Pascal's Recurrence*/
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
