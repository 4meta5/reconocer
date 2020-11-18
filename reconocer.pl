:- use_module(library(clpfd)).

/*Ensure Every Input Sequence is Monotonically Increasing*/
mincreasing([A,B]) :- A=<B.
mincreasing([A,B,C|T]) :-
    mincreasing([A,B]),
    mincreasing([B,C|T]).

/*Docs, called in rec output if input sequence is recognized*/
half :-
    write('To compute the Nth term, call `nth(Seq,N,R)`'),nl.
full :-
    half,
    write('To compute sum from 0 to N, call `nsum(Seq,N,R)`'),nl,
    write('To compute sum from X to N, call `sum(Seq,X,N,R)`').

/*Recognize Sequence, Print Functions Available and Calling Information*/
rec(L) :-
    mincreasing(L),
    (special_rec(L),nl,half,!;
    arithmetic(L,_) -> write('~~Arithmetic Series~~'),nl,full,!;
    geometric(L,_) -> write('~~Geometric Series~~'),nl,full,!;
    linear_rec(L,_) -> write('Linear Homogenous Eq of Deg2'),nl,half).
/*Recognize Sequence and Compute Nth Term*/
nth(L,N,R) :-
    (special_nth(L,N,R);
    nth_arithmetic(L,N,R);
    nth_geometric(L,N,R);
    linear_rec(L,X) -> linear_nth(X,N,R)).
/*Recognize Sequence and Compute Sum [0,N] s.t. X < N*/
nsum(L,N,R) :- 
    sum_arithmetic(L,N,R);sum_geometric(L,N,R).
/*Recognize Sequence and Compute Sum [X,N] s.t. X < N*/
sum(L,X,N,R) :-
    X<N,
    (sum_arithmetic(L,X,R1) -> sum_arithmetic(L,N,R2),R is R2-R1;
    sum_geometric(L,X,R1) -> sum_geometric(L,N,R2),R is R2-R1).

/*Quadratic Formula*/
sqroot(X,R) :- X*X#=R,X#>=0.
quadratic(A,B,C,R) :-
    D is (B^2)-(4*A*C),
    (D<0 -> R = [];
    D#=0 -> X is (-B)/(2*A), R = [X];
    sqroot(S,D), X1 is (-B-S)/(2*A), 
      X2 is (-B+S)/(2*A), R = [X1,X2]).
/*Solve system of linear equations for X,Y \in L..H
*  C1*X + C2*Y = S1
*  D1*X + D2*Y = S2
*/
linear_sys(C1,C2,S1,D1,D2,S2,L,H,X,Y) :-
    X in L..H,Y in L..H,
    A1 #= X*C1,
    A2 #= Y*C2,
    S1 #= A1+A2,
    B1 #= X*D1,
    B2 #= Y*D2,
    S2 #= B1+B2.
/*Ascertain linear parameterization*/
linear_par(L,_) :- length(L,N),N=<3,!. %Cut to end recursion
linear_par([H1,H2,H3,H4|T],[B,C]) :-
    /*solves for B, C from H3 = H1*B + H2*C, H4 = H2*B + H3*C
    call default bounds B,C in 1..10*/
    linear_sys(H1,H2,H3,H2,H3,H4,1,10,C,B),
    linear_par([H2,H3,H4|T],[B,C]).
linear_par([H1,H2|T],[B,C],[A0,A1]) :-
    length([H1,H2|T],N),N>=4,
    A0 is H1,
    A1 is H2,
    linear_par([H1,H2|T],[B,C]).
/*Linear Homogenous Equation Degree 2
* input equation a_n = (B)a_n-1 + (C)a_n-2 
* A0, A1 are initial conditions when n = 0,1 respectively
* Result represents n-term coefficients for A_n = C1*X1^n + C2*X2^n 
* in the form [[X1,C1],[X2,C2]]
*/
linear_rec(Seq,[[X1,C1],[X2,C2]]) :-
    linear_par(Seq,[B,C],[A0,A1]),
    NB is -B,NC is -C,quadratic(1,NB,NC,[X1,X2]),
    linear_sys(1,1,A0,X1,X2,A1,-10,10,C1,C2).
/*Nth Term Formula: A_n = C1*X1^n + C2*X2^n from first arg [[X1,C1],[X2,C2]]*/
linear_nth([[X1,C1],[X2,C2]],N,R) :-
    T1 is C1*(X1**N),
    T2 is C2*(X2**N),
    R is T1+T2.

arithmetic([X,Y],Q) :-
    Q is Y-X.
arithmetic([X,Y,Z|T],Q) :-
    arithmetic([X,Y],Q),
    arithmetic([Y,Z|T],Q).

geometric([X,Y],Q) :-
    Q is Y/X.
geometric([X,Y,Z|T],Q) :-
    geometric([X,Y],Q),
    geometric([Y,Z|T],Q).

nth_arithmetic([H0|Seq],N,R) :-
    arithmetic([H0|Seq],Q) -> X is N*Q, R is H0+X.
nth_geometric([H0|Seq],N,R) :-
    geometric([H0|Seq],Q) -> X is Q**N, R is H0*X.

sum_arithmetic([H0|_],0,H0).
sum_arithmetic([H0|Seq],N,R) :-
    arithmetic([H0|Seq],Q),
    X is (N+1)*H0,
    Y is Q*N*(N+1)*0.5,
    R is X+Y.
sum_geometric([H0|_],0,H0).
sum_geometric([H0|Seq],N,R) :-
    geometric([H0|Seq],Q),
    N1 is N+1,
    (Q #= 1 -> R is N1*H0;
    R is (((Q^N1)-1)*H0)/(Q-1)).

/*Catalan Numbers
* C_n = \sum_{k=0}{n-1} C_k * C_{n-1-k}
*/
cat(_,_,[]).
cat(A,B,[H|T]) :-
    B1 is B+1,
    H is 2 * (2 * B - 1) * A / B1,
    cat(H,B1,T).
/*Derangement Numbers*/
der(N,R) :-
    der(2,N,0,1,R).
der(X,N,R,_,R) :- X is N+1.
der(X,N,_,R,R) :- X=N.
der(X,N,D1,D2,R) :-
    X<N,
    X1 is X+1,
    D3 is (X * (D1 + D2)),
    der(X1,N,D2,D3,R).
/*Fibonacci Numbers*/
fib(N,R) :-
    fib(2,N,1,1,R).
fib(X,N,_,F2,F2) :-
    X>=N.
fib(X,N,F1,F2,R) :-
    X<N,
    X1 is X+1,
    F3 is F1+F2,
    fib(X1,N,F2,F3,R).
/*Lucas Numbers*/
lucas(N,R) :-
    fib(2,N,2,1,R).

/*Recognize Special Sequences*/
special_rec(Seq) :-
    length(Seq,N),
    N1 is N-1,
    (cat_seq(N1,L1),L1=Seq -> write('**Catalan Numbers**'),!;
    fib_seq(N,L3),L3=Seq -> write('**Fibonacci Numbers**'),!;
    der_seq(N,L2),L2=Seq -> write('**Derangement Numbers**'),!;
    lucas_seq(N,L4),L4=Seq -> write('**Lucas Numbers**')).
/*Recognize Special Sequence and Output List with Nth Term*/
special_nth(Seq,N,R) :-
    length(Seq,L),L<N,
    (cat_seq(L,Seq) -> cat_seq(N,R);
    der_seq(L,Seq) -> der_seq(N,R);
    fib_seq(L,Seq) -> fib_seq(N,R);
    lucas_seq(L,Seq) -> lucas_seq(N,R)).

/*Sequence Generators*/
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
