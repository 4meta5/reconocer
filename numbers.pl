/*Special Numbers and Sequences*/

/* Combine these two methods into one using
MetaPredicates https://www.metalevel.at/prolog/metapredicates
known(C) :- 
    C = fib; C = der.
seq(C,N,L) :-
    known(C),
    length(L,N),
    findall(X,between(1,N,X),List),
    C is fib -> maplist(fib,List,L),
    C is der -> maplist(der,List,L).

?- seq(fib,10,L).
ERROR: Arithmetic: `fib/0' is not a function
ERROR: In:
ERROR:   [11] fib is fib
ERROR:   [10] seq(fib,10,[_12156,_12162|...]) at /Users/4meta5/skool/4610/prolog/reconocer/numbers.pl:9
ERROR:    [9] <user>
*/
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