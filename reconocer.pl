/*
* Recognize (Some) Recurrence Relations
* geometric and arithmetic sequences
*/

geometric([X,Y],Q) :-
    Y is Q*X.
geometric([X,Y|T],Q) :-
    geometric([X,Y],Q),
    geometric([Y|T],Q).

arithmetic([X,Y],Q) :-
    Y is Q+X.
arithmetic([X,Y|T],Q) :-
    arithmetic([X,Y],Q),
    arithmetic([Y|T],Q).