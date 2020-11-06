/*
* Recognize (Some) Recurrence Relations
* geometric and arithmetic sequences
*/

geometric([X,Y],Q) :-
    Y is Q * X.
geometric([X,Y|T], Q) :-
    Y is Q * X,
    geometric([Y|T], Q).

arithmetic([X,Y],Q) :-
    Y is Q + X.
arithmetic([X,Y|T], Q) :-
    Y is Q + X,
    arithmetic([Y|T], Q).