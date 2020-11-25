# reconocer

Recognize Recurrence Relations, Special Sequences, and Generate Polynomials

## References
* [prolog higher-order predicates](https://www.metalevel.at/prolog/metapredicates)
* [automatically finding recurrence relations from integer sequences](http://www.ryanhmckenna.com/2015/06/automatically-finding-recurrence.html)

## Usage

`reconocer` may be used to recognize 
1. arithmetic and geometric series
2. recurrence relations that are linear, homogenous degree 2 with constant coefficients
3. special sequences including Fibonacci Numbers, Lucas Numbers, Catalan Numbers, Derangement Numbers, Involution Numbers

For each recognizable sequence, the program can derive the general nth term and partial sums.

`reconocer` also supports generating polynomials from the difference sequence of input integers, but this functionality is NOT part of the sequence recognition syntax.

*All prolog example queries are made from the [swish](https://www.swi-prolog.org/Download.html) REPL*

```prolog
(main)⚡ % swipl reconocer.pl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.2.1)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.
For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).
?- 
```

### Arithmetic and Geometric Series

Now, we'll query the db to demonstrate functionality.

```prolog
?- rec([1,3,5,7,9,11]).
~~Arithmetic Series~~
To compute the Nth term, call `nth(Seq,N,R)`
To compute sum from 0 to N, call `nsum(Seq,N,R)`
To compute sum from X to N, call `sum(Seq,X,N,R)`
true .
?- rec([1,5,25,125,625]).
~~Geometric Series~~
To compute the Nth term, call `nth(Seq,N,R)`
To compute sum from 0 to N, call `nsum(Seq,N,R)`
To compute sum from X to N, call `sum(Seq,X,N,R)`
true .
```

Let's perform easily verifiable queries for the arithmetic series.

```prolog
?- nth([1,3,5,7],0,R).
R = 1 .
?- nth([1,3,5,7],1,R).
R = 3 .
?- nth([1,3,5,7],2,R).
R = 5 .
?- nth([1,3,5,7],3,R).
R = 7 .
?- nth([1,3,5,7],4,R).
R = 9 .
?- nth([1,3,5,7],5,R).
R = 11 .
```

Query arithmetic partial sums from 0 to N (where N is the second parameter input).
```prolog
?- nsum([1,3,5,7],0,R).
R = 1 .
?- nsum([1,3,5,7],1,R).
R = 4 .
?- nsum([1,3,5,7],2,R).
R = 9 .
?- nsum([1,3,5,7],3,R).
R = 16 .
?- nsum([1,3,5,7],4,R).
R = 25 .
?- nsum([1,3,5,7],5,R).
R = 36 .
```

And we can use these partial sums from 0 to N to manually verify the sum function from X to N is `nsum(N)-nsum(X)`.
```prolog
?- sum([1,3,5,7],2,4,R).
R = 16 .
?- sum([1,3,5,7],1,4,R).
R = 21 .
?- sum([1,3,5,7],1,3,R).
R = 12 .
```
* `nsum(4)-nsum(2)`= 25-9=16
* `nsum(4)-nsum(1)`= 25-4=21
* `nsum(3)-nsum(1)`= 16-4=12

All of this functionality is also available for geometric series without any changes to the query syntax.

Geometric Nth Terms:
```prolog
?- nth([1,4,16,64],0,R).
R = 1 .
?- nth([1,4,16,64],1,R).
R = 4 .
?- nth([1,4,16,64],2,R).
R = 16 .
?- nth([1,4,16,64],3,R).
R = 64 .
?- nth([1,4,16,64],4,R).
R = 256 .
?- nth([1,4,16,64],5,R).
R = 1024 .
```

Geometric Sums from 0 to N:
```prolog
?- nsum([1,4,16,64],0,R).
R = 1 .
?- nsum([1,4,16,64],1,R).
R = 5 .
?- nsum([1,4,16,64],2,R).
R = 21 .
?- nsum([1,4,16,64],3,R).
R = 85 .
?- nsum([1,4,16,64],4,R).
R = 341 .
?- nsum([1,4,16,64],5,R).
R = 1365 .
```

Geometric Sums from X to N:
```prolog
?- sum([1,4,16,64],1,3,R).
R = 80 .
?- sum([1,4,16,64],2,3,R).
R = 64 .
?- sum([1,4,16,64],2,4,R).
R = 320 .
```
* `nsum(3)-nsum(1)`= 85-5=80
* `nsum(3)-nsum(2)`= 85-21=64
* `nsum(4)-nsum(2)`= 341-21=320

### Linear Homogenous Eq of Degree 2

**[Ex 1](http://nms.lu.lv/wp-content/uploads/2016/04/21-linear-recurrences.pdf)**: What is the solution of the recurrence relation `a_n = a_(n-1) + 2a_(n-2)` with initial conditions `a_0=2,a_1=7`?

```prolog
?- rec([2,7,11,25,47]).
Linear Homogenous Equation of Degree 2
To compute the Nth term, call `nth(Seq,N,R)`
To compute sum from 0 to N, call `nsum(Seq,N,R)`
To compute sum from X to N, call `sum(Seq,X,N,R)`
true.
```

Rather than providing the user with a structured array of coefficients, `reconocer` solves the recurrence directly whenever the nth term or partial sums are queried.

```prolog
?- nth([2,7,11,25,47],0,R).
R = 2.
?- nth([2,7,11,25,47],1,R).
R = 7.
?- nth([2,7,11,25,47],2,R).
R = 11.
?- nth([2,7,11,25,47],3,R).
R = 25.
?- nth([2,7,11,25,47],4,R).
R = 47.
?- nth([2,7,11,25,47],5,R).
R = 97.
```
We can verify the last query by calculating the next term in the sequence using the given recurrence relation `a_n = a_(n-1) + 2a_(n-2)` => 47 + 2*50 = 97.

The partial sums from 0 to N are computed using the same derived nth term formula.

```prolog
?- nsum([2,7,11,25,47],0,R).
R = 2 .
?- nsum([2,7,11,25,47],1,R).
R = 9.
?- nsum([2,7,11,25,47],2,R).
R = 20.
?- nsum([2,7,11,25,47],3,R).
R = 45.
?- nsum([2,7,11,25,47],4,R).
R = 92.
?- nsum([2,7,11,25,47],5,R).
R = 189.
```

Likewise, the sum from X to N can be manually verified:

```prolog
?- sum([2,7,11,25,47],0,1,R).
R = 7 .
?- sum([2,7,11,25,47],1,3,R).
R = 36.
?- sum([2,7,11,25,47],1,4,R).
R = 83.
```
* `nsum(1)-nsum(0)`=9-2=7
* `nsum(3)-nsum(1)`=45-9=36
* `nsum(4)-nsum(1)`=92-9=83

*[TODO: implement for degree >2](https://github.com/4meta5/reconocer/issues/2)*

### Special Sequences

* Fibonacci Numbers
* Lucas Numbers
* Catalan Numbers
* Derangement Numbers
* Involution Numbers

### Generating Polynomials

For any distinct `n+1` points, there is a unique polynomial of degree `n` which passes through all points. 

For our purposes, an input sequence can be interpreted as the euclidean coordinates `(n,h(n))` s.t. `h(n)` is the `nth` term of the input sequence. In particular, we can generate a unique polynomial for any integer sequence from its difference sequences.

