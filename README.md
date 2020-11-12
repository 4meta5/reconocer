# reconocer

Recognize Recurrence Relations and Special Sequences

## goal

tool for recognizing recurrence relations to derive 
* general nth term
* partial sums

inspired by [this blog post](http://www.ryanhmckenna.com/2015/06/automatically-finding-recurrence.html)

## usage

every input sequence must have at least 2 elements

```
?- reconocer([1,1,2,3,5,8,13,21,34,55,89],X).
X = fib
```

```
?- reconocer([1,7,13,19,25,31],X).
X = arithmetic with Q = 6
```

```
?- reconocer([2,4,8,16,32,64],X).
X = geometric with Q = 2
```