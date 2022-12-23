# Top-level translation

Other classes are discarded.

Possibly we can derive modifies/accessible clauses for the processes/actions.

In the old pass, this pass was also responsible for rewriting process expressions involving || and + outside futures
into p_merge and p_seq.

## Model
```
model M {
...
}
```
==
```
class M {
...
}
```

## Model fields:
`T t`
==
`T t`

## Processes
```
accessible a;
modifies b;
requires p;
ensures q;
process P(args) = processExpr;
```
== 
```
requires 0 < r && r < 1;
context Perm(a', r);
context Perm(b', 1);
requires p';
ensures q';
void processP(args', Rational r) { 
    _createBody(_expandUnguarded(processExpr))
};
```

## Actions
```
accessible a;
modifies b;
requires p;
ensures q;
action a(args);
```
==
```
requires 0 < r && r < 1;
context Perm(a', r);
context Perm(b', 1);
requires p';
ensures q';
void a(args', Rational r);
```


## Pure functions inside the model
```
requires p;
ensures q;
T func(args) = body;
```
==
```
requires p';
ensures q';
T' func(args') = body';
```




# _expandUnguarded

## action, empty

Return as-is.

## P(args)

`_expandUnguarded(_inline(P, args))`

## P || Q (parallel composition)

`_leftMerge(_expandUnguarded(P), Q) + _leftMerge(_expandUnguarded(Q), P)`

## P + Q (non-deterministic choice)

`_expandUnguarded(P) + _expandUnguarded(Q)`

## P * Q (sequential composition)

`_expandUnguarded(P) * Q`

## b ? P : Q (boolean choice)

`b ? _expandUnguarded(P) : _expandUnguarded(Q)`



# _leftMerge

Left merge of (P, Q) is the process where P _must_ take an action, and after that it behaves as P' || Q.

## (a, P)

Where `a` is an action:

`a * P`

## (empty, P)

`P`

## (P + Q, R)

`_leftMerge(P, R) + _leftMerge(Q, R)`

## (P * Q(xs), R(ys))

P must be an action, otherwise it would've been expanded by `_expandUnguarded`
Q can be either an action or a process
There should also be a top-level process that has Q & R composed in parallel: Q || R, for some ordering of P and Q.
Call this process S. We concatenate xs and ys (!!!), resulting in the following expression:

`P * S(xs, ys)`

## (b ? P : Q, R)

`b ? _leftMerge(P, R) : _leftMerge(Q, R)`



# _createBody

Creates a plain program from a process expression. Sequential composition becomes ;, non-deterministic choice becomes an
if with a random boolean method call as condition, parallel composition is not supported, and if then else just becomes
an if-then-else statement. Actions & process calls must be handled specially, in case any accessible/modifies clauses
appear.