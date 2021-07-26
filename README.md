## Simple Constraint Solver in Scheme
(Tested compatibility with mit-scheme and chicken-scheme)

Functions:
* `(<+> x y)` addition
* `(<-> x y)` subtraction
* `(<*> x y)` multiplication
* `(</> x y)` division
* `(<=> x y)` equality
* `(<?> x)` get value
* `(<!> x)` remove value
* `(<var> name)` new variable

Examples:
```scheme
(define (pythagorean-triplet a b c)
  (<=> (<+> (<*> a a) (<*> b b)) (<*> c c)))

> (define a (<var> 'a))
> (define b (<var> 'b))
> (define c (<var> 'c))
> (pythagorean-triplet a b c)
> (<=> a 3)
    Variable a = 3
> (<=> c 5)
    Variable b = 4
    Variable c = 5
```
