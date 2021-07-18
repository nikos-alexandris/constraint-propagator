## Simple Constraint Solver in Scheme

Functions:
* (<+> x y)
* (<-> x y)
* (<*> x y)
* (</> x y)
* (<=> x y)
* (<?> x)
* (<!> x)

Examples:
```
(define (pythagorean-triplet a b c)
  (<=> (<+> (<*> a a) (<*> b b)) (<*> c c)))
