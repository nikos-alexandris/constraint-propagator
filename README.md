## Simple Constraint Solver in Scheme

Functions:
* (<+> x y) addition
* (<-> x y) subtraction
* (<*> x y) multplication
* (</> x y) division
* (<=> x y) equality
* (<?> x) get value
* (<!> x) remove value  

Examples:
```
(define (pythagorean-triplet a b c)
  (<=> (<+> (<*> a a) (<*> b b)) (<*> c c)))

> (define a (make-named-variable 'a))
> (define b (make-named-variable 'b))
> (define c (make-named-variable 'c))
> (pythagorean-triplet a b c)
> (<=> a 3)
    Variable a = 1
> (<=> c 5)
    Variable b = 4
    Variable c = 5
```
