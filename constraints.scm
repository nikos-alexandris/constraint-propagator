;; Top-level constraint functions

(define (<+> x y)
  (let ((z (make-variable))
        (xvar (if (number? x)
                  (number->variable x)
                  x))
        (yvar (if (number? y)
                  (number->variable y)
                  y)))
    (adder xvar yvar z)
    z))

(define (<-> x y)
  (let ((z (make-variable))
        (xvar (if (number? x)
                  (number->variable x)
                  x))
        (yvar (if (number? y)
                  (number->variable y)
                  y)))
    (subtractor xvar yvar z)
    z))

(define (<*> x y)
  (let ((z (make-variable))
        (xvar (if (number? x)
                  (number->variable x)
                  x))
        (yvar (if (number? y)
                  (number->variable y)
                  y)))
    (multiplier xvar yvar z)
    z))

(define (</> x y)
  (let ((z (make-variable))
        (xvar (if (number? x)
                  (number->variable x)
                  x))
        (yvar (if (number? y)
                  (number->variable y)
                  y)))
    (divider xvar yvar z)
    z))

(define (<=> x y)
  (cond ((and (number? x) (not (number? y)))
         (set-value! y x 'user))
        ((and (number? y) (not (number? x)))
         (set-value! x y 'user))
        (else (let ((xvar (if (number? x)
                              (number->variable x)
                              x))
                    (yvar (if (number? y)
                              (number->variable y)
                              y)))
                (equal xvar yvar)))))

(define (<!> var)
  (forget-value! var 'user))

(define (<?> var)
  (get-value var))

;; Variables

(define (make-named-variable name)
  (let ((var (make-variable)))
    (track name var)
    var))

(define (make-variable)
  (let ((value false) (informant false) (constraints '()))
    (define (set-value! newval setter)
      (cond ((not (has-value? this))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))

    (define (forget-value! retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
            (cons new-constraint constraints)))
      (if (has-value? this)
          (inform-about-value new-constraint))
      'done)

    (define (this message)
      (cond ((eq? message 'has-value?)
             (if informant true false))
            ((eq? message 'value) value)
            ((eq? message 'set-value!) set-value!)
            ((eq? message 'forget) forget-value!)
            ((eq? message 'connect) connect)
            (else (error "Unknown operation: VARIABLE"
                         message))))
    this))

(define (has-value? variable)
  (variable 'has-value?))
(define (get-value variable)
  (variable 'value))
(define (set-value! variable new-value informant)
  ((variable 'set-value!) new-value informant))
(define (forget-value! variable retractor)
  ((variable 'forget) retractor))
(define (connect variable new-constraint)
  ((variable 'connect) new-constraint))

;; Constraints

(define (adder a1 a2 sum)

  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       this))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       this))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       this))
          ((and (has-value? sum) (eq? a1 a2))
           (set-value! a1
                       (/ (get-value sum) 2)))))

  (define (process-forget-value)
    (forget-value! sum this)
    (forget-value! a1 this)
    (forget-value! a2 this)
    (process-new-value))

  (define (this message)
    (cond ((eq? message 'I-have-a-value) (process-new-value))
          ((eq? message 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" message))))

  (connect a1 this)
  (connect a2 this)
  (connect sum this)

  this)

(define (subtractor s1 s2 diff)

  (define (process-new-value)
    (cond ((and (has-value? s1) (has-value? s2))
           (set-value! diff
                       (- (get-value s1) (get-value s2))
                       this))
          ((and (has-value? s1) (has-value? diff))
           (set-value! s2
                       (- (get-value s1) (get-value diff))
                       this))
          ((and (has-value? s2) (has-value? diff))
           (set-value! s1
                       (+ (get-value diff) (get-value s2))
                       this))))

  (define (process-forget-value)
    (forget-value! diff this)
    (forget-value! s1 this)
    (forget-value! s2 this)
    (process-new-value))

  (define (this message)
    (cond ((eq? message 'I-have-a-value) (process-new-value))
          ((eq? message 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" message))))

  (connect s1 this)
  (connect s2 this)
  (connect diff this)

  this)


(define (multiplier m1 m2 product)

  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 this))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       this))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       this))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       this))
          ((and (has-value? product) (eq? m1 m2))
           (set-value! m1
                       (sqrt (get-value product))
                       this))))

  (define (process-forget-value)
    (forget-value! product this)
    (forget-value! m1 this)
    (forget-value! m2 this)
    (process-new-value))

  (define (this message)
    (cond ((eq? message 'I-have-a-value) (process-new-value))
          ((eq? message 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" message))))

  (connect m1 this)
  (connect m2 this)
  (connect product this)

  this)

(define (divider d1 d2 quotient)

  (define (process-new-value)
    (cond ((and (has-value? d2) (= (get-value d2) 0))
           (error "Division by zero is illegal: DIVIDER"))
          ((and (has-value? d1) (has-value? d2))
           (set-value! quotient
                       (/ (get-value d1) (get-value d2))
                       this))
          ((and (has-value? quotient) (has-value? d1))
           (set-value! d2
                       (/ (get-value d1)
                          (get-value quotient))
                       this))
          ((and (has-value? quotient) (has-value? d2))
           (set-value! d1
                       (* (get-value quotient)
                          (get-value d2))
                       this))))

  (define (process-forget-value)
    (forget-value! quotient this)
    (forget-value! d1 this)
    (forget-value! d2 this)
    (process-new-value))

  (define (this message)
    (cond ((eq? message 'I-have-a-value) (process-new-value))
          ((eq? message 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: DIVIDER" message))))

  (connect d1 this)
  (connect d2 this)
  (connect quotient this)

  this)

(define (equal v1 v2)
  (define (process-new-value)
    (cond ((and (has-value? v1) (not (has-value? v2)))
           (set-value! v2
                       (get-value v1)
                       this))
          ((and (not (has-value? v1)) (has-value? v2))
           (set-value! v1
                       (get-value v2)
                       this))
          ((and (has-value? v1) (has-value? v2) (= (get-value v1) (get-value v2)))
           'ok)
          (else (error "Amounts not equal: EQUAL" (list (get-value v1) (get-value v2))))))

  (define (process-forget-value)
    (forget-value! v1 this)
    (forget-value! v2 this))

  (define (this message)
    (cond ((eq? message 'I-have-a-value) (process-new-value))
          ((eq? message 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: <=>" message))))

  (connect v1 this)
  (connect v2 this)

  this)

(define (constant variable value)
  (define (this message)
    (error "Unknown request: <=>" message))
  (connect variable this)
  (set-value! variable value this))

(define (track name variable)
  (define (print-tracked value)
    (newline) (display "Variable ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-tracked (get-value variable)))
  (define (process-forget-value) (print-tracked "?"))
  (define (this message)
    (cond ((eq? message 'I-have-a-value) (process-new-value))
          ((eq? message 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: TRACK" message))))
  (connect variable this)
  this)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;; Helpers

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (number->variable num)
  (let ((var (make-variable)))
    (constant var num)
    var))

(define (pythagorean-triplet a b c)
  (<=> (<+> (<*> a a) (<*> b b)) (<*> c c)))
