#|
    Determine the values of the following expressions. Use your Scheme system to verify your answers.
    a. (cons 'car 'cdr)
    b. (list 'this '(is silly))
    c. (cons 'is '(this silly?))
    d. (quote (+ 2 3))
    e. (cons '+ '(2 3))
    f. (car '(+ 2 3))
    g. (cdr '(+ 2 3))
    h. cons
    i. (quote cons)
    j. (quote (quote cons))
    k. (car (quote (quote cons)))
    l. (+ 2 3)
    m. (+ '2 '3)
    n. (+ (car '(2 3)) (car (cdr '(2 3))))
    o. ((car (list + - * /)) 2 3)
|#

;;; answer:
;;; 
;;; (cons 'car 'cdr)                            => ('car . 'cdr)
;;; (list 'this '(is silly))                    => (this (is silly))
;;; (cons 'is '(this silly?))                   => (is this silly?)
;;; (quote (+ 2 3))                             => (+ 2 3)
;;; (cons '+ '(2 3))                            => (+ 2 3)
;;; (car '(+ 2 3))                              => +
;;; (cdr '(+ 2 3))                              => (2 3)
;;; cons                                        => #<procedure cons>
;;; (quote cons)                                => cons
;;; (quote (quote cons))                        => 'cons
;;; (car (quote (quote cons)))                  => quote
;;; (+ 2 3)                                     => 5
;;; (+ '2 '3)                                   => 5
;;; (+ (car '(2 3)) (car (cdr '(2 3))))         => 5
;;; ((car (list + - * /)) 2 3)                  => 5
