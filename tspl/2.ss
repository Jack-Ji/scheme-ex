;;;
;;;  Copyright (c) 2016 Chengde Ji
;;; 
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;; 
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;; 

;; Excercise 2.2.1
;;
;; Convert the following arithmetic expressions into Scheme expressions and evaluate them.
;; a. 1.2 × (2 - 1/3) + -8.7
;; b. (2/3 + 4/9) ÷ (5/11 - 4/3)
;; c. 1+1÷(2+1÷(1+1/2))
;; d. 1×-2×3×-4×5×-6×7
;;
;; Answer:
;;  (+ (* 1.2 (- 2 1/3)) -8.7)          => -6.699999999999999
;;  (/ (+ 2/3 4/9) (- 5/11 4/3))        => -110/87
;;  (+ 1 (/ 1 (+ 2 (/ 1 (+ 1 1/2)))))   => 11/8
;;  (* 1 -2 3 -4 5 -6 7)                => -5040

#|================================================================================================|#

;; Excercise 2.2.2
;;
;; Experiment with the procedures +, -, *, and / to determine Scheme's rules for the type of value
;; returned by each when given different types of numeric arguments.
;;
;; Answer:
;;  (op inexact exact)      => inexact
;;  (op integer rational)   => rational
;;  (op integer real)       => real
;;  (op integer complex)    => complex
;;  (op rational real)      => real
;;  (op rational complex)   => complex
;;  (op real complex)       => complex

#|================================================================================================|#

;; Excercise 2.2.3
;;
;; Determine the values of the following expressions. Use your Scheme system to verify your answers.
;; a. (cons 'car 'cdr)
;; b. (list 'this '(is silly))
;; c. (cons 'is '(this silly?))
;; d. (quote (+ 2 3))
;; e. (cons '+ '(2 3))
;; f. (car '(+ 2 3))
;; g. (cdr '(+ 2 3))
;; h. cons
;; i. (quote cons)
;; j. (quote (quote cons))
;; k. (car (quote (quote cons)))
;; l. (+ 2 3)
;; m. (+ '2 '3)
;; n. (+ (car '(2 3)) (car (cdr '(2 3))))
;; o. ((car (list + - * /)) 2 3)
;;
;; Answer:
;;  (cons 'car 'cdr)                            => ('car . 'cdr)
;;  (list 'this '(is silly))                    => (this (is silly))
;;  (cons 'is '(this silly?))                   => (is this silly?)
;;  (quote (+ 2 3))                             => (+ 2 3)
;;  (cons '+ '(2 3))                            => (+ 2 3)
;;  (car '(+ 2 3))                              => +
;;  (cdr '(+ 2 3))                              => (2 3)
;;  cons                                        => #<procedure cons>
;;  (quote cons)                                => cons
;;  (quote (quote cons))                        => 'cons
;;  (car (quote (quote cons)))                  => quote
;;  (+ 2 3)                                     => 5
;;  (+ '2 '3)                                   => 5
;;  (+ (car '(2 3)) (car (cdr '(2 3))))         => 5
;;  ((car (list + - * /)) 2 3)                  => 5

#|================================================================================================|#

;; Excercise 2.2.4
;;
;; (car (car '((a b) (c d)))) yields a. Determine which compositions of car and cdr applied to
;; ((a b) (c d)) yield b, c, and d.
;;
;; Answer:
;; (car (cdr (car '((a b) (c d)))))         => b 
;; (car (car (cdr '((a b) (c d)))))         => c
;; (car (cdr (car (cdr '((a b) (c d))))))   => d

#|================================================================================================|#

;; Excercise 2.2.5
;;
;; Write a Scheme expression that evaluates to the following internal list structure.
;;
;;   +-------+    +-------+    +---------+
;;   |   |   ---->|   |   ---->| () | () |
;;   +-|-----+    +-|-----+    +---------+
;;     |            |
;;     v            v
;;     +-------+    +-------+    +--------+
;;     | a | b |    |   |   ---->| d | () |
;;     +-------+    +-|-----+    +--------+
;;                    |          
;;                    v          
;;                    +--------+
;;                    | c | () |
;;                    +--------+
;;
;; Answer:
;;  (cons '(a . b)
;;        (cons (cons '(c) '(d))
;;              '(())))           => ((a . b) ((c) d) ())

#|================================================================================================|#

;; Excercise 2.2.6
;;
;; Draw the internal list structure produced by the expression below.
;; (cons 1 (cons '(2 . ((3) . ())) (cons '(()) (cons 4 5))))
;; 
;; Answer:
;;  +-------+    +-------+                       +-------+    +-------+
;;  | 1 |   ---->|   |   ----------------------->|   |   ---->| 4 | 5 |
;;  +-------+    +-|-----+                       +-|-----+    +-------+
;;                 |                               |
;;                 v                               v
;;                 +-------+    +--------+         +---------+
;;                 | 2 |   ---->|   | () |         | () | () |
;;                 +-------+    +-|------+         +---------+
;;                                |
;;                                v
;;                                +--------+
;;                                | 3 | () |
;;                                +--------+

#|================================================================================================|#

;; Excercise 2.2.7
;;
;; The behavior of (car (car (car '((a b) (c d))))) is undefined because
;; (car '((a b) (c d))) is (a b), (car '(a b)) is a, and (car 'a) is undefined.
;; Determine all legal compositions of car and cdr applied to ((a b) (c d)).
;;
;; Answer:
;;  assuming x is '((a b) (c d))
;;  (car (car x))       => a
;;  (car (cdr x))       => (c d)
;;  (cdr (car x))       => (b)
;;  (cdr (cdr x))       => ()
;;  (car (car (car x))) => illegal
;;  (cdr (car (car x))) => illegal
;;  (car (cdr (car x))) => b
;;  (car (car (cdr x))) => c
;;  (cdr (cdr (car x))) => ()
;;  (car (cdr (cdr x))) => illegal
;;  (cdr (car (cdr x))) => d
;;  (cdr (cdr (cdr x))) => illegal

#|================================================================================================|#

;; Exercise 2.3.1
;;
;; Write down the steps necessary to evaluate the expression below.
;; ((car (cdr (list + - * /))) 17 5)
;;
;; Answer:
;;  ((car (cdr (list + - * /))) 17 5)
;;  => ((car (cdr '(+ - * /))) 17 5)
;;  => ((car '(- * /)) 17 5)
;;  => (- 17 5)
;;  => 12

#|================================================================================================|#

;; Exercise 2.4.1
;;
;; Rewrite the following expressions, using let to remove common subexpressions and to improve the
;; structure of the code. Do not perform any algebraic simplifications.
;; a. (+ (- (* 3 a) b) (+ (* 3 a) b))
;; b. (cons (car (list a b c)) (cdr (list a b c)))
;;
;; Answer:
;;  (let ([3a (* 3 a)]
;;    (+ (- 3a b) (+ 3a b))))
;;  (let ([x (list a b c)])
;;    (cons (car x) (cdr x)))

#|================================================================================================|#

;; Exercise 2.4.2
;;
;; Determine the value of the following expression. Explain how you derived this value.
;; (let ([x 9])
;;   (* x
;;      (let ([x (/ x 3)])
;;          (+ x x))))
;;
;; Answer:
;;  (let ([x 9])
;;    (* x
;;       (let ([x (/ x 3)])
;;           (+ x x))))
;;  => (let ([x 9])
;;       (* 9
;;          (let ([x (/ x 3)])
;;              (+ 3 3))))
;;  => (let ([x 9]) (* 9 6))
;;  => 54

#|================================================================================================|#

;; Exercise 2.4.2
;;
;; Rewrite the following expressions to give unique names to each different let-bound variable
;; so that none of the variables is shadowed. Verify that the value of your expression is the
;; same as that of the original expression.
;; a. (let ([x 'a] [y 'b])
;;      (list (let ([x 'c]) (cons x y))
;;            (let ([y 'd]) (cons x y))))
;; b. (let ([x '((a b) c)])
;;        (cons (let ([x (cdr x)])
;;                (car x))
;;              (let ([x (car x)])
;;                (cons (let ([x (cdr x)])
;;                        x)
;;                      (cdr x)))))
;;
;; Answer:
;;  (let ([x 'a] [y 'b])
;;    (list (let ([z 'c]) (cons z y))
;;          (let ([z 'd]) (cons x z))))  => ((c . b) (a . d))
;;  (let ([x '((a b) c)])
;;      (cons (let ([y (cdr x)])
;;              (car y))
;;            (let ([y (car x)])
;;              (cons (let ([z (cdr y)])
;;                      z)
;;                    (cdr y)))))   => (c (b) b)

#|================================================================================================|#

;; Exercise 2.5.1
;;
;; Determine the values of the expressions below.
;; a. (let ([f (lambda (x) x)]) (f 'a))
;; b. (let ([f (lambda x x)])
;;      (f 'a))
;; c. (let ([f (lambda (x . y) x)])
;;      (f 'a))
;; d. (let ([f (lambda (x . y) y)])
;;      (f 'a))
;;
;; Answer:
;;  a
;;  (a)
;;  a
;;  ()

#|================================================================================================|#

;; Exercise 2.5.2
;;
;; How might the primitive procedure list be defined?

;; Answer:
(define list (lambda x x))

#|================================================================================================|#

;; Exercise 2.5.3
;;
;; List the variables that occur free in each of the lambda expressions below.
;; Do not omit variables that name primitive procedures such as + or cons.
;; a. (lambda (f x) (f x))
;; b. (lambda (x) (+ x x))
;; c. (lambda (x y) (f x y))
;; d. (lambda (x) (cons x (f x y)))
;; e. (lambda (x)
;;      (let ([z (cons x y)])
;;          (x y z)))
;; f. (lambda (x)
;;      (let ([y (cons x y)])
;;          (x y z)))
;;
;; Answer:
;;  a. none
;;  b. +
;;  c. f
;;  d. cons f y
;;  e. let cons y
;;  f. let cons z

#|================================================================================================|#

;; Exercise 2.6.1
;;
;; What would happen if you were to type (double-any double-any double-any),
;; given the definition of double-any as following:
;;  (define double-any (lambda (f x)
;;      (f x x)))
;;
;; Answer:
;;  It's an infinite recursion, the call will never return and one core of cpu will be totally occupied.
;;  Because of its tail-call nature, the stack wouldn't overflow either.

#|================================================================================================|#

;; Exercise 2.6.2
;;
;; A more elegant (though possibly less efficient) way to define cadr and cddr than given in this
;; section is to define a procedure that composes two procedures to create a third. Write the procedure
;; compose, such that (compose p1 p2) is the composition of p1 and p2(assuming both take one argument).
;; That is, (compose p1 p2) should return a new procedure of onear gument that applies p1 to the result
;; of applying p2 to the argument. Use compose to define cadr and cddr.

;; Answer: 
(define compose
  (lambda (x y)
    (lambda (z)
      (x (y z)))))

(define cadr
  (compose car cdr))

(define cddr
  (compose cdr cdr))

#|================================================================================================|#

;; Exercise 2.6.3
;;
;; Scheme also provides caar, cdar, caaar, caadr, and so on, with any combination of up to four
;; a's (representing car) and d's (representing cdr) between the c and the r.
;; Define each of these with the compose procedure of the preceding exercise.

;; Answer:
(define compose
  (lambda (x y)
    (lambda (z)
      (x (y z)))))

(define caar (compose car car))
(define cadr (compose car cdr))
(define cdar (compose cdr car))
(define cddr (compose cdr cdr))

(define caaar (compose car caar))
(define caadr (compose car cadr))
(define cadar (compose car cdar))
(define caddr (compose car cddr))
(define cdaar (compose cdr caar))
(define cdadr (compose cdr cadr))
(define cddar (compose cdr cdar))
(define cdddr (compose cdr cddr))

(define caaaar (compose car caaar))
(define caaadr (compose car caadr))
(define caadar (compose car cadar))
(define caaddr (compose car caddr))
(define cadaar (compose car cdaar))
(define cadadr (compose car cdadr))
(define caddar (compose car cddar))
(define cadddr (compose car cdddr))
(define cdaaar (compose cdr caaar))
(define cdaadr (compose cdr caadr))
(define cdadar (compose cdr cadar))
(define cdaddr (compose cdr caddr))
(define cddaar (compose cdr cdaar))
(define cddadr (compose cdr cdadr))
(define cdddar (compose cdr cddar))
(define cddddr (compose cdr cdddr))

#|================================================================================================|#

;; Exercise 2.7.1
;;
;; Define the predicate atom?, which returns true if its argument is not a pair and false if it is.

;; Answer:
(define atom?
  (lambda (x)
    (not (pair? x))))

#|================================================================================================|#

;; Exercise 2.7.2
;;
;; The procedure length returns the length of its argument, which must be a list. For example,
;; (length '(a b c)) is 3. Using length, define the procedure shorter, which returns the shorter
;; of two list arguments. Have it return the first list if they have the same length.
;; (shorter '(a b) '(c d e)) => (a b)
;; (shorter '(a b) '(c d)) => (a b)
;; (shorter '(a b) '(c)) => (c)

;; Answer:
(define shorter
  (lambda (a b)
    (let ([la (length a)]
          [lb (length b)])
      (if (> la lb) b a))))

#|================================================================================================|#

;; Exercise 2.8.1
;;
;; Describe what would happen if you switched the order of the arguments to cons in the definition
;; of tree-copy.
;;
;; Answer:
;;  The left sub-tree and right sub-tree of each tree-node will exchange places.
;;  (define tree-copy
;;    (lambda (tr)
;;      (if (not (pair? tr))
;;        tr
;;        (cons (tree-copy (cdr tr))
;;              (tree-copy (car tr))))))
;;  (tree-copy '((a . b) . (c . d))) =>  ((d . c) b . a)

#|================================================================================================|#

;; Exercise 2.8.2
;;
;; Consult Section 6.3 for the description of append and define a two-argument version of it.
;; What would happen if you switched the order of the arguments in the call to append within
;; your definition of append?

;; Answer:
(define append1
  (lambda (ls x)
    (if (not (list? ls))
        (assertion-violation 'append1 "first argument isn't a legal list!"))
    (if (null? ls)
        x
        (cons (car ls)
              (append (cdr ls) x)))))

#|================================================================================================|#

;; Exercise 2.8.3
;;
;; Define the procedure make-list, which takes a nonnegative integer n and an object and 
;; returns a new list, n long, each element of which is the object.
;;
;;  (make-list 7 '()) (() () () () () () ())
;;
;; [Hint: The base test should be (= n 0), and the recursion step should involve (- n 1).
;; Whereas () is the natural base case for recursion on lists, 0 is the natural base case for
;; recursion on nonnegative integers. Similarly, subtracting 1 is the natural way to bring a 
;; nonnegative integer closer to 0.]

;; Answer:
(define make-list
  (lambda (n obj)
    (if (<= n 0)
        '()
        (cons obj
              (make-list (- n 1) obj)))))

#|================================================================================================|#

;; Exercise 2.8.4
;;
;; The procedures list-ref and list-tail return the nth element and nth tail of a list ls.
;;
;;  (list-ref '(1 2 3 4) 0) => 1
;;  (list-tail '(1 2 3 4) 0) => (1 2 3 4)
;;  (list-ref '(a short (nested) list) 2) => (nested)
;;  (list-tail '(a short (nested) list) 2) => ((nested) list)
;;
;; Define both procedures.

;; Answer:
(define list-ref
  (lambda (ls n)
    (cond
      [(or (< n 0) (>= n (length ls))) (assertion-violation 'list-ref "invalid nth argument" n)]
      [(= n 0) (car ls)]
      [else (list-ref (cdr ls) (1- n))])))

(define list-tail
  (lambda (ls n)
    (cond
      [(or (< n 0) (> n (length ls))) (assertion-violation 'list-tail "invalid nth argument" n)]
      [(= n 0) ls]
      [else (list-tail (cdr ls) (1- n))])))

#|================================================================================================|#

;; Exercise 2.8.5
;;
;; Exercise 2.7.2 had you use length in the definition of shorter, which returns the shorter of its
;; two list arguments, or the first if the two have the same length. Write shorter without using length.
;;
;; [Hint: Define a recursive helper, shorter?, and use it in place of the length comparison.]

;; Answer:
(define shorter?
  (lambda (a b)
    (cond
      [(null? a) #t]
      [(null? b) #f]
      [else (shorter? (cdr a) (cdr b))])))

(define shorter
  (lambda (a b)
    (if (shorter? a b) a b)))

#|================================================================================================|#

;; Exercise 2.8.6
;;
;; All of the recursive procedures shown so far have been directly recursive.
;; That is, each procedure directly applies itself to a new argument.
;; It is also possible to write two procedures that use each other, resulting in indirect recursion.
;; Define the procedures odd? and even?, each in terms of the other.
;;
;; (even? 17) => #f
;; (odd? 17) => #t
;;
;; [Hint: What should each return when its argument is 0?]

;; Answer:
(define even?
  (lambda (n)
    (cond
      [(= n 0) #f]
      [(odd? (1- n)) #t]
      [else #f])))

(define odd?
  (lambda (n)
    (cond
      [(= n 1) #f]
      [(even? (1- n)) #t]
      [else #f])))

#|================================================================================================|#

;; Exercise 2.8.7
;;
;; Use map to define a procedure, transpose, that takes a list of pairs and returns a pair of
;; lists as follows.
;;
;; (transpose'((a . 1) (b . 2) (c . 3))) => ((a b c) 1 2 3)
;;
;; [Hint: ((a b c) 1 2 3) is the same as ((a b c) . (1 2 3)).]

;; Answer:
(define transpose
  (lambda (ps)
    (cons (map car ps) (map cdr ps))))

#|================================================================================================|#

;; Exercise 2.9.1
;;
;; Modify make-counter to take two arguments: an initial value for the counter to use in place of 0
;; and an amount to increment the counter by each time.

;; Answer:
(define make-counter
  (lambda (a b)
    (let ([init a] [step b])
        (lambda ()
          (let ([v init])
            (set! init (+ init step))
            v)))))

#|================================================================================================|#

;; Exercise 2.9.2
;;
;; Look up the description of case in Section 5.3. Replace the cond expression in make-stack with
;; an equivalent case expression. Add mt? as a second name for the empty? message.

;; Answer:
(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (case msg
          [(mt? empty?) (null? ls)]
          [(push!) (set! ls (cons (car args) ls))]
          [(top) (car ls)]
          [(pop!) (set! ls (cdr ls))]
          [else "oops"])))))

#|================================================================================================|#

;; Exercise 2.9.3
;;
;; Modify the stack object to allow the two messages ref and set!. 
;; (stack 'ref i) should return the ith element from the top of the stack.
;; (stack 'ref 0) should be equivalent to (stack 'top).
;; (stack 'set! i v) should change the ith element from the top of the stack to v.
;;
;; (define stack (make-stack))
;; (stack 'push! 'a)
;; (stack 'push! 'b)
;; (stack 'push! 'c)
;; (stack 'ref 0) => c
;; (stack 'ref 2) => a
;; (stack 'set! 1 'd)
;; (stack 'ref 1) => d
;; (stack 'top) => c
;; (stack 'pop!)
;; (stack 'top) => d
;;
;; [Hint: Use list-ref to implement ref and list-tail with set-car! to implement set!.]

;; Answer:
(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (case msg
          [(mt? empty?) (null? ls)]
          [(push!) (set! ls (cons (car args) ls))]
          [(top) (car ls)]
          [(pop!) (set! ls (cdr ls))]
          [(ref) (list-ref ls (car args))]
          [(set!) (set-car! (list-tail ls (car args))
                            (cadr args))]
          [else "oops"])))))

#|================================================================================================|#

;; Exercise 2.9.4
;;
;; Scheme supports vectors as well as lists. Like lists, vectors are aggregate objects that contain
;; other objects. Unlike lists, vectors have a fixed size and are laid out in one flat block of memory,
;; typically with a header containing the length of the vector, as in the ten-element vector below.
;;
;; +----+---+---+---+---+---+---+---+---+---+---+
;; | 10 | a | b | c | d | e | f | g | h | i | j |
;; +----+---+---+---+---+---+---+---+---+---+---+
;;
;; This makes vectors more suitable for applications needing fast access to any element of the aggregate
;; but less suitable for applications needing data structures that grow and shrink as needed.
;; Look up the basic vector operations in Section 6.9 and reimplement the stack object to use a vector
;; instead of a list to hold the stack contents. Include the ref and set! messages of Exercise 2.9.3.
;; Have the new make-stack accept a size argument n and make the vector length n, but do not otherwise
;; change the external (abstract) interface.

;; Answer:
(define make-stack
  (lambda (n)
    (let ([v (make-vector n)] [top 0])
      (lambda (msg . args)
        (case msg
          [(mt? empty?) (= 0 top)]
          [(push!) (if (= top n)
                     (assertion-violation 'stack "stack is full!"))
                   (vector-set! v top (car args))
                   (set! top (1+ top))]
          [(top) (if (= top 0)
                   (assertion-violation 'stack "stack is empty!"))
                 (vector-ref v (1- top))]
          [(pop!) (if (= top 0)
                   (assertion-violation 'stack "stack is empty!"))
                  (set! top (1- top))]
          [(ref) (vector-ref v
                             (- top (car args) 1))]
          [(set!) (vector-set! v
                               (- top (car args) 1)
                               (cadr args))]
          [else "oops"])))))

#|================================================================================================|#

;; Exercise 2.9.5
;;
;; Define a predicate, emptyq?, for determining if a queue is empty. Modify getq and delq! to raise
;; an exception when an empty queue is found, using assertion-violation.

;; Answer:
(define make-queue
  (lambda ()
    (let ([end (cons 'ignored '())])
      (cons end end))))

(define emptyq?
  (lambda (q)
    (eqv? (car q) (cdr q))))

(define putq!
  (lambda (q v)
    (let ([end (cons 'ignored '())])
      (set-car! (cdr q) v)
      (set-cdr! (cdr q) end)
      (set-cdr! q end))))

(define getq
  (lambda (q)
    (if (emptyq? q) (assertion-violation 'getq "empty queue!"))
    (car (car q))))

(define delq!
  (lambda (q)
    (if (emptyq? q) (assertion-violation 'delq "empty queue!"))
    (set-car! q (cdr (car q)))))

#|================================================================================================|#

;; Exercise 2.9.6
;;
;; In the queue implementation, the last pair in the encapsulated list is a placeholder,
;; i.e., it never holds anything useful. Recode the queue operators to avoid this wasted pair.
;; Make sure that the series of queue operations given earlier works with the new implementation.
;; Which implementation do you prefer?

;; Answer:
;;  I don't like new implementation for two reason:
;;  1. Careful care have to be taken when dealing with empty queue;
;;  2. More code have to be written.
(define make-queue
  (lambda ()
    (cons 'ignored '())))

(define emptyq?
  (lambda (q)
    (eqv? (car q) 'ignored)))

(define putq!
  (lambda (q v)
    (let ([new-pair (cons v '())])
      (if (emptyq? q)
        (begin
          (set-car! q new-pair)
          (set-cdr! q new-pair))
        (begin
          (set-cdr! (cdr q) new-pair)
          (set-cdr! q new-pair))))))

(define getq
  (lambda (q)
    (if (emptyq? q) (assertion-violation 'getq "empty queue!"))
    (car (car q))))

(define delq!
  (lambda (q)
    (if (emptyq? q) (assertion-violation 'delq "empty queue!"))
    (set-car! q (cdr (car q)))
    (if (null? (car q)) (set-car! q 'ignored))))

#|================================================================================================|#

;; Exercise 2.9.7
;;
;; Using set-cdr!, it is possible to create cyclic lists. For example, the following expression 
;; evaluates to a list whose car is the symbol a and whose cdr is the list itself.
;;
;; (let ([ls (cons 'a '())])
;;   (set-cdr! ls ls)
;;   ls)
;;
;; What happens when you enter the above expression during an interactive Scheme session?
;; What will the implementation of length on page 42 do when given a cyclic list?
;; What does the built-in length primitive do?
;;
;; Answer:
;;  1. Scheme will give a warning about cyclic list, which can't be printed out.
;;  2. The length function will never return.
;;  3. An exception about circular list will be raised.

#|================================================================================================|#

;; Exercise 2.9.8
;;
;; Define the predicate list?, which returns #t if its argument is a proper list and #f otherwise
;; (see Section 6.3). It should return #f for cyclic lists as well as for lists terminated by objects
;; other than ().
;;
;; (list? '()) => #t
;; (list? '(1 2 3)) => #t
;; (list? '(a . b)) => #f
;; (list? (let ([ls (cons 'a '())])
;;          (set-cdr! ls ls) ls)) => #f
;;
;; First write a simplified version of list? that does not handle cyclic lists, then extend this to
;; handle cyclic lists correctly. Revise your definition until you are satisfied that it is as clear
;; and concise as possible. 
;;
;; [Hint: Use the following "hare and tortoise" algorithm to detect cycles. Define a recursive help
;; procedure of two arguments, the hare and the tortoise. Start both the hare and the tortoise at the
;; beginning of the list. Have the hare advance by two cdrs each time the tortoise advances by one cdr.
;; If the hare catches the tortoise, there must be a cycle.]

;; Answer:
(define list?
  (let ()
    (define has-cycle?
      (lambda (hare tortoise)
        (cond
          [(or (null? hare)
               (null? (cdr hare))
               (null? (cddr hare))) #f]
          [(eqv? hare tortoise) #t]
          [else (has-cycle? (cddr hare) (cdr tortoise))])))

    (lambda (ls)
      (cond 
        [(null? ls) #t]
        [(not (pair? ls)) #f]
        [(has-cycle? ls ls) #f]
        [else (list? (cdr ls))]))))
