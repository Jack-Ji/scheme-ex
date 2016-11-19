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

;; Excercise 3.1.1
;;
;; Write out the expansion steps necessary to expand
;;  
;; (let ([x (memv 'a ls)])
;;  (and x (memv 'b x)))
;;
;; into core forms.
;;
;; Answer:
;;  (let ([x (memv 'a ls)])
;;   (and x (memv 'b x)))
;;  --> ((lambda (x)
;;          (and x (memv 'b x)))
;;        (memv 'a ls))
;;  --> ((lambda (x)
;;          (if x (and (mem 'b x)) #f))
;;        (mem 'a ls)) 
;;  --> ((lambda (x)
;;          (if x (mem 'b x) #f))
;;        (mem 'a ls))

#|================================================================================================|#

;; Excercise 3.1.2
;;
;; Write out the expansion steps necessary to expand
;;
;; (or (memv x '(a b c)) (list x))
;;
;; into core forms.
;;
;; Answer:
;;  (or (memv x '(a b c)) (list x))
;;  --> (let ([t (memv x '(a b c))])
;;          (if t t (or (list x))))
;;  --> (let ([t (memv x '(a b c))])
;;          (if t t (list x)))

#|================================================================================================|#

;; Excercise 3.1.3
;;
;; let* is similar to let but evaluates its bindings in sequence.
;; Each of the right-hand-side expressions is within the scope of the earlier bindings.
;;
;; (let* ([a 5]
;;        [b (+ a a)]
;;        [c (+ a b)])
;;   (list a b c)) => (5 10 15)
;;
;; let* can be implemented as nested let expressions.
;; For example, the let* expression above is equivalent to the nested let expressions below.
;;
;; (let ([a 5])
;;   (let ([b (+ a a)])
;;     (let ([c (+ a b)])
;;       (list a b c)))) => (5 10 15)
;;
;; Define let* with define-syntax.

;; Answer:
(define-syntax let*
  (syntax-rules ()
    ; no bindings
    [(_ () b1 b2 ...)
     (let () b1 b2 ...)]

    ; one binding
    [(_ ((x e)) b1 b2 ...)
     (let ((x e)) b1 b2 ...)]

    ; two or more bindings
    [(_ ((x1 e1) (x2 e2) (x3 e3) ...) b1 b2 ...)
     (let ((x1 e1))
       (let* ((x2 e2) (x3 e3) ...)
         b1 b2 ...))]))

#|================================================================================================|#

;; Excercise 3.1.4
;;
;; As we saw in Section 2.9, it is legal to omit the third, or alternative, subexpression of an if
;; expression. Doing so, however, often leads to confusion. Scheme provides two syntactic forms,
;; when and unless, that may be used in place of such "one-armed" if expressions.
;;
;; (when test expr1 expr2 ...)
;; (unless test expr1 expr2 ...)
;;
;; With both forms, test is evaluated first. For when, if test evaluates to true,
;; the remaining forms are evaluated in sequence as if enclosed in an implicit begin expression.
;; If test evaluates to false, the remaining forms are not evaluated, and the result is unspecified.
;; unless is similar except that the remaining forms are evaluated only if test evaluates to false.
;;
;; (let ([x 3])
;;   (unless (= x 0) (set! x (+ x 1)))
;;   (when (= x 4) (set! x (* x 2)))
;;   x) => 8
;;
;; Define when as a syntactic extension in terms of if and begin, and define unless in terms of when.

;; Answer:
(define-syntax when
  (syntax-rules ()
    [(_ t e1 e2 ...)
     (if t (begin e1 e2 ...))]))

(define-syntax unless
  (syntax-rules ()
    [(_ t e1 e2 ...)
     (when (not t) e1 e2 ...)]))

#|================================================================================================|#

;; Excercise 3.2.2
;;
;; Rewrite factor using letrec to bind f in place of named let. Which version do you prefer?

;; Answer:
(define factorial
  (lambda (n)
    (letrec ([f
              (lambda (a x)
                (cond
                  [(<= x 1) a]
                  [else (f (* a x) (- x 1))]))])
      (f 1 n))))

#|================================================================================================|#

;; Excercise 3.2.3
;;
;; Can the letrec expression below be rewritten using named let? If not, why not? If so, do it.
;; (letrec ([even? (lambda (x)
;;                   (or (= x 0)
;;                       (odd? (- x 1))))]
;;          [odd? (lambda (x)
;;                  (and (not (= x 0)) (even? (- x 1))))])
;;   (even? 20))
;;
;; Answer:
;;  No way, cause varaibles can't appear free in the body of named let!

#|================================================================================================|#

;; Excercise 3.2.4
;;
;; Rewrite both definitions of fibonacci given in this section to count the number of recursive
;; calls to fib, using a counter similar to the one used in the cons-count example of Section 2.9.
;; Count the number of recursive calls made in each case for several input values. What do you notice?

;; Answer:
(define call-fib-counter 0)

(define fibonacci
  (lambda (n)
    (letrec ([f (lambda (i)
                  (set! call-fib-counter (1+ call-fib-counter))
                  (cond
                    [(= i 0) 0]
                    [(= i 1) 1]
                    [else (+ (f (- i 1)) (f (- i 2)))]))])
      (f n))))

(define fibonacci
  (lambda (n)
    (if (= n 0)
      0
      (letrec ([f (lambda (i a1 a2)
                    (set! call-fib-counter (1+ call-fib-counter))
                    (if (= i 1)
                      a1
                      (f (- i 1) (+ a1 a2) a1)))])
        (f n 1 0)))))

#|================================================================================================|#

;; Excercise 3.2.5
;;
;; Augment the definition of let given in Section 3.1 to handle named let as well as unnamed let,
;; using two rules.

;; Answer:
(define-syntax let 
  (syntax-rules ()
    [(_ name ((x e) ...) b1 b2 ...)
     (letrec ([name (lambda (x ...) b1 b2 ...)]) (name e ...))]
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

#|================================================================================================|#

;; Excercise 3.2.6
;;
;; The following definition of or is simpler than the one given in Section 3.1.
;; (define-syntax or ; incorrect!
;;   (syntax-rules ()
;;     [(_) #f]
;;     [(_ e1 e2 ...)
;;      (let ([t e1])
;;        (if t t (or e2 ...)))]))
;;     
;; Say why it is not correct. [Hint: Think about what would happen if this version of or were used
;; in the even? and odd? example given on page 66 for very large inputs.]
;;
;; Answer:
;;      (letrec ([even? (lambda (x)
;;                        (or (= x 0)
;;                            (odd? (- x 1))))]
;;               [odd? (lambda (x)
;;                       (and (not (= x 0)) (even? (- x 1))))])
;;        (list (even? 20) (odd? 20))) => (#t #f)
;;  In this incorrect version, application of odd? in even? wouldn't be a tail call when there's only
;;  one parameter for `or`. As a result, when given a very large number, scheme's runtime stack will
;;  grow out of control!

#|================================================================================================|#

;; Excercise 3.2.7
;;
;; The definition of factor is not the most efficient possible.
;;
;;  First, no factors of n besides n itself can possibly be found beyond (sqrt n).
;;  Second, the division (/ n i) is performed twice when a factor is found.
;;  Third, after 2, no even factors can possibly be found.
;;
;; Recode factor to correct all three problems. Which is the most important problem to solve?
;; Are there any additional improvements you can make?

;; Answer:
;;  Addtional optimizations: 
;;  1. result of `(sqrt n)` has been added as parameter to avoid redundant calculation.
;;  2. use tail-recursion, add accumulating list as parameter.
(define factor
  (lambda (n)
    (let f ([n n] [i 2] [t (sqrt n)] [ls '()])
      (let ([r (/ n i)])
        (cond
          [(>= i t) (cons n ls)]
          [(integer? r) (f r i (sqrt r) (cons i ls))]
          [(> i 2) (f n (+ i 2) t ls)]
          [else (f n (+ i 1) t ls)])))))
;;
;; > (define factor
;;     (lambda (n)
;;       (let f ([n n] [i 2])
;;         (cond
;;           [(>= i n) (list n)]
;;           [(integer? (/ n i))
;;            (cons i (f (/ n i) i))]
;;           [else (f n (+ i 1))]))))
;; > (time (factor 36288111))
;; (time (factor 36288111))
;;     46 collections
;;     3.278358000s elapsed cpu time, including 0.001942000s collecting
;;     3.281544000s elapsed real time, including 0.002116000s collecting
;;     387102592 bytes allocated, including 388595136 bytes reclaimed
;; (3 12096037)
;;
;; > (define factor
;;     (lambda (n)
;;       (let f ([n n] [i 2] [t (sqrt n)] [ls '()])
;;         (let ([r (/ n i)])
;;           (cond
;;             [(>= i t) (cons n ls)]
;;             [(integer? r) (f r i (sqrt r) (cons i ls))]
;;             [(> i 2) (f n (+ i 2) t ls)]
;;             [else (f n (+ i 1) t ls)])))))
;; > (time (factor 36288111))
;; (time (factor 36288111))
;; no collections
;; 0.000334000s elapsed cpu time
;; 0.000332000s elapsed real time
;; 83760 bytes allocated
;; (12096037 3)
