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
