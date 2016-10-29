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
