#lang play
(require "main.rkt")
(print-only-errors)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                 TESTS BASE                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (run-val '(+ 1 2)) 3)
(test (run-val '(< 1 2)) #t)
(test (run-val '(- 2 1)) 1)
(test (run-val '(* 2 3)) 6)
(test (run-val '(= (+ 2 1) (- 4 1))) #t)
(test (run-val '(and #t #f)) #f)
(test (run-val '(or #t #f)) #t)
(test (run-val '(not (not #t))) #t)
(test (run-val '(if (not #f) (+ 2 1) 4)) 3)
(test (run-val '(local ([define x 5])
                  (seqn {+ x 1}
                        x))) 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                  SUS TESTS                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Clases y objetos como valores
(test (run-val '(local
                      [(define c (class
                                     (field x 1)
                                   (method sum (y) (+ (get this x) y))))
                       (define o (new c))]
                      (get o x))) 1)
(test/exn (run-val '(local
                      [(define c (class
                                     (field x 1)
                                   (method sum (y) (+ (get this x) y))))
                       (define o (new c))]
                      (get o y))) "field not found")
(test (run-val '(local
                      [(define c (class
                                     (field x 1)
                                   (method sum (y) (+ (get this x) y))))
                       (define o (new c))]
                      (send o sum 3))) 4)
(test/exn (run-val '(local
                      [(define c (class
                                     (field x 1)
                                   (method sum (y) (+ (get this x) y))))
                       (define o (new c))]
                      (send o set-x 2))) "method not found")
(test (run-val '(local
                      [(define c (class
                                     (field x 1)
                                   (method sum (y) (+ (get this x) y))
                                   (method sum2 (a b) (+ (get this x) (+ a b)))))
                       (define o (new c))]
                      (send o sum2 3 4))) 8)
(test (run-val '(local
                  [(define c (class
                                 (field x 1)
                               (field y 2)
                               (method sum (z) (+ (get this x) (+ (get this y) z)))
                               (method set-x (val) (set this x val))))
                   (define o (new c))]
                  (seqn
                   (send o set-x (+ 1 3))
                   (+ (send o sum 3) (get o y)))))
      11)

(test (run-val '(local
                  [(define A
                     (class
                         (method apply (c)
                                 (send (new c) m))))
                   (define ins (new A))]
                  (send ins apply (class
                                      (field x 2) 
                                    (method m () (get this x))))))
      2)