#lang racket

(require rackunit
         "project-3.rkt")

(check-equal? (make-term  3 5) '(3 5))

(check-true (same-variable? (make-poly 'x (make-term 3 5) (make-term 1 2) (make-term 3 0)) (make-poly 'x (make-term 5 3) (make-term 5 0))))
(check-false (same-variable? (make-poly 'x (make-term 1 1)) (make-poly 'y)))

(check-equal? (variable (make-poly 'x)) 'x)
(check-not-equal? (variable (make-poly 'x)) 'y)
(check-equal? (variable (make-poly 'x (make-term 3 5))) 'x)

(check-equal? (adjoin-term (make-term 3 5) '()) '((3 5)))
(check-equal? (adjoin-term (make-term 3 5) '((4 7))) '((3 5) (4 7)))
(check-equal? (adjoin-term (make-term 3 5) '((1 1) (4 7))) '((3 5) (1 1) (4 7)))
(check-equal? (adjoin-term (make-term 3 5) '((3 5))) '((3 5) (3 5)))
(check-equal? (adjoin-term (make-term 3 5) '((1 1) (3 5) (4 7))) '((3 5) (1 1) (3 5) (4 7)))


(check-equal? (add-coeff (make-term 0 0) (make-term 1 0)) 1)
(check-equal? (add-coeff (make-term 0 0) (make-term 1 1)) 1)
(check-equal? (add-coeff (make-term 5 3) (make-term 8 2)) 13)

(check-equal? (order (make-term 0 0)) 0)
(check-equal? (order (make-term 0 1)) 1)
(check-equal? (order (make-term 1 1)) 1)

(check-equal? (rest-terms '((1 3))) '())
(check-equal? (rest-terms '((1 3) (1 1))) '((1 1)))
(check-equal? (rest-terms '((1 3) (1 1) (5 3))) '((1 1) (5 3)))

(check-equal? (the-empty-termlist) '())

(check-equal? (term-list (make-poly 'x (make-term 3 5) (make-term 1 2) (make-term 3 0))) '((3 5) (1 2) (3 0)))
(check-equal? (term-list (make-poly 'x)) (the-empty-termlist))


(check-true (empty-termlist? (the-empty-termlist)) '())
(check-false (empty-termlist? (term-list (make-poly 'x (make-term 3 5) (make-term 1 2) (make-term 3 0)))) '())

(check-equal? (multiply-list (term-list (make-poly 'x (make-term 2 3) (make-term 3 2) (make-term 4 1))) (term-list (make-poly 'x (make-term 3 3) (make-term 2 2)))) '((6 9)(4 6)(9 6)(6 4)(12 3) (8 2)))
(mul-terms (term-list (make-poly 'x (make-term 2 3) (make-term 3 2) (make-term 4 1))) (term-list (make-poly 'x (make-term 3 3) (make-term 4 2) (make-term 5 1))))


(check-true (equal-poly? (make-poly 'x (make-term 3 2) (make-term 2 3) (make-term 4 1)) (make-poly 'x (make-term 3 2) (make-term 2 3) (make-term 4 1))))

(check-false (equal-poly?  (make-poly 'x (make-term 2 3) (make-term 3 2) (make-term 4 1)) (make-poly 'x (make-term 3 3) (make-term 4 2) (make-term 5 1))))
 

