;Author: Matthew Peek / Michael Luney
#lang racket
;Last Modified: 5 May 2018
;Project 3

(require racket/trace)
(provide (all-defined-out))

(define add-poly
  (lambda (p1 p2)
    (if (same-variable? p1 p2)
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD POLY"
               (list p1 p2)))))

(define mul-poly
  (lambda (p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2)))))

(define add-terms
  (lambda (tlist1 tlist2)
    (cond ((empty-termlist? tlist1) tlist2)
          ((empty-termlist? tlist2) tlist1)
          (else
           (let ((t1 (first-term tlist1))
                 (t2 (first-term tlist2)))
                 (cond ((> (order t1) (order t2))
                        (adjoin-term t1 (add-terms (rest-terms tlist1) tlist2)))
                       ((< (order t1) (order t2))
                        (adjoin-term t2 (add-terms tlist1 (rest-terms tlist2))))
                       (else
                        (adjoin-term
                         (make-term (add-coeff t1 t2)(order t1))
                         (add-terms (rest-terms tlist1)
                                    (rest-terms tlist2))))))))))

(define same-variable?
  (lambda (p1 p2)
    (eq? (variable p1) (variable p2))))

(define make-term
  (lambda (coeff order)
    (list coeff order)))

(define order
  (lambda (x)
    (car(cdr x))))

(define coeff
  (lambda (x)
    (car x)))

(define variable
  (lambda (poly)
    (first poly)))

(define first-term
  (lambda (t1)
    (car t1)))

(define rest-terms
  (lambda (t1)
    (rest t1)))

(define (the-empty-termlist) '())

(define (make-poly var . terms)
  (list var terms))

(define term-list
  (lambda(poly)
    (car(cdr poly))))





(define (mul-terms t1 t2)
    (if (empty-termlist? t1)
        (the-empty-termlist)
        (add-terms (mult-all (first-term t1) t2)
                   (mul-terms (rest-terms t1) t2))))
  (define (mult-all t1 x)
    (if (empty-termlist? x)
        (the-empty-termlist)
        (let ((t2 (first-term x)))
          (adjoin-term
           (make-term (* (coeff t1) (coeff t2))
                      (+ (order t1) (order t2)))
           (mult-all t1 (rest-terms x))))))


(define empty-termlist?
         (lambda (t1)
           (empty? t1)))
                   

(define adjoin-term
         (lambda (var1 var2)
           (cons var1 var2)))
           

(define add-coeff
  (lambda (var1 var2)
    (+ (coeff var1)(coeff var2))))

;needs work 
(define zero-poly?
  (lambda (var)
    (zero? (coeff (car var)))))

;Needs work
(define equal-poly?
  (lambda (p1 p2)
    (eq? (order p1) (order p2))))




(define (div-terms t1 t2)
    (if (empty-termlist? t1)
        (the-empty-termlist)
        (add-terms (div-all (first-term t1) t2)
                   (div-terms (rest-terms t1) t2))))
  (define (div-all t1 x)
    (if (empty-termlist? x)
        (the-empty-termlist)
        (let ((t2 (first-term x)))
          (adjoin-term
           (make-term (/ (coeff t1) (coeff t2))
                      (+ (order t1) (order t2)))
           (div-all t1 (rest-terms x))))))


;Helpers for subtract-terms

(define Sub-order-one
  (lambda (x y)
    (= (order(car x)) (order(car y)))))

(define Sub-coeff-one
  (lambda (x y)
    (- (coeff(car x)) (coeff(car y)))))

(define Sub-order-two
  (lambda (x y)
    (= (car(cdr(order x))) (car(cdr(order y))))))

(define Sub-coeff-two
  (lambda (x y)
    (- (car(coeff(cdr x))) (car(coeff(cdr y))))))

  (define Sub-order-three
  (lambda (x y)
    (= (car(cdr(order(cdr x)))) (car(cdr(order(cdr y)))))))

  (define Sub-coeff-three
  (lambda (x y)
    (- (car(coeff(cdr(cdr x)))) (car(coeff(cdr(cdr y)))))))
;End Sub-term helpers

(define (sub-terms t1 t2)
    (if (empty-termlist? t1)
        (the-empty-termlist)
        (add-terms (sub-all (first-term t1) t2)
                   (sub-terms (rest-terms t1) t2))))
  (define (sub-all t1 x)
    (if (empty-termlist? x)
        (the-empty-termlist)
        (let ((t2 (first-term x)))
          (adjoin-term
           (make-term (- (coeff t1) (coeff t2))
                      (+ (order t1) (order t2)))
           (sub-all t1 (rest-terms x))))))

;Derivative function
(define (der-terms t1)
   (if (empty-termlist? t1)
       (the-empty-termlist)
       (div-terms (der-all (first-term t1))
                  (der-terms (rest-terms t1)))))
  (define (der-all t1 x)
    (if (empty-termlist? x)
        (the-empty-termlist)
    (let ((t1 (first-term x)))
      (adjoin-term
       (make-term (* (coeff t1) (order t1))
                  (- (order 1)))
       (der-all t1 (rest-terms x))))))

(trace variable)
(trace add-poly)
(trace make-poly)
(trace same-variable?)
(trace add-terms)
(trace empty-termlist?)
(trace first-term)
(trace adjoin-term)
(trace make-term)
(trace mul-terms)
(trace div-terms)
(trace sub-terms)
(trace der-terms)
(mul-terms  (term-list (make-poly 'x (make-term 2 3) (make-term 3 2) (make-term 4 1))) (term-list (make-poly 'x (make-term 3 3) (make-term 2 2))))