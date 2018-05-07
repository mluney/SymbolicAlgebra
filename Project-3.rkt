;Author: Matthew Peek / Michael Luney
#lang racket
;Last Modified: 6 May 2018
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
;term
(define term-list
  (lambda(poly)
    (car(cdr poly))))




;Mul-terms
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

;Empty-termlist
(define empty-termlist?
         (lambda (t1)
           (empty? t1)))
                   
;Adjoin-terms
(define adjoin-term
         (lambda (var1 var2)
           (cons var1 var2)))
           

(define add-coeff
  (lambda (var1 var2)
    (+ (coeff var1)(coeff var2))))

;Zero-poly?
(define zero-poly?
  (lambda (var)
    (zero? (coeff (car var)))))

;Equal-poly?
(define equal-poly?
  (lambda (p1 p2)
    (equal? p1 p2)))

;Negation
(define (negation t1)
  (if(empty-termlist? t1)
     (the-empty-termlist)
     (let ((x (first-term t1)))
     (adjoin-term
           (make-term (* (coeff x) -1) (order x))
           (negation (rest-terms t1))))))

;Value Testing 
(define (value poly x)
  (if (empty-termlist? (term-list poly))
      (the-empty-termlist)
      (add-value(get-it (term-list poly) x))))

(define (get-it t1 x)
  (if (empty-termlist? t1)
      (the-empty-termlist)
      (let ((z (first-term t1)))
       (cons (*(expt x (order z)) (coeff z))
             (get-it (rest-terms t1) x)))))

(define (add-value lst)
   (if
    (null? lst)
    0
    (+ (car lst)(add-value(cdr lst)))))


      
             
;Here is a free poly
(define (poly1) (make-poly 'x(make-term 2 3) (make-term 1 2) (make-term 4 1)))


;Divide Terms
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
                      (- (order t1) (order t2)))
           (div-all t1 (rest-terms x))))))



;Subtract Terms
(define (sub-terms t1 t2)
    (add-terms t1 (negation t2)))

;Derivative function
(define (der-terms t1)
   (if (empty-termlist? t1)
       (the-empty-termlist)
       (der-all t1)))
  (define (der-all t1)
    (if (empty-termlist? t1)
        (the-empty-termlist)
    (let ((x (first-term t1)))
      (adjoin-term
       (make-term (* (coeff x) (order x))
                  (- (order x) 1))
       (der-all (rest-terms t1))))))

;integration function
(define (int-terms t1)
   (if (empty-termlist? t1)
       (the-empty-termlist)
       (int-all t1)))
  (define (int-all t1)
    (if (empty-termlist? t1)
        (the-empty-termlist)
    (let ((x (first-term t1)))
      (adjoin-term
       (make-term (/ (coeff x) (order x))
                  (+ (order x) 1))
       (int-all (rest-terms t1))))))

;(trace variable)
;(trace add-poly)
;(trace make-poly)
;(trace same-variable?)
;(trace add-terms)
;(trace empty-termlist?)
;(trace first-term)
;(trace adjoin-term)
;(trace make-term)
;(trace mul-terms)
;(trace div-terms)
;(trace sub-terms)
;(trace der-terms)
;(mul-terms  (term-list (make-poly 'x (make-term 2 3) (make-term 3 2) (make-term 4 1))) (term-list (make-poly 'x (make-term 3 3) (make-term 2 2))))
;(der-terms (term-list (make-poly 'x(make-term 2 3) (make-term 1 2) (make-term 4 1))))

;(negation (term-list (make-poly 'x (make-term 3 3) (make-term 2 2))))
;(value (make-poly 'x(make-term 2 3) (make-term 1 2) (make-term 4 1)) 2)
;(int-terms (term-list (make-poly 'x(make-term 2 3) (make-term 1 2) (make-term 4 1))))
;(poly1)
