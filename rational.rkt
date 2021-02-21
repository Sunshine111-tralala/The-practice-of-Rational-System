#lang typed/racket

(define-type Non-Zero-Integer (Refine [num : Integer] (! num Zero)))


(define-type Rational (U Zero-Rational Non-Zero-Rational))
(define-type Zero-Rational (U Zero Zero-Rational-Pair))
(define-type Non-Zero-Rational (U Non-Zero-Integer Non-Zero-Rational-Pair))

(define-predicate rational? Rational)
(define-predicate zero-rational? Zero-Rational)
(define-predicate non-zero-rational? Non-Zero-Rational)


(define-type Zero-Rational-Pair (Pair Zero-Rational Non-Zero-Rational))
(define-type Non-Zero-Rational-Pair (Pair Non-Zero-Rational Non-Zero-Rational))

(define-predicate zero-rational-pair? Zero-Rational-Pair)
(define-predicate non-zero-rational-pair? Non-Zero-Rational-Pair)


(: make-rational (case-> [-> Zero-Rational Zero-Rational]
                         [-> Non-Zero-Rational Non-Zero-Rational]
                         [-> Rational Rational]
                         
                         [-> Non-Zero-Rational Non-Zero-Rational Non-Zero-Rational]
                         [-> Zero-Rational Non-Zero-Rational Zero-Rational]
                         [-> Rational Non-Zero-Rational Rational]))
(define make-rational
  (case-lambda
    [(r) r]
    [(r1 r2)
     (cons r1 r2)]))


(: numer (case-> [-> Zero-Rational Zero-Rational]
                 [-> Non-Zero-Rational Non-Zero-Rational]
                 [-> Rational Rational]))
(define numer
  (λ (r)
    (if (pair? r) (car r) r)))

(: denom [-> Rational Non-Zero-Rational])
(define denom
  (λ (r)
    (if (pair? r) (cdr r) 1)))


(: +rational [-> Rational Rational Rational])
(define (+rational x y)
  (let ([x-numer : Rational (numer x)]
        [y-numer : Rational (numer y)]
        [x-denom : Non-Zero-Rational (denom x)]
        [y-denom : Non-Zero-Rational (denom y)])
    (if (and (integer? x-numer)
             (integer? y-numer)
             (integer? x-denom)
             (integer? y-denom))
        (make-rational (+ (* x-numer y-denom)
                          (* y-numer x-denom))
                       (cast (* x-denom y-denom) Non-Zero-Rational))
        (make-rational (+rational (*rational x-numer y-denom)
                                  (*rational y-numer x-denom))
                       (cast (*rational x-denom y-denom) Non-Zero-Rational)))))

(: *rational [-> Rational Rational Rational])
(define (*rational x y)
  (let ([x-numer : Rational (numer x)]
        [y-numer : Rational (numer y)]
        [x-denom : Non-Zero-Rational (denom x)]
        [y-denom : Non-Zero-Rational (denom y)])
    (if (and (integer? x-numer)
             (integer? y-numer)
             (integer? x-denom)
             (integer? y-denom))
        (make-rational (* x-numer y-numer)
                       (cast (* x-denom y-denom) Non-Zero-Rational))
        (make-rational (*rational x-numer y-numer)
                       (cast (*rational x-denom y-denom) Non-Zero-Rational)))))
