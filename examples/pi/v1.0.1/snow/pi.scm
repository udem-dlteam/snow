;;;============================================================================

;;; File: "pi.scm", Time-stamp: <2007-04-04 17:33:35 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Computes digits of pi.

(package* pi/v1.0.1
 (provide:

  (define (pi-digits n)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Computes the digits of pi.")

 (keywords: example snow)

 (license: lgpl/v2.1)

 (require: bignum/v1))

;;;============================================================================

;;; Compute pi using the 'brent-salamin' method.

(define (width x)
  (let loop ((i 0) (n (fixnum->bignum 1)))
    (if (bignum< x n)
        i
        (loop (+ i 1)
              (bignum* n (fixnum->bignum 2))))))

(define (root x y)
  (let loop ((g (bignum-expt
                 (fixnum->bignum 2)
                 (fixnum->bignum (quotient (+ (width x) (- y 1)) y)))))
    (let ((a (bignum-expt g (fixnum->bignum (- y 1)))))
      (let ((b (bignum* a (fixnum->bignum y))))
        (let ((c (bignum* a (fixnum->bignum (- y 1)))))
          (let ((d (bignum-quotient (bignum+ x (bignum* g c)) b)))
            (if (bignum< d g)
                (loop d)
                g)))))))

(define (square-root x)
  (root x 2))

(define (square x)
  (bignum* x x))

(define (pi-brent-salamin nb-digits)
  (let ((one (bignum-expt (fixnum->bignum 10)
                          (fixnum->bignum nb-digits))))
    (let loop ((a one)
               (b (square-root (bignum-quotient (bignum* one one)
                                                (fixnum->bignum 2))))
               (t (bignum-quotient one (fixnum->bignum 4)))
               (x (fixnum->bignum 1)))
      (if (bignum= a b)
          (bignum-quotient (square (bignum+ a b))
                           (bignum* (fixnum->bignum 4) t))
          (let ((new-a (bignum-quotient (bignum+ a b)
                                        (fixnum->bignum 2))))
            (loop new-a
                  (square-root (bignum* a b))
                  (bignum-
                   t
                   (bignum-quotient (bignum* x (square (bignum- new-a a)))
                                    one))
                  (bignum* (fixnum->bignum 2) x)))))))

(define (pi-digits n)
  (bignum->string (pi-brent-salamin n)))

;;;============================================================================
