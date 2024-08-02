;;;=============================================================================

;;; File: "random.scm", Time-stamp: <2007-04-05 00:52:42 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;=============================================================================

;;; Provides procedures to get random bits of high quality.

(package* random/v1.0.1
 (provide:

  (define (make-random-u8vector len))
  (define (random-bignum range))
  (define (random-fixnum range)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "High-quality random number generation.")

 (keywords: snow)

 (license: lgpl/v2.1)

 (require: homovector/v1)
 (require: binio/v1)
 (require: bignum/v1))

;;;=============================================================================

(cond-expand

 ((and srfi-27
       (not gauche) ;; work around a bug in Gauche
       (not stklos)) ;; work around a bug in STklos

  (define (make-random-u8vector len)
    (let* ((rs (make-random-source))
           (mi (random-source-make-integers rs))
           (u8vect (snow-make-u8vector len)))
      (random-source-randomize! rs)
      (let loop ((i 0))
        (if (< i len)
            (begin
              (snow-u8vector-set! u8vect i (mi 256))
              (loop (+ i 1)))))
      u8vect)))

 (else

  (define-macro (random-bits-file) "/dev/random")

  (define (make-random-u8vector len)
    (let* ((in (binio-open-input-file (random-bits-file)))
           (u8vect (snow-make-u8vector len))
           (n (binio-read-subu8vector u8vect 0 len in)))
      (binio-close-input-port in)
      (if (= n len)
          u8vect
          (snow-error "random bits file ended prematurely"))))))

;;;----------------------------------------------------------------------------

(define (random-bignum range)
  (let* ((range-bits (bignum-integer-length range))
         (len (quotient (+ range-bits 20) 8))
         (n (bignum-expt (fixnum->bignum 256) (fixnum->bignum len)))
         (divisor (bignum-quotient n range))
         (limit (bignum* divisor range)))
    (let loop ()
      (let* ((u8vect (make-random-u8vector len))
             (x (fixnum-list->bignum (snow-u8vector->list u8vect) 255)))
        (if (bignum>= x limit)
            (loop)
            (bignum-quotient x divisor))))))

(define (random-fixnum range)
  (bignum->fixnum (random-bignum (fixnum->bignum range))))

;;;=============================================================================
