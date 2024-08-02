;;;============================================================================

;;; File: "fixnum.scm", Time-stamp: <2007-09-01 17:11:16 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides operations on fixnums.

(package* fixnum/v1.0.3
 (provide:

  (cond-expand

   (bigloo

    (define-macro (snow-fxand x y) `(bit-and ,x ,y))
    (define-macro (snow-fxior x y) `(bit-or  ,x ,y))
    (define-macro (snow-fxxor x y) `(bit-xor ,x ,y))
    (define-macro (snow-fxnot x)   `(bit-not ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(bit-lsh ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(bit-rsh ,x ,n)))

   (chez

    (define-macro (snow-fxand x y) `(fxlogand ,x ,y))
    (define-macro (snow-fxior x y) `(fxlogior ,x ,y))
    (define-macro (snow-fxxor x y) `(fxlogxor ,x ,y))
    (define-macro (snow-fxnot x)   `(fxlognot ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(fxsll ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(fxsra ,x ,n)))

   (chicken

    (define-macro (snow-fxand x y) `(bitwise-and ,x ,y))
    (define-macro (snow-fxior x y) `(bitwise-ior ,x ,y))
    (define-macro (snow-fxxor x y) `(bitwise-xor ,x ,y))
    (define-macro (snow-fxnot x)   `(bitwise-not ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(arithmetic-shift ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(arithmetic-shift ,x (- ,n))))

   (gambit

    (define-macro (snow-fxand x y) `(fxand ,x ,y))
    (define-macro (snow-fxior x y) `(fxior ,x ,y))
    (define-macro (snow-fxxor x y) `(fxxor ,x ,y))
    (define-macro (snow-fxnot x)   `(fxnot ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(fxarithmetic-shift-left ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(fxarithmetic-shift-right ,x ,n)))

   (gauche

    (define-macro (snow-fxand x y) `(logand ,x ,y))
    (define-macro (snow-fxior x y) `(logior ,x ,y))
    (define-macro (snow-fxxor x y) `(logxor ,x ,y))
    (define-macro (snow-fxnot x)   `(lognot ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(ash ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(ash ,x (- ,n))))

   (guile

    (define-macro (snow-fxand x y) `(logand ,x ,y))
    (define-macro (snow-fxior x y) `(logior ,x ,y))
    (define-macro (snow-fxxor x y) `(logxor ,x ,y))
    (define-macro (snow-fxnot x)   `(lognot ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(ash ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(ash ,x (- ,n))))

   (gauche

    (define-macro (snow-fxand x y) `(logand ,x ,y))
    (define-macro (snow-fxior x y) `(logior ,x ,y))
    (define-macro (snow-fxxor x y) `(logxor ,x ,y))
    (define-macro (snow-fxnot x)   `(lognot ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(ash ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(ash ,x (- ,n))))

   (kawa

    (define-macro (snow-fxand x y) `(logand ,x ,y))
    (define-macro (snow-fxior x y) `(logior ,x ,y))
    (define-macro (snow-fxxor x y) `(logxor ,x ,y))
    (define-macro (snow-fxnot x)   `(lognot ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(ash ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(ash ,x (- ,n))))

   (larceny

    (define-macro (snow-fxand x y) `(fxlogand ,x ,y))
    (define-macro (snow-fxior x y) `(fxlogior ,x ,y))
    (define-macro (snow-fxxor x y) `(fxlogxor ,x ,y))
    (define-macro (snow-fxnot x)   `(fxlognot ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(fxlsh ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(fxrsha ,x ,n)))

   (mit

    (define-macro (snow-fxand x y) `(fix:and ,x ,y))
    (define-macro (snow-fxior x y) `(fix:or ,x ,y))
    (define-macro (snow-fxxor x y) `(fix:xor ,x ,y))
    (define-macro (snow-fxnot x)   `(fix:not ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(fix:lsh ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(fix:lsh ,x (- ,n))))

   (mzscheme

    (define-macro (snow-fxand x y) `(bitwise-and ,x ,y))
    (define-macro (snow-fxior x y) `(bitwise-ior ,x ,y))
    (define-macro (snow-fxxor x y) `(bitwise-xor ,x ,y))
    (define-macro (snow-fxnot x)   `(bitwise-not ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(arithmetic-shift ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(arithmetic-shift ,x (- ,n))))

   (scheme48

    (define-macro (snow-fxand x y) `(bitwise-and ,x ,y))
    (define-macro (snow-fxior x y) `(bitwise-ior ,x ,y))
    (define-macro (snow-fxxor x y) `(bitwise-xor ,x ,y))
    (define-macro (snow-fxnot x)   `(bitwise-not ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(arithmetic-shift ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(arithmetic-shift ,x (- ,n))))

   (scm

    (define-macro (snow-fxand x y) `(logand ,x ,y))
    (define-macro (snow-fxior x y) `(logior ,x ,y))
    (define-macro (snow-fxxor x y) `(logxor ,x ,y))
    (define-macro (snow-fxnot x)   `(lognot ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(ash ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(ash ,x (- ,n))))

   (scsh

    (define-macro (snow-fxand x y) `(bitwise-and ,x ,y))
    (define-macro (snow-fxior x y) `(bitwise-ior ,x ,y))
    (define-macro (snow-fxxor x y) `(bitwise-xor ,x ,y))
    (define-macro (snow-fxnot x)   `(bitwise-not ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(arithmetic-shift ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(arithmetic-shift ,x (- ,n))))

   (sisc

    (define-macro (snow-fxand x y) `(logand ,x ,y))
    (define-macro (snow-fxior x y) `(logor ,x ,y))
    (define-macro (snow-fxxor x y) `(logxor ,x ,y))
    (define-macro (snow-fxnot x)   `(lognot ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(ashl ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(ashr ,x ,n)))

   (stklos

    (define-macro (snow-fxand x y) `(bit-and ,x ,y))
    (define-macro (snow-fxior x y) `(bit-or ,x ,y))
    (define-macro (snow-fxxor x y) `(bit-xor ,x ,y))
    (define-macro (snow-fxnot x)   `(bit-not ,x))

    (define-macro (snow-fxarithmetic-shift-left x n)
      `(bit-shift ,x ,n))

    (define-macro (snow-fxarithmetic-shift-right x n)
      `(bit-shift ,x (- ,n))))))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Operations on small integers.")

 (keywords: math snow)

 (license: lgpl/v2.1))

;;;============================================================================

;;; System dependencies.

(cond-expand
 (sisc
  (import logicops))
 (else))

;;;----------------------------------------------------------------------------

;; Self tests.

(test*
 (expect* (=  8 (snow-fxand 12 10)))
 (expect* (= 14 (snow-fxior 12 10)))
 (expect* (=  6 (snow-fxxor 12 10)))
 (expect* (=  5 (snow-fxnot -6)))
 (expect* (= 12 (snow-fxarithmetic-shift-left 3 2)))
 (expect* (=  3 (snow-fxarithmetic-shift-right 13 2))))

;;;============================================================================
