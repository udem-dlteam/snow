;;;============================================================================

;;; File: "rsa.scm", Time-stamp: <2007-04-05 00:53:04 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides procedures to encrypt, decrypt, sign and verify messages
;;; using the RSA public-key cryptosystem.

(package* rsa/v1.0.1
 (provide:

  (define* (make-rsa-key-pair size (show-trace _)))
  (define (public-rsa-key rsa-key-pair))
  (define (private-rsa-key rsa-key-pair))

  (define (rsa-key= rsa-key1 rsa-key2))

  (define (rsa-key->list rsa-key))
  (define (list->rsa-key lst))

  (define (PKCS1-pad u8vect final-len))
  (define (PKCS1-unpad u8vect))

  (define (rsa-encrypt-u8vector u8vect rsa-key final-len))
  (define (rsa-decrypt-u8vector u8vect rsa-key))

  (define* (make-salt (len _)))
  (define (PBKDF1 password salt iter-count len))
  (define (PBKDF2 password salt iter-count len)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Message encryption and decryption based on the RSA asymmetric cipher.")

 (keywords: crypto conv snow)

 (license: lgpl/v2.1)

 (require: homovector/v1)
 (require: fixnum/v1)
 (require: bignum/v1)
 (require: random/v1)
 (require: digest/v1)
 (require: extio/v1))

;;;============================================================================

;; Generation of RSA public and private key pairs.

(define-record* rsa-key
  ;; uid: rsa-key-973409ff-46e7-4643-8e26-f5a2e6314956
  size
  modulus ;; (expt 2 size) <= modulus < (expt 2 (+ size 1))
  exponent)

(define (random-prime start end show-trace)

  (define (product-of-primes n)
    (let loop ((n (- n 1)) (p (fixnum->bignum 2)) (i 3))
      (cond ((= n 0)
             p)
            ((bignum= (fixnum->bignum 1) (bignum-gcd (fixnum->bignum i) p))
             (loop (- n 1) (bignum* p (fixnum->bignum i)) (+ i 2)))
            (else
             (loop n p (+ i 2))))))

  (if show-trace
      (begin
        (display ".")
        (snow-force-output)))

  (let ((prod-small-primes (product-of-primes 300)))

    (define (likely-prime? n)
      (and (bignum= (fixnum->bignum 1)
                    (bignum-gcd n prod-small-primes))
           (bignum= (fixnum->bignum 1)
                    (bignum-expt-mod (fixnum->bignum 2)
                                     (bignum- n (fixnum->bignum 1))
                                     n))))

    (let loop ((i 1))
      (if show-trace
          (begin
            (if (= 0 (modulo i 10)) (newline))
            (display "+")
            (snow-force-output)))
      (let* ((x (bignum+ start (random-bignum (bignum- end start))))
             (n (if (bignum-odd? x) x (bignum+ x (fixnum->bignum 1)))))
        (if (or (bignum>= n end)
                (not (likely-prime? n)))
            (loop (+ i 1))
            (begin
              (if show-trace (newline))
              n))))))

(define (gcd-ext x y)
  (let loop ((x x)
             (y y)
             (u1 (fixnum->bignum 1))
             (u2 (fixnum->bignum 0))
             (v1 (fixnum->bignum 0))
             (v2 (fixnum->bignum 1)))
    (if (bignum-zero? y)
        (list x u1 v1)
        (let ((q (bignum-quotient x y))
              (r (bignum-remainder x y)))
          (loop y
                r
                u2
                (bignum- u1 (bignum* q u2))
                v2
                (bignum- v1 (bignum* q v2)))))))

(define (mod-inverse x b)
  (let* ((x1 (bignum-modulo x b))
         (g (gcd-ext x1 b)))
    (if (not (bignum= (car g) (fixnum->bignum 1)))
        (snow-error "internal error, numbers are not relatively prime" x b)
        (bignum-modulo (cadr g) b))))

(define* (make-rsa-key-pair size (show-trace #f))
  (let* ((size-p (quotient size 2))
         (start-p (bignum-expt (fixnum->bignum 2) (fixnum->bignum size-p)))
         (end-p (bignum* start-p (fixnum->bignum 2)))
         (p (random-prime start-p end-p show-trace))
         (start-n (bignum-expt (fixnum->bignum 2) (fixnum->bignum size)))
         (end-n (bignum* start-n (fixnum->bignum 2)))
         (start-q
          (bignum+ (bignum-quotient (bignum- start-n (fixnum->bignum 1)) p)
                   (fixnum->bignum 1)))
         (end-q
          (bignum-quotient end-n p)))
    (let loop ()
      (let ((q (random-prime start-q end-q show-trace)))
        (if (not (bignum= (bignum-gcd p q) (fixnum->bignum 1)))
            (loop)
            (let* ((n (bignum* p q))
                   (p-1 (bignum- p (fixnum->bignum 1)))
                   (q-1 (bignum- q (fixnum->bignum 1)))
                   (phi (bignum-quotient (bignum* p-1 q-1)
                                         (bignum-gcd p-1 q-1)))
                   (e (let loop ((e (fixnum->bignum 65537)))
                        (if (bignum= (fixnum->bignum 1) (bignum-gcd e phi))
                            e
                            (loop (bignum+ e (fixnum->bignum 2))))))
                   (d (mod-inverse e phi)))
              (cons (make-rsa-key size n e) ;; public and private keys
                    (make-rsa-key size n d))))))))

(define (public-rsa-key rsa-key-pair)
  (car rsa-key-pair))

(define (private-rsa-key rsa-key-pair)
  (cdr rsa-key-pair))

;;;----------------------------------------------------------------------------

;; Key comparison.

(define (rsa-key= rsa-key1 rsa-key2)
  (and (= (rsa-key-size rsa-key1)
          (rsa-key-size rsa-key2))
       (bignum= (rsa-key-modulus rsa-key1)
                (rsa-key-modulus rsa-key2))
       (bignum= (rsa-key-exponent rsa-key1)
                (rsa-key-exponent rsa-key2))))

;;;----------------------------------------------------------------------------

;; Conversion of RSA keys for serialization and deserialization.

(define (rsa-key->list rsa-key)
  (list (rsa-key-size rsa-key)
        (bignum->base64-string (rsa-key-modulus rsa-key))
        (bignum->base64-string (rsa-key-exponent rsa-key))))

(define (list->rsa-key lst)
  (if (not (and (list? lst) (= 3 (length lst))))
      (snow-error "improperly formatted RSA key")
      (let* ((size
              (car lst))
             (modulus-str
              (cadr lst))
             (exponent-str
              (caddr lst))
             (modulus
              (and (string? modulus-str)
                   (base64-string->bignum modulus-str)))
             (exponent
              (and (string? exponent-str)
                   (base64-string->bignum exponent-str))))
        (if (not (and (memv size '(512 1024 2048))
                      modulus
                      exponent))
            (snow-error "improperly formatted RSA key")
            (make-rsa-key size modulus exponent)))))

;;;----------------------------------------------------------------------------

;; Message padding and unpadding.

(define (PKCS1-pad u8vect final-len)

  (define (err)
    (snow-error "not enough space is available for proper padding"))

  (let* ((len (snow-u8vector-length u8vect))
         (n (- final-len (+ len 3))))
    (if (< n 8)
        (err)
        (let ((pad
               (let loop ((lst '(0)) (i 0))
                 (if (< i n)
                     (loop (cons (+ 1 (random-fixnum 255)) lst) (+ i 1))
                     (snow-list->u8vector (cons 0 (cons 2 lst)))))))
          (snow-u8vector-append pad u8vect)))))

(define (PKCS1-unpad u8vect)

  (define (err)
    (snow-error "improperly padded message"))

  (let ((len (snow-u8vector-length u8vect)))
    (let loop1 ((i 0))
      (if (>= i len)
          (err)
          (let ((x (snow-u8vector-ref u8vect i)))
            (cond ((= x 0)
                   (loop1 (+ i 1)))
                  ((not (= x 2))
                   (err))
                  (else
                   (let loop2 ((j (+ i 1)))
                     (if (>= j len)
                         (err)
                         (let ((x (snow-u8vector-ref u8vect j)))
                           (cond ((not (= x 0))
                                  (loop2 (+ j 1)))
                                 ((< (- j i) 8) ;; need at least 8 byte pad
                                  (err))
                                 (else
                                  (snow-subu8vector
                                   u8vect
                                   (+ j 1)
                                   len)))))))))))))

;;;----------------------------------------------------------------------------

;; Message encryption and decryption.

(define (rsa-crypt message rsa-key) ;; encryption and decryption
  (bignum-expt-mod
   message
   (rsa-key-exponent rsa-key)
   (rsa-key-modulus rsa-key)))

(define (rsa-encrypt-u8vector u8vect rsa-key final-len)
  (bignum->u8vector
   (rsa-crypt
    (u8vector->bignum (PKCS1-pad u8vect final-len))
    rsa-key)))

(define (rsa-decrypt-u8vector u8vect rsa-key)
  (PKCS1-unpad
   (bignum->u8vector
    (rsa-crypt
     (u8vector->bignum u8vect)
     rsa-key))))

;;;----------------------------------------------------------------------------

;; Implementation of a subset of RFC 2898 (PKCS #5: Password-Based
;; Cryptography Specification Version 2.0).

(define (get-u8vector-password password)
  (if (string? password)
      (snow-ISO-8859-1-string->u8vector password)
      password))

(define (u8vector-xor u8vect1 u8vect2)
  (let* ((len (snow-u8vector-length u8vect1))
         (result (snow-make-u8vector len)))
    (let loop ((i (- len 1)))
      (if (>= i 0)
          (begin
            (snow-u8vector-set!
             result
             i
             (snow-fxxor (snow-u8vector-ref u8vect1 i)
                         (snow-u8vector-ref u8vect2 i)))
            (loop (- i 1)))
          result))))

(define* (make-salt (len 8)) ;; default is 64 bit salt
  (make-random-u8vector len))

(define (PBKDF1 password salt iter-count len)
  (if (> len 20)
      (snow-error "derived key too long")
      (let ((algorithm (if (> len 16) 'sha-1 'md5))
            (password (get-u8vector-password password)))
        (let loop ((k 0)
                   (t (snow-u8vector-append password salt)))
          (if (< k iter-count)
              (loop (+ k 1)
                    (digest-u8vector t algorithm 'u8vector))
              (snow-subu8vector t 0 len))))))

(define (PBKDF2 password salt iter-count len)

  (define (u32-be i)
    (snow-u8vector
     (snow-fxand #xff (snow-fxarithmetic-shift-right i 24))
     (snow-fxand #xff (snow-fxarithmetic-shift-right i 16))
     (snow-fxand #xff (snow-fxarithmetic-shift-right i  8))
     (snow-fxand #xff i)))

  (define (PRF password salt)
    ;; TODO: should really be using HMAC-SHA-1 (see RFC 2898)
    (digest-u8vector
     (snow-u8vector-append password salt)
     'sha-1
     'u8vector))

  (define (F password salt iter-count i)
    (let ((x (PRF password (snow-u8vector-append salt (u32-be i)))))
      (let loop ((k 1) (u x) (t x))
        (if (< k iter-count)
            (let ((x (PRF password u)))
              (loop (+ k 1) x (u8vector-xor t x)))
            t))))

  (if (> len 65536) ;; arbitrary limit, more than enough for most purposes
      (snow-error "derived key too long")
      (let ((password (get-u8vector-password password)))
        (let loop ((i 1)
                   (n len)
                   (lst '()))
          (if (> n 0)
              (let* ((t (F password salt iter-count i))
                     (x (snow-u8vector-length t)))
                (loop (+ i 1)
                      (- n x)
                      (cons (if (> n x)
                                t
                                (snow-subu8vector t 0 n))
                            lst)))
              (snow-apply-u8vector-append (reverse lst)))))))

;;;============================================================================
