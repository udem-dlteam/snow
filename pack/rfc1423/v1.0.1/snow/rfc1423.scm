;;;============================================================================

;;; File: "rfc1423.scm", Time-stamp: <2007-04-05 00:52:53 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides procedures to pad and unpad messages.

(package* rfc1423/v1.0.1
 (provide:

  (define* (RFC1423-pad u8vect (multiple _)))
  (define* (RFC1423-unpad u8vect (multiple _))))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Message padding and unpadding.")

 (keywords: crypto conv snow)

 (license: lgpl/v2.1)

 (require: homovector/v1))

;;;============================================================================

;; Generalized message padding/unpadding from RFC 1423 (Privacy
;; Enhancement for Internet Electronic Mail: Part III: Algorithms,
;; Modes, and Identifiers).

(define* (RFC1423-pad u8vect (multiple 8))
  (if (or (<= multiple 0) (>= multiple 256))
      (snow-error "illegal padding multiple")
      (let* ((len (snow-u8vector-length u8vect))
             (n (+ multiple (remainder (- len) multiple))))
        (snow-u8vector-append u8vect (snow-make-u8vector n n)))))

(define* (RFC1423-unpad u8vect (multiple 8))
  (if (or (<= multiple 0) (>= multiple 256))
      (snow-error "illegal padding multiple")
      (let ((len (snow-u8vector-length u8vect)))
        (if (or (< len multiple)
                (not (= 0 (modulo len multiple))))
            (snow-error "improperly padded u8vector")
            (let ((n (snow-u8vector-ref u8vect (- len 1))))
              (if (or (= n 0) (> n multiple))
                  (snow-error "improperly padded u8vector")
                  (let loop ((i n))
                    (if (>= i 2)
                        (if (not (= n (snow-u8vector-ref u8vect (- len i))))
                            (snow-error "improperly padded u8vector")
                            (loop (- i 1)))
                        (snow-subu8vector u8vect 0 (- len n))))))))))

;;;============================================================================
