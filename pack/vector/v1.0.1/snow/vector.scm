;;;============================================================================

;;; File: "vector.scm", Time-stamp: <2007-04-05 19:01:40 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Provides procedures to operate on vectors.

(package* vector/v1.0.1
 (provide:

  (define (snow-subvector-move! src src-start src-end dst dst-start))
  (define (snow-subvector vect start end))
  (define (snow-apply-vector-append lst))
  (define (snow-vector-append . lst)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Operations on vectors.")

 (keywords: snow)

 (license: lgpl/v2.1))

;;;============================================================================

(define (snow-subvector-move! src src-start src-end dst dst-start)

  ;; Copy direction must be selected in case src and dst are the same vector.

  (if (< src-start dst-start)
      (let loop1 ((i (- src-end 1))
                  (j (- (+ dst-start (- src-end src-start)) 1)))
        (if (< i src-start)
            dst
            (begin
              (vector-set! dst j (vector-ref src i))
              (loop1 (- i 1)
                     (- j 1)))))
      (let loop2 ((i src-start)
                  (j dst-start))
        (if (< i src-end)
            (begin
              (vector-set! dst j (vector-ref src i))
              (loop2 (+ i 1)
                     (+ j 1)))
            dst))))

(define (snow-subvector u8vect start end)
  (snow-subvector-move!
   u8vect
   start
   end
   (make-vector (max (- end start) 0))
   0))

(define (snow-apply-vector-append lst)

  (define (append-rest-at i lst)
    (if (pair? lst)
        (let* ((src (car lst))
               (len (vector-length src))
               (dst (append-rest-at (+ i len) (cdr lst))))
          (snow-subvector-move! src 0 len dst i)
          dst)
        (make-vector i)))

  (append-rest-at 0 lst))

(define (snow-vector-append . lst)
  (snow-apply-vector-append lst))
                                     
;;;============================================================================
