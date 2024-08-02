;;;============================================================================

;;; File: "homovector.scm", Time-stamp: <2007-04-05 00:51:55 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Provides procedures to operate on homogeneous vectors, including
;; a subset of SRFI 4.

(package* homovector/v1.0.1
 (provide:

  (cond-expand

   ((or bigloo
        chicken
        gambit)

    (define-macro (snow-make-u8vector n . fill)
      `(make-u8vector ,n ,@fill))

    (define-macro (snow-u8vector . lst)
      `(u8vector ,@lst))

    (define-macro (snow-u8vector? obj)
      `(u8vector? ,obj))

    (define-macro (snow-u8vector-length u8vect)
      `(u8vector-length ,u8vect))

    (define-macro (snow-u8vector-ref u8vect i)
      `(u8vector-ref ,u8vect ,i))

    (define-macro (snow-u8vector-set! u8vect i x)
      `(u8vector-set! ,u8vect ,i ,x))

    (define-macro (snow-u8vector->list u8vect)
      `(u8vector->list ,u8vect))

    (define-macro (snow-list->u8vector lst)
      `(list->u8vector ,lst))

    (define-macro (snow-make-u16vector n . fill)
      `(make-u16vector ,n ,@fill))

    (define-macro (snow-u16vector . lst)
      `(u16vector ,@lst))

    (define-macro (snow-u16vector? obj)
      `(u16vector? ,obj))

    (define-macro (snow-u16vector-length u16vect)
      `(u16vector-length ,u16vect))

    (define-macro (snow-u16vector-ref u16vect i)
      `(u16vector-ref ,u16vect ,i))

    (define-macro (snow-u16vector-set! u16vect i x)
      `(u16vector-set! ,u16vect ,i ,x))

    (define-macro (snow-u16vector->list u16vect)
      `(u16vector->list ,u16vect))

    (define-macro (snow-list->u16vector lst)
      `(list->u16vector ,lst)))

   (else

    (define-macro (snow-make-u8vector n . fill)
      `(snow-make-u8v ,n ,@fill))

    (define-macro (snow-u8vector . lst)
      `(snow-u8v ,@lst))

    (define-macro (snow-u8vector? obj)
      `(snow-u8v? ,obj))

    (define-macro (snow-u8vector-length u8vect)
      `(snow-u8v-length ,u8vect))

    (define-macro (snow-u8vector-ref u8vect i)
      `(snow-u8v-ref ,u8vect ,i))

    (define-macro (snow-u8vector-set! u8vect i x)
      `(snow-u8v-set! ,u8vect ,i ,x))

    (define-macro (snow-u8vector->list u8vect)
      `(snow-u8v->list ,u8vect))

    (define-macro (snow-list->u8vector lst)
      `(snow-list->u8v ,lst))

    (define-macro (snow-make-u16vector n . fill)
      `(snow-make-u16v ,n ,@fill))

    (define-macro (snow-u16vector . lst)
      `(snow-u16v ,@lst))

    (define-macro (snow-u16vector? obj)
      `(snow-u16v? ,obj))

    (define-macro (snow-u16vector-length u16vect)
      `(snow-u16v-length ,u16vect))

    (define-macro (snow-u16vector-ref u16vect i)
      `(snow-u16v-ref ,u16vect ,i))

    (define-macro (snow-u16vector-set! u16vect i x)
      `(snow-u16v-set! ,u16vect ,i ,x))

    (define-macro (snow-u16vector->list u16vect)
      `(snow-u16v->list ,u16vect))

    (define-macro (snow-list->u16vector lst)
      `(snow-list->u16v ,lst))

    ;; The following procedures are only exported for use
    ;; inside the above macros.  They are not part of the API.

    (define* (snow-make-u8v n (fill _)))
    (define (snow-u8v . lst))
    (define (snow-u8v? obj))
    (define (snow-u8v-length u8vect))
    (define (snow-u8v-ref u8vect i))
    (define (snow-u8v-set! u8vect i x))
    (define (snow-u8v->list u8vect))
    (define (snow-list->u8v lst))

    (define* (snow-make-u16v n (fill _)))
    (define (snow-u16v . lst))
    (define (snow-u16v? obj))
    (define (snow-u16v-length u16vect))
    (define (snow-u16v-ref u16vect i))
    (define (snow-u16v-set! u16vect i x))
    (define (snow-u16v->list u16vect))
    (define (snow-list->u16v lst))))

  (define (snow-subu8vector-move! src src-start src-end dst dst-start))
  (define (snow-subu8vector u8vect start end))

  (define (snow-subu16vector-move! src src-start src-end dst dst-start))
  (define (snow-subu16vector u16vect start end))

  (define (snow-ISO-8859-1-substring->u8vector str start end))
  (define (snow-ISO-8859-1-string->u8vector str))
  (define (snow-subu8vector->ISO-8859-1-string u8vect start end))
  (define (snow-u8vector->ISO-8859-1-string u8vect))

  (define (snow-hex-substring->u8vector str start end))
  (define (snow-hex-string->u8vector str))
  (define (snow-subu8vector->hex-string u8vect start end))
  (define (snow-u8vector->hex-string u8vect))

  (define (snow-apply-u8vector-append lst))
  (define (snow-u8vector-append . lst)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Homogeneous numeric vectors.")

 (keywords: math snow)

 (license: lgpl/v2.1)

 (require: fixnum/v1))

;;;============================================================================

;; System dependencies.

(cond-expand

 ((or bigloo
      gambit)

  #f)

 (chicken

  (require-extension srfi-4))

 (else

  ;; u16vectors need to be implemented with u8vectors, which are in
  ;; turn either implemented with Scheme48/Scsh byte-vectors or
  ;; strings.

  (cond-expand

   ((or scheme48
        scsh)

    (define* (snow-make-u8v n (fill 0))
      (make-byte-vector n fill))

    (define snow-u8v byte-vector)
    (define snow-u8v? byte-vector?)
    (define snow-u8v-length byte-vector-length)
    (define snow-u8v-ref byte-vector-ref)
    (define snow-u8v-set! byte-vector-set!))

   (else

    (define-record* snow-u8vect
      ;; uid: snow-u8vect-70c9ad92-1af6-4510-9505-89819df4ea9d
      str)

    (define* (snow-make-u8v n (fill 0))
      (make-snow-u8vect (make-string n (integer->char fill))))

    (define (snow-u8v? obj)
      (snow-u8vect? obj))

    (define (snow-u8v . lst)
      (snow-list->u8v lst))

    (define (snow-u8v-length u8vect)
      (string-length (snow-u8vect-str u8vect)))

    (define (snow-u8v-ref u8vect i)
      (char->integer (string-ref (snow-u8vect-str u8vect) i)))

    (define (snow-u8v-set! u8vect i x)
      (string-set! (snow-u8vect-str u8vect) i (integer->char x)))))

  (define (snow-u8v->list u8vect)
    (let loop ((lst '()) (i (- (snow-u8v-length u8vect) 1)))
      (if (< i 0)
          lst
          (loop (cons (snow-u8v-ref u8vect i) lst)
                (- i 1)))))

  (define (snow-list->u8v lst)
    (let loop1 ((x lst) (n 0))
      (if (pair? x)
          (loop1 (cdr x) (+ n 1))
          (let ((u8vect (snow-make-u8v n)))
            (let loop2 ((x lst) (i 0))
              (if (pair? x)
                  (let ((elem (car x)))
                    (snow-u8v-set! u8vect i elem)
                    (loop2 (cdr x) (+ i 1)))
                  u8vect))))))

  (define-record* snow-u16vect
    ;; uid: snow-u16vect-3df9ca2a-9810-4c14-b2db-3969557a875a
    u8vect)

  (define* (snow-make-u16v n (fill 0))
    (let* ((fill-lo (snow-fxand #xff fill))
           (fill-hi (snow-fxand #xff (snow-fxarithmetic-shift-right fill 8)))
           (n*2 (* n 2))
           (u8vect (snow-make-u8v n*2 fill-lo)))
      (if (not (= fill-hi fill-lo))
          (let loop ((i 1))
            (if (< i n*2)
                (begin
                  (snow-u8v-set! u8vect i fill-hi)
                  (loop (+ i 2)))
                u8vect)))
      (make-snow-u16vect u8vect)))

  (define (snow-u16v? obj)
    (snow-u16vect? obj))

  (define (snow-u16v . lst)
    (snow-list->u16v lst))

  (define (snow-u16v-length u16vect)
    (quotient (snow-u8v-length (snow-u16vect-u8vect u16vect)) 2))

  (define (snow-u16v-ref u16vect i)
    (let* ((u8vect (snow-u16vect-u8vect u16vect))
           (i*2 (* i 2))
           (lo (snow-u8v-ref u8vect i*2))
           (hi (snow-u8v-ref u8vect (+ 1 i*2))))
      (+ (snow-fxarithmetic-shift-left hi 8) lo)))

  (define (snow-u16v-set! u16vect i x)
    (let* ((u8vect (snow-u16vect-u8vect u16vect))
           (i*2 (* i 2))
           (lo (snow-fxand #xff x))
           (hi (snow-fxand #xff (snow-fxarithmetic-shift-right x 8))))
      (snow-u8v-set! u8vect i*2 lo)
      (snow-u8v-set! u8vect (+ 1 i*2) hi)))

  (define (snow-u16v->list u16vect)
    (let loop ((lst '()) (i (- (snow-u16v-length u16vect) 1)))
      (if (< i 0)
          lst
          (loop (cons (snow-u16v-ref u16vect i) lst)
                (- i 1)))))

  (define (snow-list->u16v lst)
    (let loop1 ((x lst) (n 0))
      (if (pair? x)
          (loop1 (cdr x) (+ n 1))
          (let ((u16vect (snow-make-u16v n)))
            (let loop2 ((x lst) (i 0))
              (if (pair? x)
                  (let ((elem (car x)))
                    (snow-u16v-set! u16vect i elem)
                    (loop2 (cdr x) (+ i 1)))
                  u16vect))))))))

;;;----------------------------------------------------------------------------

(define (snow-subu8vector-move! src src-start src-end dst dst-start)

  ;; Copy direction must be selected in case src and dst are the same vector.

  (if (< src-start dst-start)
      (let loop1 ((i (- src-end 1))
                  (j (- (+ dst-start (- src-end src-start)) 1)))
        (if (< i src-start)
            dst
            (begin
              (snow-u8vector-set! dst j (snow-u8vector-ref src i))
              (loop1 (- i 1)
                     (- j 1)))))
      (let loop2 ((i src-start)
                  (j dst-start))
        (if (< i src-end)
            (begin
              (snow-u8vector-set! dst j (snow-u8vector-ref src i))
              (loop2 (+ i 1)
                     (+ j 1)))
            dst))))

(define (snow-subu8vector u8vect start end)
  (snow-subu8vector-move!
   u8vect
   start
   end
   (snow-make-u8vector (max (- end start) 0))
   0))

(define (snow-subu16vector-move! src src-start src-end dst dst-start)

  ;; Copy direction must be selected in case src and dst are the same vector.

  (if (< src-start dst-start)
      (let loop1 ((i (- src-end 1))
                  (j (- (+ dst-start (- src-end src-start)) 1)))
        (if (< i src-start)
            dst
            (begin
              (snow-u16vector-set! dst j (snow-u16vector-ref src i))
              (loop1 (- i 1)
                     (- j 1)))))
      (let loop2 ((i src-start)
                  (j dst-start))
        (if (< i src-end)
            (begin
              (snow-u16vector-set! dst j (snow-u16vector-ref src i))
              (loop2 (+ i 1)
                     (+ j 1)))
            dst))))

(define (snow-subu16vector u16vect start end)
  (snow-subu16vector-move!
   u16vect
   start
   end
   (snow-make-u16vector (max (- end start) 0))
   0))

(define (snow-ISO-8859-1-substring->u8vector str start end)
  (let* ((len (- end start))
         (u8vect (snow-make-u8vector len)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (snow-u8vector-set!
             u8vect
             i
             (char->integer (string-ref str (+ start i))))
            (loop (+ i 1)))
          u8vect))))

(define (snow-ISO-8859-1-string->u8vector str)
  (snow-ISO-8859-1-substring->u8vector
   str
   0
   (string-length str)))

(define (snow-subu8vector->ISO-8859-1-string u8vect start end)
  (let* ((len (- end start))
         (str (make-string len)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (string-set!
             str
             i
             (integer->char (snow-u8vector-ref u8vect (+ start i))))
            (loop (+ i 1)))
          str))))

(define (snow-u8vector->ISO-8859-1-string u8vect)
  (snow-subu8vector->ISO-8859-1-string
   u8vect
   0
   (snow-u8vector-length u8vect)))

(define (snow-hex-substring->u8vector str start end)

    (define (char->digit c)
      (cond ((and (char>=? c #\0) (char<=? c #\9))
             (- (char->integer c) (char->integer #\0)))
            ((and (char>=? c #\a) (char<=? c #\f))
             (+ 10 (- (char->integer c) (char->integer #\a))))
            ((and (char>=? c #\A) (char<=? c #\F))
             (+ 10 (- (char->integer c) (char->integer #\A))))
            (else
             #f)))

  (let ((n (- end start)))
    (if (odd? n)
        (snow-error "string length must be even")
        (let* ((len (quotient n 2))
               (u8vect (snow-make-u8vector len)))
          (let loop ((i 0) (j (- len 1)))
            (if (>= j 0)
                (let ((hi4 (char->digit (string-ref str i)))
                      (lo4 (char->digit (string-ref str (+ i 1)))))
                  (if (or (not hi4)
                          (not lo4))
                      (snow-error "string must contain hex digits only")
                      (begin
                        (snow-u8vector-set!
                         u8vect
                         j
                         (+ (* 16 hi4) lo4))
                        (loop (+ i 2) (- j 1)))))
                u8vect))))))

(define (snow-hex-string->u8vector str)
  (snow-hex-substring->u8vector
   str
   0
   (string-length str)))

(define (snow-subu8vector->hex-string u8vect start end)

  (define (digit->char d)
    (string-ref "0123456789abcdef" d))

  (let* ((len (- end start))
         (n (* len 2))
         (str (make-string n)))
    (let loop ((i 0) (j (- len 1)))
      (if (>= j 0)
          (let ((x (snow-u8vector-ref u8vect j)))
            (string-set! str i (digit->char (quotient x 16)))
            (string-set! str (+ i 1) (digit->char (modulo x 16)))
            (loop (+ i 2) (- j 1)))
          str))))

(define (snow-u8vector->hex-string u8vect)
  (snow-subu8vector->hex-string
   u8vect
   0
   (snow-u8vector-length u8vect)))

(define (snow-apply-u8vector-append lst)

  (define (append-rest-at i lst)
    (if (pair? lst)
        (let* ((src (car lst))
               (len (snow-u8vector-length src))
               (dst (append-rest-at (+ i len) (cdr lst))))
          (snow-subu8vector-move! src 0 len dst i)
          dst)
        (snow-make-u8vector i)))

  (append-rest-at 0 lst))

(define (snow-u8vector-append . lst)
  (snow-apply-u8vector-append lst))

;;;----------------------------------------------------------------------------

;; Self tests.

(test*

 (expect* (snow-u8vector? (snow-make-u8vector 0)))

 (expect* (snow-u8vector? (snow-u8vector 11 22 33)))

 (expect* (not (snow-u8vector? "hello")))

 (expect* (equal? '(11 22) (snow-u8vector->list (snow-u8vector 11 22))))

 (expect* (= 5 (snow-u8vector-length (snow-make-u8vector 5 99))))

 (expect* (= 3 (snow-u8vector-length (snow-u8vector 11 22 33))))

 (expect* (= 22 (snow-u8vector-ref (snow-u8vector 11 22 33) 1)))

 (expect* (equal? '(11 99 33)
                  (let ((v (snow-u8vector 11 22 33)))
                    (snow-u8vector-set! v 1 99)
                    (snow-u8vector->list v))))

 (expect* (equal? '(11 22 33)
                  (snow-u8vector->list (snow-list->u8vector '(11 22 33)))))

 (expect* (snow-u16vector? (snow-make-u16vector 0)))

 (expect* (snow-u16vector? (snow-u16vector 11 22 33)))

 (expect* (not (snow-u16vector? "hello")))

 (expect* (equal? '(11 22) (snow-u16vector->list (snow-u16vector 11 22))))

 (expect* (= 5 (snow-u16vector-length (snow-make-u16vector 5 99))))

 (expect* (= 3 (snow-u16vector-length (snow-u16vector 11 22 33))))

 (expect* (= 22 (snow-u16vector-ref (snow-u16vector 11 22 33) 1)))

 (expect* (equal? '(11 99 33)
                  (let ((v (snow-u16vector 11 22 33)))
                    (snow-u16vector-set! v 1 99)
                    (snow-u16vector->list v))))

 (expect* (equal? '(11 22 33)
                  (snow-u16vector->list (snow-list->u16vector '(11 22 33))))))

;;;============================================================================
