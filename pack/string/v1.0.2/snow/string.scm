;;;============================================================================

;;; File: "string.scm", Time-stamp: <2007-09-01 23:24:17 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Provides procedures to operate on strings.

(package* string/v1.0.2
 (provide:

  (define (snow-call-with-input-string str proc))
  (define (snow-call-with-output-string proc))
  (define (snow-string->object str))
  (define (snow-object->string obj))
  (define (snow-string->object-list str))
  (define (snow-object-list->string lst))

  (define (snow-substring-move! src src-start src-end dst dst-start))
  (define (snow-apply-string-append lst))
  (define (snow-remove-string-prefix str prefix))
  (define (snow-remove-string-suffix str suffix))
  (define (snow-string-index-from str char i))
  (define (snow-string-index str char))
  (define (snow-string-split str char)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Operations on strings.")

 (keywords: snow)

 (license: lgpl/v2.1))

;;;============================================================================

;;; System dependencies.

(cond-expand

 ((or chicken
      gauche
      guile
      scm
      stklos)

  (define (snow-call-with-input-string str proc)
    (call-with-input-string str proc))

  (define (snow-call-with-output-string proc)
    (call-with-output-string proc)))

 (gambit

  (define (snow-call-with-input-string str proc)
    (call-with-input-string str proc))

  (define (snow-call-with-output-string proc)
    (call-with-output-string "" proc)))

 (mit

  (define (snow-call-with-input-string str proc)
    (call-with-input-string str proc))

  (define (snow-call-with-output-string proc)
    (call-with-output-string proc)))

 (else

  (cond-expand

   ((or scheme48
        scsh)

    (define open-input-string make-string-input-port)
    (define open-output-string make-string-output-port)
    (define get-output-string string-output-port-output))

   (else))

  (define (snow-call-with-input-string str proc)
    (let ((port (open-input-string str)))
      (call-with-values
          (lambda () (proc port))
        (lambda vals
          (close-input-port port)
          (apply values vals)))))

  (define (snow-call-with-output-string proc)
    (let ((port (open-output-string)))
      (proc port)
      (let ((str (get-output-string port)))
        ;; work around a Scsh bug:
        ;; (close-output-port port)
        str)))))

;;;----------------------------------------------------------------------------

;;; Serialization and deserialization of objects.

(define (snow-string->object str)
  (snow-call-with-input-string
   str
   (lambda (port)
     (read port))))

(define (snow-object->string obj)
  (snow-call-with-output-string
   (lambda (port)
     (write obj port))))

(define (snow-string->object-list str)
  (snow-call-with-input-string
   str
   (lambda (port)
     (let loop ((lst '()))
       (let ((x (read port)))
         (if (eof-object? x)
             (reverse lst)
             (loop (cons x lst))))))))

(define (snow-object-list->string lst)
  (snow-call-with-output-string
   (lambda (port)
     (let loop ((lst lst))
       (if (pair? lst)
           (begin
             (write (car lst) port)
             (newline port)
             (loop (cdr lst))))))))

;;;----------------------------------------------------------------------------

(define (snow-substring-move! src src-start src-end dst dst-start)

  ;; Copy direction must be selected in case src and dst are the same string.

  (if (< src-start dst-start)
      (let loop1 ((i (- src-end 1))
                  (j (- (+ dst-start (- src-end src-start)) 1)))
        (if (< i src-start)
            dst
            (begin
              (string-set! dst j (string-ref src i))
              (loop1 (- i 1)
                     (- j 1)))))
      (let loop2 ((i src-start)
                  (j dst-start))
        (if (< i src-end)
            (begin
              (string-set! dst j (string-ref src i))
              (loop2 (+ i 1)
                     (+ j 1)))
            dst))))

(define (snow-apply-string-append lst)

  (define (append-rest-at i lst)
    (if (pair? lst)
        (let* ((src (car lst))
               (len (string-length src))
               (dst (append-rest-at (+ i len) (cdr lst))))
          (snow-substring-move! src 0 len dst i)
          dst)
        (make-string i)))

  (append-rest-at 0 lst))

(define (snow-remove-string-prefix str prefix)
  (let ((len-str (string-length str))
        (len-prefix (string-length prefix)))
    (if (and (<= len-prefix len-str)
             (string=? (substring str 0 len-prefix) prefix))
        (substring str len-prefix len-str)
        #f)))

(define (snow-remove-string-suffix str suffix)
  (let ((len-str (string-length str))
        (len-suffix (string-length suffix)))
    (if (and (<= len-suffix len-str)
             (string=? (substring str (- len-str len-suffix) len-str) suffix))
        (substring str 0 (- len-str len-suffix))
        #f)))

(define (snow-string-index-from str char i)
  (let loop ((i i))
    (if (< i (string-length str))
        (if (char=? (string-ref str i) char)
            i
            (loop (+ i 1)))
        #f)))

(define (snow-string-index str char)
  (snow-string-index-from str char 0))

(define (snow-string-split str char)
  (let loop ((i 0) (rev-pieces '()))
    (let ((j (snow-string-index-from str char i)))
      (if (not j)
          (reverse
           (if (< i (string-length str))
               (cons (substring str i (string-length str)) rev-pieces)
               rev-pieces))
          (loop (+ j 1)
                (cons (substring str i j) rev-pieces))))))

;;;============================================================================
