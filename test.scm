":";exec snow -- "$0" "$@"

;;;============================================================================

;;; File: "test.scm", Time-stamp: <2008-03-12 17:10:21 feeley>

;;; Copyright (c) 2006-2008 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Test of the Scheme Now! framework.

(package* test/v1.0.0
 (require: fixnum/v1)
 (require: bignum/v1)
 (require: string/v1)
 (require: filesys/v1)
 (require: hostos/v1)
 (require: homovector/v1)
 (require: time/v1)
 (require: random/v1)
 (require: extio/v1)
 (require: genport/v1)
 (require: aes/v1)
 (require: digest/v1)
 (require: tcpip/v1)
 (require: http/v1)
 (require: zlib/v1)
 (require: tar/v1))

;;;============================================================================

(define nb-failed 0)

(define (failed expr)
  (set! nb-failed (+ 1 nb-failed))
  (display "FAILED: ")
  (write expr)
  (newline))

(display "START OF SELF TEST")
(newline)

;;;----------------------------------------------------------------------------

;;; Sorting.

(define (sort-list lst <?)

  (define (mergesort lst)

    (define (merge lst1 lst2)
      (cond ((null? lst1) lst2)
            ((null? lst2) lst1)
            (else
             (let ((e1 (car lst1)) (e2 (car lst2)))
               (if (<? e1 e2)
                 (cons e1 (merge (cdr lst1) lst2))
                 (cons e2 (merge lst1 (cdr lst2))))))))

    (define (split lst)
      (if (or (null? lst) (null? (cdr lst)))
        lst
        (cons (car lst) (split (cddr lst)))))

    (if (or (null? lst) (null? (cdr lst)))
      lst
      (let* ((lst1 (mergesort (split lst)))
             (lst2 (mergesort (split (cdr lst)))))
        (merge lst1 lst2))))

  (mergesort lst))

;;;----------------------------------------------------------------------------

;;; Test builtin packages

(display "exceptions")

(if (not (equal? 101 (snow-with-exception-catcher
                      (lambda (exc) (+ exc 100))
                      (lambda () (+ 10 (snow-raise 1))))))
    (failed `(equal? 101 (snow-with-exception-catcher
                          (lambda (exc) (+ exc 100))
                          (lambda () (+ 10 (snow-raise 1)))))))

(if (not (snow-condition? (make-snow-condition)))
    (failed `(snow-condition? (make-snow-condition))))

(if (not (snow-condition? (make-type-check-condition 'foo)))
    (failed `(snow-condition? (make-type-check-condition 'foo))))

(display ", records")

(define-record* myrec a b)

(if (snow-vector? (make-myrec 11 22))
    (failed `(not (snow-vector? (make-myrec 11 22)))))

(if (not (myrec? (make-myrec 11 22)))
    (failed `(myrec? (make-myrec 11 22))))

(if (not (equal? 11 (myrec-a (make-myrec 11 22))))
    (failed `(equal? 11 (myrec-a (make-myrec 11 22)))))

(if (not (equal? 33 (let ((r (make-myrec 11 22)))
                      (myrec-a-set! r 33)
                      (myrec-a r))))
    (failed `(equal? 33 (let ((r (make-myrec 11 22)))
                          (myrec-a-set! r 33)
                          (myrec-a r)))))

(if (not (equal? "**exception**"
                 (snow-with-exception-catcher
                  (lambda (exc) "**exception**")
                  (lambda () (+ 100 (myrec-a '#(1 2 3 4)))))))
    (failed `(equal? "**exception**"
                     (snow-with-exception-catcher
                      (lambda (exc) "**exception**")
                      (lambda () (+ 100 (myrec-a '#(1 2 3 4))))))))

;;; Skip the following test because it defines an internal record, which
;;; is not portable.
'
(let ((x (make-myrec 11 22)))

  (define-record* myrec a b)

  (if (not (equal? "**exception**"
                   (snow-with-exception-catcher
                    (lambda (exc) "**exception**")
                    (lambda () (+ 100 (myrec-a x))))))
      (failed `(equal? "**exception**"
                       (snow-with-exception-catcher
                        (lambda (exc) "**exception**")
                        (lambda () (+ 100 (myrec-a x))))))))

;;; Skip the following test because it defines a derived record, which
;;; is not supported.
'(
(define-record* foo-root
  uid: root-382edf4c-8381-4ea3-b6c3-95334aedef7b
  sealed: #f
  a)

(define-record* foo-myrec
  parent: foo-root
  uid: myrec-98f92873-6f8f-4b4d-8570-a559201087a2
  sealed: #f
  b)

(if (not (foo-myrec? (make-foo-myrec 11 22)))
    (failed `(foo-myrec? (make-foo-myrec 11 22))))

(if (not (equal? 11 (foo-root-a (make-foo-myrec 11 22))))
    (failed `(equal? 11 (foo-root-a (make-foo-myrec 11 22)))))

(if (not (equal? 22 (foo-myrec-b (make-foo-myrec 11 22))))
    (failed `(equal? 22 (foo-myrec-b (make-foo-myrec 11 22)))))

(if (not (equal? 33 (let ((r (make-foo-myrec 11 22)))
                      (foo-root-a-set! r 33)
                      (foo-root-a r))))
    (failed `(equal? 33 (let ((r (make-foo-myrec 11 22)))
                          (foo-root-a-set! r 33)
                          (foo-root-a r)))))

(if (not (equal? "**exception**"
                 (snow-with-exception-catcher
                  (lambda (exc) "**exception**")
                  (lambda () (+ 100 (foo-myrec-b '#(1 2 3 4)))))))
    (failed `(equal? "**exception**"
                     (snow-with-exception-catcher
                      (lambda (exc) "**exception**")
                      (lambda () (+ 100 (foo-myrec-b '#(1 2 3 4))))))))
)

;;; Skip the following test because it defines an internal record, which
;;; is not portable.
'
(let ((x (make-foo-myrec 11 22)))

  (define-record* bar-myrec
    parent: foo-root
    uid: myrec-98f92873-6f8f-4b4d-8570-a559201087a2
    sealed: #f
    b)

  (if (not (equal? 122 (snow-with-exception-catcher
                        (lambda (exc) exc)
                        (lambda () (+ 100 (bar-myrec-b x))))))
      (failed `(equal? 122 (snow-with-exception-catcher
                            (lambda (exc) exc)
                            (lambda () (+ 100 (bar-myrec-b x))))))))

(display ", optional parameters")

(define* (opt1 a b (c 11) (d 22))
  (list a b c d))

(if (not (equal? (opt1 1 2) '(1 2 11 22)))
    (failed `(equal? (opt1 1 2) '(1 2 11 22))))

(if (not (equal? (opt1 1 2 3) '(1 2 3 22)))
    (failed `(equal? (opt1 1 2 3) '(1 2 3 22))))

(if (not (equal? (opt1 1 2 3 4) '(1 2 3 4)))
    (failed `(equal? (opt1 1 2 3 4) '(1 2 3 4))))

(if (not (equal? (opt1 1 2) '(1 2 11 22)))
    (failed `(equal? (opt1 1 2) '(1 2 11 22))))

(define* (opt2 a b (c: c 11) (d: d 22))
  (list a b c d))

(if (not (equal? (opt2 1 2) '(1 2 11 22)))
    (failed `(equal? (opt2 1 2) '(1 2 11 22))))

(if (not (equal? (opt2 1 2 c: 3) '(1 2 3 22)))
    (failed `(equal? (opt2 1 2 c: 3) '(1 2 3 22))))

(if (not (equal? (opt2 1 2 d: 3) '(1 2 11 3)))
    (failed `(equal? (opt2 1 2 d: 3) '(1 2 11 3))))

(if (not (equal? (opt2 1 2 d: 3 c: 4) '(1 2 4 3)))
    (failed `(equal? (opt2 1 2 d: 3 c: 4) '(1 2 4 3))))

;;;----------------------------------------------------------------------------

;;; Test keyword

(display ", keywords")

(if (snow-keyword? 'c)
    (failed `(not (snow-keyword? 'c))))

(if (not (snow-keyword? c:))
    (failed `(snow-keyword? c:)))

(if (not (equal? "c" (snow-keyword->string c:)))
    (failed `(equal? "c" (snow-keyword->string c:))))

(if (not (eq? c: (snow-string->keyword "c")))
    (failed `(eq? c: (snow-string->keyword "c"))))

;;;----------------------------------------------------------------------------

;;; Test fixnum

(display ", fixnum")

(if (not (equal? 8 (snow-fxand 10 12)))
    (failed `(equal? 8 (snow-fxand 10 12))))

(if (not (equal? 14 (snow-fxior 10 12)))
    (failed `(equal? 14 (snow-fxior 10 12))))

(if (not (equal? 6 (snow-fxxor 10 12)))
    (failed `(equal? 6 (snow-fxxor 10 12))))

(if (not (equal? 1 (snow-fxnot -2)))
    (failed `(equal? 1 (snow-fxnot -2))))

(if (not (equal? 1600 (snow-fxarithmetic-shift-left 100 4)))
    (failed `(equal? 1600 (snow-fxarithmetic-shift-left 100 4))))

(if (not (equal? 6 (snow-fxarithmetic-shift-right 100 4)))
    (failed `(equal? 6 (snow-fxarithmetic-shift-right 100 4))))

;;;----------------------------------------------------------------------------

;;; Test homovector

(display ", homovector")

(define v1 (snow-make-u8vector 8 0))

(if (not (equal? (snow-u8vector? v1) #t))
    (failed `(equal? (snow-u8vector? ,v1) #t)))

(if (not (equal? (snow-u8vector? "hello") #f))
    (failed `(equal? (snow-u8vector? "hello") #f)))

(if (not (equal? (snow-u8vector? '#(1 2 3)) #f))
    (failed `(equal? (snow-u8vector? '#(1 2 3)) #f)))

(snow-u8vector-set! v1 0 3)
(snow-u8vector-set! v1 1 1)
(snow-u8vector-set! v1 2 4)
(snow-u8vector-set! v1 3 10)
(snow-u8vector-set! v1 4 5)
(snow-u8vector-set! v1 5 13)
(snow-u8vector-set! v1 6 10)

(if (not (equal? (snow-u8vector-length v1) 8))
    (failed `(equal? (snow-u8vector-length ,v1) 8)))

(if (not (equal? (snow-u8vector-ref v1 3) 10))
    (failed `(equal? (snow-u8vector-ref ,v1 3) 10)))

(if (not (equal? (snow-u8vector->list v1) '(3 1 4 10 5 13 10 0)))
    (failed `(equal? (snow-u8vector->list ,'v1) '(3 1 4 10 5 13 10 0))))

(define v2 (snow-make-u8vector 8 99))

(snow-u8vector-set! v2 0 3)
(snow-u8vector-set! v2 1 1)
(snow-u8vector-set! v2 7 0)

(if (not (equal? (snow-u8vector->list v2) '(3 1 99 99 99 99 99 0)))
    (failed `(equal? (snow-u8vector->list ,v2) '(3 1 99 99 99 99 99 0))))

(define v3 (snow-make-u16vector 8 0))

(if (not (equal? (snow-u16vector? v3) #t))
    (failed `(equal? (snow-u16vector? ,v3) #t)))

(if (not (equal? (snow-u16vector? "hello") #f))
    (failed `(equal? (snow-u16vector? "hello") #f)))

(if (not (equal? (snow-u16vector? '#(1 2 3)) #f))
    (failed `(equal? (snow-u16vector? '#(1 2 3)) #f)))

(snow-u16vector-set! v3 0 3)
(snow-u16vector-set! v3 1 1)
(snow-u16vector-set! v3 2 4)
(snow-u16vector-set! v3 3 10)
(snow-u16vector-set! v3 4 5)
(snow-u16vector-set! v3 5 13)
(snow-u16vector-set! v3 6 10)

(if (not (equal? (snow-u16vector-length v3) 8))
    (failed `(equal? (snow-u16vector-length ,v3) 8)))

(if (not (equal? (snow-u16vector-ref v3 3) 10))
    (failed `(equal? (snow-u16vector-ref ,v3 3) 10)))

(if (not (equal? (snow-u16vector->list v3) '(3 1 4 10 5 13 10 0)))
    (failed `(equal? (snow-u16vector->list ,v3) '(3 1 4 10 5 13 10 0))))

(define v4 (snow-make-u16vector 8 99))

(snow-u16vector-set! v4 0 3)
(snow-u16vector-set! v4 1 1)
(snow-u16vector-set! v4 7 0)

(if (not (equal? (snow-u16vector->list v4) '(3 1 99 99 99 99 99 0)))
    (failed `(equal? (snow-u16vector->list ,v4) '(3 1 99 99 99 99 99 0))))

;;;----------------------------------------------------------------------------

;;; Test time

(display ", time")

(define start-time (current-time-seconds))

;; The following test will fail after Sun Mar 13 03:06:40 EDT 2011
;; If a newer version of Snow is not available, just increase the
;; value of range-hi .

(let* ((range-lo (string->bignum "1168000000"))
       (range-hi (string->bignum "1300000000"))
       (s start-time))
  (if (not (and (bignum>= s range-lo) (bignum<= s range-hi)))
      (failed `(and (bignum>= ,s ,range-lo) (bignum<= ,s ,range-hi)))))

;;;----------------------------------------------------------------------------

;;; Test bignum

(display ", bignum")

(define (try-op x y name op bop)
  (let* ((bx (fixnum->bignum x))
         (by (fixnum->bignum y))
         (e (op x y))
         (be (fixnum->bignum e))
         (br (bop bx by))
         (r (bignum->fixnum br))
         (se (number->string e 10))
         (sbr (bignum->string br 10)))
    (if (not (and (equal? be br)
                  (equal? e r)
                  (equal? se sbr)))
        (failed `(and (equal? ,be ,br)
                      (equal? ,e ,r)
                      (equal? ,se ,sbr))))))

(define (try-pair x y)
  (try-op x y '+ + bignum+)
  (try-op x y '- - bignum-)
  (try-op x y '* * bignum*)
  (if (not (= y 0))
      (begin
        (try-op x y 'quotient  quotient  bignum-quotient)
        (try-op x y 'remainder remainder bignum-remainder)
        (try-op x y 'modulo    modulo    bignum-modulo)))
  ' ;; don't try expt, numbers get too large
  (if (>= y 0)
      (try-op x y 'expt expt bignum-expt)))

(define (test-range n m)
  (let loop1 ((x (- n)))
    (if (< x n)
        (begin
          (let loop2 ((y (- n)))
            (if (< y n)
                (begin
                  (try-pair (* m x) (* m y))
                  (loop2 (+ y 1)))))
          (loop1 (+ x 1))))))

(test-range 9 1)
(test-range 9 10)
(test-range 9 100)
(test-range 9 1000)

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

(if (not (equal? (bignum->string (pi-brent-salamin 50))
                 "314159265358979323846264338327950288419716939937453"))
    (failed `(equal? (bignum->string (pi-brent-salamin 50))
                     "314159265358979323846264338327950288419716939937453")))

;;;----------------------------------------------------------------------------

;;; Test string

(display ", string")

(let ((r
       (snow-call-with-input-string
        "1 2"
        (lambda (i)
          (let loop ((lst '()))
            (let ((x (read i)))
              (if (eof-object? x)
                  (reverse lst)
                  (loop (cons x lst)))))))))
  (if (not (equal? '(1 2) r))
      (failed `(equal? '(1 2) ,r))))

(let ((s
       (snow-call-with-output-string
        (lambda (o)
          (write '((1 2) 34 "a B") o)))))
  (if (not (equal? "((1 2) 34 \"a B\")" s))
      (failed `(equal? "((1 2) 34 \"a B\")" ,s))))

;;;----------------------------------------------------------------------------

;;; Test filesys

(display ", filesys")

(define test-dir "test0000.dir")
(define test-subdir1 ".test1111.dir")
(define test-subdir2 "test2222.dir")
(define test-subfile1 "test3333.tmp")
(define test-subfile2 "test4444.tmp")

(define (join dir file)
  (snow-make-filename dir file))

(if (snow-file-exists? test-dir)
    (failed `(not (snow-file-exists? ,test-dir))))

(if (not (snow-with-exception-catcher
           (lambda (exc) #f)
           (lambda () (snow-create-directory test-dir) #t)))
    (failed `(snow-with-exception-catcher
              (lambda (exc) #f)
              (lambda () (snow-create-directory ,test-dir) #t))))

(if (not (snow-file-exists? test-dir))
    (failed `(snow-file-exists? ,test-dir)))

(if (not (snow-file-directory? test-dir))
    (failed `(snow-file-directory? ,test-dir)))

(if (not (snow-with-exception-catcher
           (lambda (exc) #f)
           (lambda () (snow-delete-directory test-dir) #t)))
    (failed `(snow-with-exception-catcher
              (lambda (exc) #f)
              (lambda () (snow-delete-directory ,test-dir) #t))))

(if (snow-file-exists? test-dir)
    (failed `(not (snow-file-exists? ,test-dir))))

(if (not (snow-with-exception-catcher
           (lambda (exc) #f)
           (lambda () (snow-create-directory test-dir) #t)))
    (failed `(snow-with-exception-catcher
              (lambda (exc) #f)
              (lambda () (snow-create-directory ,test-dir) #t))))

(if (snow-with-exception-catcher
     (lambda (exc) #f)
     (lambda () (snow-create-directory test-dir) #t))
    (failed `(not (snow-with-exception-catcher
                   (lambda (exc) #f)
                   (lambda () (snow-create-directory ,test-dir) #t)))))

(snow-create-directory (join test-dir test-subdir1))
(snow-create-directory (join test-dir test-subdir2))
(close-output-port (open-output-file (join test-dir test-subfile1)))

(if (not (equal? (sort-list (snow-directory-files test-dir) string<=?)
                 (list test-subdir1
                       test-subdir2
                       test-subfile1)))
    (failed `(equal? (sort-list (snow-directory-files ,test-dir) string<=?)
                     ',(list test-subdir1
                             test-subdir2
                             test-subfile1))))

(if (not (snow-file-directory? (join test-dir test-subdir1)))
    (failed `(snow-file-directory? ,(join test-dir test-subdir1))))

(if (not (snow-file-exists? (join test-dir test-subfile1)))
    (failed `(snow-file-exists? ,(join test-dir test-subfile1))))

(if (snow-file-directory? (join test-dir test-subfile1))
    (failed `(not (snow-file-directory? ,(join test-dir test-subfile1)))))

(snow-rename-file
 (join test-dir test-subfile1)
 (join test-dir test-subfile2))

(if (snow-file-exists? (join test-dir test-subfile1))
    (failed `(not (snow-file-exists? ,(join test-dir test-subfile1)))))

(if (not (snow-file-exists? (join test-dir test-subfile2)))
    (failed `(snow-file-exists? ,(join test-dir test-subfile2))))

(snow-delete-file (join test-dir test-subfile2))

(if (snow-file-exists? (join test-dir test-subfile2))
    (failed `(not (snow-file-exists? ,(join test-dir test-subfile2)))))

(snow-delete-directory (join test-dir test-subdir2))
(snow-delete-directory (join test-dir test-subdir1))
(snow-delete-directory test-dir)

(if (snow-file-exists? test-dir)
    (failed `(not (snow-file-exists? ,test-dir))))

;;;----------------------------------------------------------------------------

;;; Test hostos

(display ", hostos")

(let ((res
       (snow-with-exception-catcher
        (lambda (exc)
          '())
        (lambda ()
          (snow-command-line)))))
  (if (not (= 1 (length res)))
      (failed `(= 1 (length ',res)))))

(if (not (string? (snow-getenv "PATH")))
    (failed `(string? (snow-getenv "PATH"))))

(if (not (equal? 999 (snow-getenv "UNKNOWNVAR" 999)))
    (failed `(equal? 999 (snow-getenv "UNKNOWNVAR" 999))))

(if (not (unbound-hostos-env-var-condition?
          (snow-with-exception-catcher
           (lambda (exc) exc)
           (lambda () (string-append "FOO" (snow-getenv "UNKNOWNVAR"))))))
    (failed `(unbound-hostos-env-var-condition?
              (snow-with-exception-catcher
               (lambda (exc) exc)
               (lambda () (string-append "FOO" (snow-getenv "UNKNOWNVAR")))))))

(define test-file "test9999.tmp")
(define command1 (string-append "echo hello > " test-file))
(define command2 (string-append "unknowncommand > " test-file))

(if (snow-file-exists? test-file)
    (snow-delete-file test-file))

(if (not (equal? 0 (snow-shell-command command1)))
    (failed `(equal? 0 (snow-shell-command ,command1))))

(if (not (snow-file-exists? test-file))
    (failed `(snow-file-exists? ,test-file))
    (begin
      (if (not (equal? 'hello (with-input-from-file test-file read)))
          (failed `(equal? 'hello (with-input-from-file ,test-file read))))
      (snow-delete-file test-file)))

(if (equal? 0 (snow-shell-command command2))
    (failed `(not (equal? 0 (snow-shell-command ,command2)))))

(if (snow-file-exists? test-file)
    (snow-delete-file test-file))

;;;----------------------------------------------------------------------------

;;; Test random

(display ", random")

(let ((v (make-random-u8vector 10)))
  (if (not (and (snow-u8vector? v)
                (equal? 10 (snow-u8vector-length v))))
      (failed `(and (snow-u8vector? ,v)
                    (equal? 10 (snow-u8vector-length ,v))))))

;;;----------------------------------------------------------------------------

;;; Test extio

(display ", extio")

(let ((s
       (snow-call-with-output-string
        (lambda (o)
          (display "hello" o)
          (snow-force-output o)))))
  (if (not (equal? "hello" s))
      (failed `(equal? "hello" ,s))))

;;;----------------------------------------------------------------------------

;;; Test genport

(display ", genport")

(if (snow-file-exists? test-file)
    (snow-delete-file test-file))

(let ((genport-out (genport-open-output-file test-file)))
  (let ((n (genport-write-subu8vector v1 2 7 genport-out)))
    (if (not (equal? n 5))
        (failed `(equal? ,n 5))))
  (genport-close-output-port genport-out))

(let ((genport-in (genport-open-input-file test-file)))
  (let ((n (genport-read-subu8vector v2 2 7 genport-in)))
    (if (not (equal? n 5))
        (failed `(equal? ,n 5))))
  (let loop ((i 0))
    (if (< i (snow-u8vector-length v2))
        (begin
          (if (not (equal? (snow-u8vector-ref v1 i)
                           (snow-u8vector-ref v2 i)))
              (failed `(equal? (snow-u8vector-ref ,v1 ,i)
                               (snow-u8vector-ref ,v2 ,i))))
          (loop (+ i 1)))))
  (let ((n (genport-read-subu8vector v2 2 7 genport-in)))
    (if (not (equal? n 0))
        (failed `(equal? ,n 0))))
  (genport-close-input-port genport-in))

(let ((p (genport-open-output-u8vector)))
  (genport-write-subu8vector v1 0 (snow-u8vector-length v1) p)
  (let ((r (genport-get-output-u8vector p)))
    (if (not (equal? (snow-u8vector->list v1)
                     (snow-u8vector->list r)))
        (failed `(equal? (snow-u8vector->list ,v1)
                         (snow-u8vector->list r))))))
  
(if (snow-file-exists? test-file)
    (snow-delete-file test-file))

;;;----------------------------------------------------------------------------

;;; Test digest

(display ", digest")

(define crc32-test-vectors
  '(
    ("" 0 ""
     "00000000")
    ("" 0 "a"
     "e8b7be43")
    ("" 0 "abc"
     "352441c2")
    ("" 0 "abcdefghijklmnopqrstuvwxyz"
     "4c2750bd")
    ("" 0 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
     "1fc2e6d2")
    ("" 0 "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
     "7ca94a72")
    ))

(define md5-test-vectors
  '(
    ;; from RFC 1321:
    ("" 0 ""
     "d41d8cd98f00b204e9800998ecf8427e")
    ("" 0 "a"
     "0cc175b9c0f1b6a831c399e269772661")
    ("" 0 "abc"
     "900150983cd24fb0d6963f7d28e17f72")
    ("" 0 "message digest"
     "f96b697d7cb7938d525a2f31aaf161d0")
    ("" 0 "abcdefghijklmnopqrstuvwxyz"
     "c3fcd3d76192e4007dfb496cca67e13b")
    ("" 0 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
     "d174ab98d277d9f5a5611c2c9f419d9f")
    ("" 0 "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
     "57edf4a22be3c955ac49da2e2107b67a")
    ))

(define sha-1-test-vectors
  '(
    ("" 0 ""
     "da39a3ee5e6b4b0d3255bfef95601890afd80709")
    ;; from RFC 3174:
    ("" 0 "abc"
     "a9993e364706816aba3e25717850c26c9cd0d89d")
    ("" 0 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
     "84983e441c3bd26ebaae4aa1f95129e5e54670f1")
;;    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 10000 ""
;;     "34aa973cd4c4daa4f61eeb2bdbad27316534016f")
    ("0123456701234567012345670123456701234567012345670123456701234567" 10 ""
     "dea356a2cddd90c7a7ecedc5ebb563934f460452")
    ))

(define sha-224-test-vectors
  '(
    ;; from RFC 3874:
    ("" 0 "abc"
     "23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7")
    ("" 0 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
     "75388b16512776cc5dba5da1fd890150b0c6455cb4f58b1952522525")
;;    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 10000 ""
;;     "20794655980c91d8bbb4c1ea97618a4bf03f42581948b2ee4ee7ad67")
   ))

(define sha-256-test-vectors
  '(
    ("" 0 ""
     "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    ;; from FIPS-180-2:
    ("" 0 "abc"
     "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    ("" 0 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
     "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
;;    ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 10000 ""
;;     "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0")
   ))

(define (test-digest)

  (define (t algorithm vectors)

;;    (display "*** ====================== testing ")
;;    (write algorithm)
;;    (display " ======================")
;;    (newline)

    (for-each
     (lambda (v)
       (let ((str1 (car v))
             (repeat (cadr v))
             (str2 (caddr v))
             (expect (cadddr v)))
         (let ((md
                (if (= repeat 0)
                    (digest-string str2 algorithm 'hex)
                    (let ((u8vect1
                           (snow-list->u8vector
                            (map char->integer (string->list str1))))
                          (u8vect2
                           (snow-list->u8vector
                            (map char->integer (string->list str2))))
                          (digest
                           (open-digest algorithm)))
                      (let loop ((i 0))
                        (if (< i repeat)
                            (begin
                              (digest-update-subu8vector
                               digest
                               u8vect1
                               0
                               (snow-u8vector-length u8vect1))
                              (loop (+ i 1)))
                            (begin
                              (digest-update-subu8vector
                               digest
                               u8vect2
                               0
                               (snow-u8vector-length u8vect2))
                              (close-digest digest 'hex))))))))
           (if (string-ci=? md expect)
'               (begin
                 (display "*** passed ")
                 (write v)
                 (newline))
                (failed `(string-ci=? ,md ,expect))))))
     vectors)

;;    (display (string-append "*** passed all "
;;                            (symbol->string algorithm)
;;                            " tests"))
;;    (newline)
)

  (t 'crc32 crc32-test-vectors)
  (t 'md5 md5-test-vectors)
  (t 'sha-1 sha-1-test-vectors)
;;  (t 'sha-224 sha-224-test-vectors)
;;  (t 'sha-256 sha-256-test-vectors)
)

(test-digest)

;;;----------------------------------------------------------------------------

;;; Test aes

(display ", aes")

(define (reverse-u8vector v)
  (snow-list->u8vector (reverse (snow-u8vector->list v))))

(define aes-test-vectors
  (list
   (list
    "06a9214036b8a15b512e03d534120006"
    "3dafba429d9eb430b422da802c9fac41"
    (snow-u8vector->hex-string
     (reverse-u8vector (snow-ISO-8859-1-string->u8vector "Single block msg")))
    "e353779c1079aeb82708942dbe77181a")
   (list
    "c286696d887c9aa0611bbb3e2025a45a"
    "562e17996d093d28ddb3ba695a2e6f58"
    "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
    "d296cd94c2cccf8a3a863028b5e1dc0a7586602d253cfff91b8266bea6d61ab1")
   (list
    "6c3ea0477630ce21a2ce334aa746c2cd"
    "c782dc4c098c66cbd9cd27d825682c81"
    (snow-u8vector->hex-string
     (reverse-u8vector (snow-ISO-8859-1-string->u8vector "This is a 48-byte message (exactly 3 AES blocks)")))
    "d0a02b3836451753d493665d33f0e8862dea54cdb293abc7506939276772f8d5021c19216bad525c8579695d83ba2684")
   (list
    "56e47a38c5598974bc46903dba290349"
    "8ce82eefbea0da3c44699ed7db51b7d9"
    "a0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedf"
    "c30e32ffedc0774e6aff6af0869f71aa0f3af07a9a31a9c684db207eb0ef8e4e35907aa632c3ffdf868bb7b29d3d46ad83ce9f9a102ee99d49a53e87f4c3da55")
   (list
    "2b7e151628aed2a6abf7158809cf4f3c"
    "000102030405060708090a0b0c0d0e0f"
    "6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710"
    "7649abac8119b246cee98e9b12e9197d5086cb9b507219ee95db113a917678b273bed6b8e3c1743b7116e69e222295163ff1caa1681fac09120eca307586e1a7")))

(define (test-encrypt-decrypt key-hex iv-hex message-hex expect-hex)
  (let* ((key (reverse-u8vector (snow-hex-string->u8vector key-hex)))
         (iv (reverse-u8vector (snow-hex-string->u8vector iv-hex)))
         (iv2 (reverse-u8vector (snow-hex-string->u8vector iv-hex)))
         (message (reverse-u8vector (snow-hex-string->u8vector message-hex)))
         (output (aes-encrypt-u8vector message key iv))
         (output2 (aes-decrypt-u8vector output key iv2))
         (output-hex (snow-u8vector->hex-string (reverse-u8vector output)))
         (output2-hex (snow-u8vector->hex-string (reverse-u8vector output2))))
    (if (not (and (equal? output-hex expect-hex)
                  (equal? output2-hex message-hex)))
        (failed `(and (equal? ,output-hex ,expect-hex)
                      (equal? ,output2-hex ,message-hex))))))

(define (test-aes)
  (for-each
   (lambda (x)
     (test-encrypt-decrypt (car x) (cadr x) (caddr x) (cadddr x)))
   aes-test-vectors))

(test-aes)

;;;----------------------------------------------------------------------------

;;; Test tcpip

(define server "snow.iro.umontreal.ca")
(define port-num 80)
(define request "selftest.bin")

'(begin ;; skip TCP/IP test

(display ", tcpip")

(define expected
  (string-append
   "This is a file used for self tests."
   (string #\newline)
   "It contains 2 lines."
   (string #\newline)))

(let* ((res
        (snow-with-exception-catcher
         (lambda (exc)
           (cons 0 (snow-make-u8vector 0)))
         (lambda ()
           (tcpip-call-with-tcp-client
            server
            port-num
            (lambda (in out send-output)
              (display (string-append "GET /" request) out)
              (newline out)
              (newline out)
              (send-output #t)
              (let* ((genport-port (genport-native-input-port->genport in))
                     (content (snow-make-u8vector 256))
                     (n (genport-read-subu8vector content 0 256 genport-port)))
                (genport-close-input-port genport-port)
                (cons n content)))))))
       (n
        (car res))
       (content
        (cdr res)))
  (if (not (equal? n 256))
      (failed `(equal? ,n 256)))
  (let loop ((i 0))
    (if (< i n)
        (begin
          (if (not (equal? (snow-u8vector-ref content i) i))
              (failed `(equal? (snow-u8vector-ref content ,i) ,i)))
          (loop (+ i 1))))))
)

;;;----------------------------------------------------------------------------

;;; Test http

(display ", http")

(define expected2
  (let ((v (snow-make-u8vector 256)))
    (let loop ((i 0))
      (if (< i 256)
          (begin
            (snow-u8vector-set! v i i)
            (loop (+ i 1)))
          v))))

(let* ((d (http-get-document server (string-append "/" request)))
       (v (if d (cdr d) (snow-make-u8vector 0)))
       (n (snow-u8vector-length v)))
  (if (not (equal? n (snow-u8vector-length expected2)))
      (failed `(equal? ,n (snow-u8vector-length ,expected2))))
  (let loop ((i 0))
    (if (< i (snow-u8vector-length v))
        (begin
          (if (not (equal? (snow-u8vector-ref v i)
                           (snow-u8vector-ref expected2 i)))
              (failed `(equal? (snow-u8vector-ref ,v ,i)
                               (snow-u8vector-ref ,expected2 ,i))))
          (loop (+ i 1))))))

;;;----------------------------------------------------------------------------

;;; Test zlib

(display ", zlib")

(define zlib-file1 "test.tgz")
(define zlib-file2 "test.tar")
(define zlib-file3 "test2.tgz")
(define zlib-file4 "test2.tar")

(define (transform in-filename out-filename filter)
  (let* ((f (genport-open-input-file in-filename))
         (in (filter f))
         (out (genport-open-output-file out-filename))
         (bufsize 1024)
         (buf (snow-make-u8vector bufsize)))
    (let loop ()
      (let ((n (genport-read-subu8vector buf 0 bufsize in)))
        (if (> n 0)
            (begin
              (genport-write-subu8vector buf 0 n out)
              (loop)))))
    (genport-close-output-port out)
    (genport-close-input-port in)
    (genport-close-input-port f)))

(define (gzip in-filename out-filename)
  (transform in-filename out-filename gzip-genport))

(define (gunzip in-filename out-filename)
  (transform in-filename out-filename gunzip-genport))

(if (snow-file-exists? zlib-file2)
    (snow-delete-file zlib-file2))

(if (snow-file-exists? zlib-file3)
    (snow-delete-file zlib-file3))

(if (snow-file-exists? zlib-file4)
    (snow-delete-file zlib-file4))

(gunzip zlib-file1 zlib-file2)
(gzip zlib-file2 zlib-file3)
(gunzip zlib-file3 zlib-file4)

(if (not (equal? 0 (snow-shell-command
                    (string-append "diff -a -q "
                                   zlib-file2
                                   " "
                                   zlib-file4))))
    (failed `(equal? 0 (snow-shell-command
                        (string-append "diff -a -q "
                                       ,zlib-file2
                                       " "
                                       ,zlib-file4)))))

;;;----------------------------------------------------------------------------

;;; Test tar

(display ", tar")

(define tar-file1 zlib-file2)
(define tar-file2 "test3.tar")
(define tar-file3 "test4.tar")

(define (unpack-pack i o)
  (let ((t (tar-unpack-file i)))
    (if (not (equal? (map tar-rec-name t)
                     '("test/"
                       "test/textfile"
                       "test/textfile.gz")))
        (failed `(equal? (map ,tar-rec-name t)
                         '("test/"
                           "test/textfile"
                           "test/textfile.gz"))))
    (if (not (equal? (map (lambda (v) (snow-u8vector-length v))
                          (map tar-rec-content t))
                     '(0 612 54)))
        (failed `(equal? (map (lambda (v) (snow-u8vector-length v))
                              (map ,tar-rec-content t))
                         '(0 612 54))))
    (tar-pack-file t o)))

(unpack-pack tar-file1 tar-file2)
(unpack-pack tar-file2 tar-file3)

(if (not (equal? 0 (snow-shell-command
                    (string-append "diff -a -q "
                                   tar-file2
                                   " "
                                   tar-file3))))
    (failed `(equal? 0 (snow-shell-command
                        (string-append "diff -a -q "
                                       ,tar-file2
                                       " "
                                       ,tar-file3)))))

(if (snow-file-exists? tar-file2)
    (snow-delete-file tar-file2))

(if (snow-file-exists? tar-file3)
    (snow-delete-file tar-file3))

;;;----------------------------------------------------------------------------

(if (snow-file-exists? zlib-file2)
    (snow-delete-file zlib-file2))

(if (snow-file-exists? zlib-file3)
    (snow-delete-file zlib-file3))

(if (snow-file-exists? zlib-file4)
    (snow-delete-file zlib-file4))

;;;----------------------------------------------------------------------------

;;; Test time

;;(display ", time")

(define end-time (current-time-seconds))

(let ((run-time (bignum->fixnum (bignum- end-time start-time))))
  (if (not (and (>= run-time 0) (<= run-time 300)))
      (failed `(and (>= ,run-time 0) (<= ,run-time 300)))))

;;;----------------------------------------------------------------------------

(newline)

(display "END OF SELF TEST")
(if (> nb-failed 0)
    (begin
      (display "   *** ")
      (write nb-failed)
      (display " TESTS FAILED ***")
      (newline)
      (snow-exit 1))
    (begin
      (display "   *** PASSED ALL TESTS ***")
      (newline)))

;;;============================================================================
