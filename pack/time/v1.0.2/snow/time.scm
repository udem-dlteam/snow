;;;============================================================================

;;; File: "time.scm", Time-stamp: <2007-09-01 23:19:31 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides time related procedures.

(package* time/v1.0.2
 (provide:

  (define (current-time-seconds)) ;; seconds elapsed since 1/1/1970
  (define (sleep-msecs msecs)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Time related procedures.")

 (keywords: time snow)

 (license: lgpl/v2.1)

 (cond-expand
  ((or bigloo chicken)
   (require: bignum/v1))
  (chez
   (require: hostos/v1)
   (require: string/v1))
  (else)))

;;;============================================================================

;;; System dependencies.

(cond-expand

 ((or bigloo
      chicken)

  (define (flonum-seconds->bignum x)
    (let* ((hi-fl (floor (/ x 65536.0)))
           (lo-fl (floor (- x (* hi-fl 65536.0))))
           (hi (inexact->exact hi-fl))
           (lo (inexact->exact lo-fl)))
      (fixnum-list->bignum (list lo hi) 65535))))

 (else

  #f))

(cond-expand

 (bigloo

  (define (current-time-seconds)
    (flonum-seconds->bignum (elong->flonum (current-seconds))))

  (define (sleep-msecs msecs)
    (sleep (* msecs 1000))))

 (chez

  (define (current-time-seconds)
    (snow-call-with-input-string
     (snow-read-shell-command "date +%s")
     read))

  (define (sleep-msecs msecs)
    (snow-shell-command
     (string-append "sleep " (number->string (quotient msecs 1000))))))

 (chicken

  (use posix)

  (define (current-time-seconds)
    (flonum-seconds->bignum (current-seconds)))

  (define (sleep-msecs msecs)
    (sleep (quotient msecs 1000))))

 (gambit

  (define (current-time-seconds)
    (inexact->exact (floor (time->seconds (current-time)))))

  (define (sleep-msecs msecs)
    (thread-sleep! (* 0.001 msecs))))

 (gauche

  (use gauche.threads)

  (define (current-time-seconds)
    (inexact->exact (floor (time->seconds (current-time)))))

  (define (sleep-msecs msecs)
    (thread-sleep! (* 0.001 msecs))))

 (guile

  (define (current-time-seconds)
    (current-time))

  (define (sleep-msecs msecs)
    (usleep (* msecs 1000))))

 (kawa

  (define (current-time-seconds-aux)
    (quotient ((make <java.util.Date>):getTime) 1000))

  (define (current-time-seconds)
    (current-time-seconds-aux))

  (define (sleep-msecs msecs)
    (invoke-static <java.lang.Thread> 'sleep msecs)))

 (larceny

  (require 'time)

  (define (current-time-seconds)
    (call-with-values current-utc-time
      (lambda (secs usecs) secs)))

  (define (sleep-msecs msecs)
    (snow-shell-command
     (string-append "sleep " (number->string (quotient msecs 1000))))))

 (mit

  (define (current-time-seconds)
    (- (get-universal-time) epoch))

  (define (sleep-msecs msecs)
    (snow-error "sleep-msecs is not implemented")))

 (mzscheme

  (define (current-time-seconds)
    ;; maybe we should substract (find-seconds 0 0 0 1 1 1970) ?
    (current-seconds))

  (define (sleep-msecs msecs)
    (sleep (* .001 msecs))))

 (scheme48

  (define (current-time-seconds)
    (time-seconds (current-time)))

  (define (sleep-msecs msecs)
    (sleep msecs)))

 (scm

  (define (current-time-seconds)
    (current-time))

  (define (sleep-msecs msecs)
    (sleep (quotient msecs 1000))))

 (scsh

  (define (current-time-seconds)
    (time))

  (define (sleep-msecs msecs)
    (process-sleep (quotient msecs 1000))))

 (sisc

  (require-extension (srfi 19))

  (define (current-time-seconds)
    (time-second (current-time)))

  (define (sleep-msecs msecs)
    (sleep msecs)))

 (stklos

  (define (current-time-seconds)
    (inexact->exact (floor (time->seconds (current-time)))))

  (define (sleep-msecs msecs)
    (sleep msecs))))

;;;============================================================================
