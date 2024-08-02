;;;=============================================================================

;;; File: "ttyui.scm", Time-stamp: <2007-04-05 00:54:59 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;=============================================================================

;;; Provides procedures to enter passwords at the console.

(package* ttyui/v1.0.1
 (provide:

  (define (current-console-input-port))
  (define (current-console-output-port))
  (define (console-display . args))
  (define* (enter-y-or-n (prompt _)))
  (define* (enter-line-ascii (prompt _)))
  (define* (enter-password (prompt _)))
  (define* (enter-new-password (prompt _))))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Terminal-based user-interface utility procedures.")

 (keywords: snow)

 (license: lgpl/v2.1)

 (require: extio/v1))

;;;=============================================================================

(define console-input-port
  (current-input-port))

(define console-output-port
  (current-output-port))

(define (current-console-input-port)
  console-input-port)

(define (current-console-output-port)
  console-output-port)

(define (console-display . args)
  (let ((port (current-console-output-port)))
    (for-each (lambda (x) (display x port)) args)
    (snow-force-output)))

(define (tty-read-line-ascii)
  (let ((port (current-console-input-port)))
    (let loop ((lst '()))
      (let ((c (read-char port)))
        (if (and (char? c)
                 (not (char=? c #\newline)))
            (if (or (char<? c #\space) (char>? c #\~))
                (loop lst)
                (loop (cons c lst)))
            (list->string (reverse lst)))))))

(define (tty-read-line-ascii-no-echo)
  ;; TODO: turn off echo!
  (tty-read-line-ascii))

(define* (enter-y-or-n (prompt "y/n? "))
  (let loop ()
    (console-display prompt)
    (let ((s (tty-read-line-ascii)))
      (cond ((or (string=? s "n") (string=? s "N"))
             #f)
            ((or (string=? s "y") (string=? s "Y"))
             #t)
            (else
             (console-display
              "You must enter y or n.  Please try again." #\newline)
             (loop))))))

(define* (enter-line-ascii (prompt ""))
  (console-display prompt)
  (tty-read-line-ascii))

(define* (enter-password (prompt "Password: "))
  (console-display prompt)
  (tty-read-line-ascii-no-echo))

(define (strong-password? password)
  (let ((classes
         (map (lambda (c)
                (cond ((char-alphabetic? c) 'alphabetic)
                      ((char-numeric? c)    'numeric)
                      (else                 'special)))
              (string->list password))))
    (and (>= (string-length password) 8)
         (memq 'alphabetic classes)
         (memq 'numeric classes)
         (memq 'special classes))))

(define* (enter-new-password (prompt "Password: "))
  (let loop ()
    (let ((password (enter-password prompt)))
      (cond ((string=? password "")
             #f)
            ((not (strong-password? password))
             (console-display
              "Password is weak." #\newline
              "Please enter a password with at least 8 characters, and which includes" #\newline
              "a combination of letters, numerals and special characters." #\newline)
             (loop))
            (else
             (console-display
              "Please retype for confirmation."
              #\newline)
             (if (not (string=? password (enter-password prompt)))
                 (begin
                   (console-display
                    "Password is not the same.  Please try again." #\newline)
                   (loop))
                 password))))))

;;;=============================================================================
