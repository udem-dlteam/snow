;;;============================================================================

;;; File: "cryptio.scm", Time-stamp: <2007-04-05 00:50:56 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides procedures to read and write encrypted files.

(package* cryptio/v1.0.1
 (provide:

  (define* (enter-filename-password filename (reason _) (prompt _)))
  (define* (enter-new-filename-password filename (reason _) (prompt _)))
  (define (read-encrypted-file filename password))
  (define (write-encrypted-file u8vect filename password)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Reading and writing encrypted files.")

 (keywords: crypto snow)

 (license: lgpl/v2.1)

 (require: ttyui/v1)
 (require: genport/v1)
 (require: homovector/v1)
 (require: aes/v1)
 (require: rsa/v1)
 (require: rfc1423/v1))

;;;============================================================================

(define (ask-for-filename-password filename reason)
  (console-display
   "Please enter " reason "password for file " filename ":" #\newline))

(define* (enter-filename-password
          filename
          (reason "access control ")
          (prompt "Password: "))
  (ask-for-filename-password filename "access control ")
  (enter-password prompt))

(define* (enter-new-filename-password
          filename
          (reason "access control ")
          (prompt "Password: "))
  (let loop ()
    (ask-for-filename-password filename reason)
    (let ((password1 (enter-password prompt)))
      (console-display
       "Please enter the password again." #\newline)
      (let ((password2 (enter-password prompt)))
        (if (not (string=? password1 password2))
            (begin
              (console-display
               "Passwords are not the same.  Please start over." #\newline)
              (loop))
            password1)))))

(define-macro (salt-len) 8)
(define-macro (block-len) 16)
(define-macro (key-len) 16)
(define-macro (iter-count) 100)

(define (read-encrypted-file filename password)
  (let ((econtent (genport-read-file filename)))
    (if (< (snow-u8vector-length econtent)
           (+ (salt-len) (block-len)))
        (snow-error "truncated file")
        (let* ((salt
                (snow-subu8vector
                 econtent
                 0
                 (salt-len)))
               (key
                (PBKDF2 password salt (iter-count) (key-len)))
               (password-check1
                (snow-subu8vector
                 econtent
                 (salt-len)
                 (+ (salt-len) (block-len))))
               (password-check2
                (snow-make-u8vector (block-len))))
          (aes-encrypt-ecb
           (u8vector->aes-context key)
           (snow-u8vector-append salt salt)
           0
           password-check2
           0)
          (if (not (equal? (snow-u8vector->list password-check1)
                           (snow-u8vector->list password-check2)))
              #f
              (RFC1423-unpad
               (aes-decrypt-subu8vector
                econtent
                (+ (salt-len) (block-len))
                (snow-u8vector-length econtent)
                key)
               (block-len)))))))

(define (write-encrypted-file u8vect filename password)
  (let* ((salt
          (make-salt (salt-len)))
         (key
          (PBKDF2 password salt (iter-count) (key-len)))
         (password-check
          (snow-make-u8vector (block-len)))
         (padded-u8vect
          (RFC1423-pad u8vect (block-len)))
         (eu8vect
          (aes-encrypt-u8vector padded-u8vect key)))
    (aes-encrypt-ecb
     (u8vector->aes-context key)
     (snow-u8vector-append salt salt)
     0
     password-check
     0)
    (genport-write-file
     (snow-u8vector-append salt password-check eu8vect)
     filename)))

;;;============================================================================
