;;;============================================================================

;;; File: "compat-scheme48.scm", Time-stamp: <2007-04-04 17:24:16 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(define-syntax cond-expand
  (syntax-rules (and or not else srfi-0 scheme48)
    ((cond-expand) '(syntax-error "Unfulfilled cond-expand"))
    ((cond-expand (else body ...))
     (begin body ...))
    ((cond-expand ((and) body ...) more-clauses ...)
     (begin body ...))
    ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
         (cond-expand
           ((and req2 ...) body ...)
           more-clauses ...))
       more-clauses ...))
    ((cond-expand ((or) body ...) more-clauses ...)
     (cond-expand more-clauses ...))
    ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
     (cond-expand
       (req1
        (begin body ...))
       (else
        (cond-expand
           ((or req2 ...) body ...)
           more-clauses ...))))
    ((cond-expand ((not req) body ...) more-clauses ...)
     (cond-expand
       (req
         (cond-expand more-clauses ...))
       (else body ...)))
    ((cond-expand (srfi-0 body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (scheme48 body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (feature-id body ...) more-clauses ...)
       (cond-expand more-clauses ...))))

(define-syntax define-macro
  (lambda (e r c)
    (let* ((pattern (cadr e))
           (name (if (pair? pattern)
                     (car pattern)
                     pattern))
           (lamb (if (pair? pattern)
                     (cons 'lambda (cons (cdr pattern) (cddr e)))
                     (caddr e))))
    `(define-syntax ,name
       (lambda (e r c)
         (let ((args (cdr e)))
           (apply ,lamb args)))))))

;;; The random character encoding in Scheme48 and scsh has to be the
;;; most stupid design decision I have ever seen.  I have spent a
;;; couple of hours figuring out how to test if a character is a
;;; "carriage return" (you can't use (char=? c #\return), or (string=?
;;; (string c) "\r") or (= (char->integer c) 13)).

(define integer->char ascii->char)
(define char->integer char->ascii)

;;;============================================================================
