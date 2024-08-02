;;;============================================================================

;;; File: "compat-mzscheme.scm", Time-stamp: <2007-04-04 17:24:10 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(define-syntax cond-expand
  (syntax-rules (and or not else srfi-0 mzscheme)
    ((cond-expand) (syntax-error "Unfulfilled cond-expand"))
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
    ((cond-expand (mzscheme body ...) more-clauses ...)
       (begin body ...))
    ((cond-expand (feature-id body ...) more-clauses ...)
       (cond-expand more-clauses ...))))

(require (lib "defmacro.ss"))
(require (lib "process.ss"))
(require (lib "pretty.ss"))

;;;============================================================================
