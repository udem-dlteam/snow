;;;============================================================================

;;; File: "compat-mit.scm", Time-stamp: <2007-09-03 16:26:08 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(define-syntax define-macro
  (rsc-macro-transformer
   (lambda (e r)
     r
     (let* ((pattern (cadr e))
            (name (if (pair? pattern)
                      (car pattern)
                      pattern))
            (lamb (if (pair? pattern)
                      (cons 'lambda (cons (cdr pattern) (cddr e)))
                      (caddr e))))
       `(define-syntax ,name
          (rsc-macro-transformer
           (lambda (e r)
             r
             (apply ,lamb (cdr e)))))))))

;;;============================================================================
