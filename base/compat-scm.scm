;;;============================================================================

;;; File: "compat-scm.scm", Time-stamp: <2007-04-04 17:24:22 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(defmacro (cond-expand . clauses)

  (define (feature? x)
    (or (memq x '(srfi-0 scm))
        (and (pair? x)
             (cond ((eq? (car x) 'or)
                    (if (pair? (cdr x))
                        (or (feature? (cadr x))
                            (feature? (cons 'or (cddr x))))
                        #f))
                   ((eq? (car x) 'and)
                    (if (pair? (cdr x))
                        (and (feature? (cadr x))
                             (feature? (cons 'or (cddr x))))
                        #t))
                   ((eq? (car x) 'not)
                    (not (feature? (cadr x))))
                   (else
                    (error "unfulfilled cond-expand"))))))

  (let loop ((clauses clauses))
    (if (null? clauses)
        (error "unfulfilled cond-expand")
        (let ((clause (car clauses)))
          (if (or (eq? (car clause) 'else)
                  (feature? (car clause)))
              `(begin ,@(cdr clause))
              (loop (cdr clauses)))))))

(defmacro (define-macro pattern . body)
  (let ((name
         (if (pair? pattern)
             (car pattern)
             pattern))
        (expander
         (if (pair? pattern)
             (cons 'lambda (cons (cdr pattern) body))
             (car body))))
    (cons 'defmacro
          (cons (cons name (cadr expander))
                (cddr expander)))))

;;;============================================================================
