;;;============================================================================

;;; File: "compat-larceny.scm", Time-stamp: <2007-08-31 16:40:05 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; For cond-expand
(require 'srfi-0)

(define-syntax define-macro
  (transformer 
   (lambda (exp rename compare)
     
     (define (arglist? x)
       (or (symbol? x)
           (null? x)
           (and (pair? x)
                (symbol? (car x))
                (arglist? (cdr x)))))
     
     ;; Uncomment this to trace macro expansion behavior
     ; (begin (display exp) (newline))
     
     (if (not (and (list? exp)
                   (>= (length exp) 3)))
         (error "Bad macro definition: " exp))
     
     (let* ((define-syntax* (rename 'define-syntax))
            (transformer* (rename 'transformer))
            (name+xform-exp (if (pair? (list-ref exp 1))
                                (let ((name (car (list-ref exp 1)))
                                      (args (cdr (list-ref exp 1)))
                                      (body (list-tail exp 2)))
                                  (list name 
                                        `(lambda ,args ,@body)))
                                (list (list-ref exp 1)
                                      (list-ref exp 2))))
            (name (car name+xform-exp))
            (xform-exp (cadr name+xform-exp)))
       `(,define-syntax* ,name
          (,transformer*
           (lambda (_defmacro_exp _defmacro_rename _defmacro_compare)
             
             (let ((final-exp (apply ,xform-exp (cdr _defmacro_exp))))
               ;; Uncomment this to trace macro expansion behavior
               ;(display _defmacro_exp) (display " ==> ")
               ;(display final-exp)     (newline)
               final-exp
               )))))
     )))
         

(require 'file-system) ;; for list-directory

(define (_snow:error . args) (apply error args))

;;;============================================================================
