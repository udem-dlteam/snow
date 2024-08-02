;;;============================================================================

;;; File: "compat-sisc.scm", Time-stamp: <2007-09-03 15:31:19 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(require-library 'sisc/libs/srfi/srfi-9)

(define _snow:command-line-arguments #f)
(define _snow:exit-continuation #f)

(define (_snow:run-program . args)
  (call-with-current-continuation
   (lambda (k)
     (set! _snow:exit-continuation k)
     (set! _snow:command-line-arguments args)
     (eval (list 'load-program* (car args)))
     0)))

(define (_snow:compile-package . args)
  (for-each
   (lambda (source-file)
     (let* ((base-file
             (list->string
              (reverse (cdr (member #\.
                                    (reverse (string->list source-file)))))))
            (scc-file
             (string-append base-file ".scc")))
       (compile-file source-file scc-file)))
   args)
  0)

;;;============================================================================
