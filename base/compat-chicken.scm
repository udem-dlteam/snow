;;;============================================================================

;;; File: "compat-chicken.scm", Time-stamp: <2007-04-04 17:41:02 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(use utils)

(use posix) ;; so that (apropos-list "") includes the ids from posix, tcp, etc.
(use tcp)
(require-extension srfi-4)

(define snow-chicken-ids
  (apropos-list ""))

(define snow-namespace-env '())

(define (snow-namespace-declare prefix ids)
  (set! snow-namespace-env
        (if (null? ids)
            (list (cons "" snow-chicken-ids)
                  (cons prefix ids))
            (cons (cons prefix ids)
                  snow-namespace-env))))

(define (snow#namespace-expand args)
  (for-each (lambda (ns)
              (snow-namespace-declare (car ns) (cdr ns)))
            args)
  '(begin))

(define (snow-namespace-rename id)
  (let ((id-str (##sys#symbol->qualified-string id)))
    (if (substring-index "#" id-str)
        id
        (let ((pfx
               (let loop ((lst snow-namespace-env))
                 (cond ((null? lst)
                        "")
                       ((or (null? (cdar lst))
                            (memq id (cdar lst)))
                        (caar lst))
                       (else
                        (loop (cdr lst)))))))
          (if (string=? pfx "")
              id
              (string->symbol
               (string-append
                pfx
                (symbol->string id))))))))

(define load
  (let ((orig-load load))
    (lambda (filename)
      (fluid-let ([snow-namespace-env '()]
                  [##sys#alias-global-hook snow-namespace-rename])
        (orig-load filename)))))

(set! ##sys#alias-global-hook snow-namespace-rename)

;;;============================================================================
