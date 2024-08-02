;;;============================================================================

;;; File: "list.scm", Time-stamp: <2007-04-05 00:52:24 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; Provides procedures to operate on lists.

(package* list/v1.0.1
 (provide:

  (define (snow-filter pred lst))
  (define (snow-apply-append lists)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Operations on lists.")

 (keywords: snow)

 (license: lgpl/v2.1))

;;;============================================================================

(define (snow-filter pred lst)

  (define (filt lst)
    (if (null? lst)
        lst
	(let ((head (car lst))
	      (tail (cdr lst)))
	  (if (pred head)
	      (let ((new-tail (filt tail)))
		(if (eq? tail new-tail)
                    lst
		    (cons head new-tail)))
	      (filt tail)))))

  (filt lst))

(define (snow-apply-append lists)

  (define (append-lists lst lists)
    (if (pair? lists)
        (append lst (append-lists (car lists) (cdr lists)))
        lst))

  (if (pair? lists)
      (append-lists (car lists) (cdr lists))
      '()))

;;;============================================================================
