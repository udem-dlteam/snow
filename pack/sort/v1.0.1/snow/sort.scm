;;;============================================================================

;;; File: "sort.scm", Time-stamp: <2007-04-05 00:54:06 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides procedures to sort sequences.

(package* sort/v1.0.1
 (provide:

  (define (snow-sort sequence less?)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Sorting procedures.")

 (keywords: data snow)

 (license: lgpl/v2.1))

;;;============================================================================

(define (sort-list lst less?)

  (define (mergesort lst)

    (define (merge lst1 lst2)
      (cond ((null? lst1) lst2)
            ((null? lst2) lst1)
            (else
             (let ((e1 (car lst1)) (e2 (car lst2)))
               (if (less? e1 e2)
                   (cons e1 (merge (cdr lst1) lst2))
                   (cons e2 (merge lst1 (cdr lst2))))))))

    (define (split lst)
      (if (or (null? lst) (null? (cdr lst)))
          lst
          (cons (car lst) (split (cddr lst)))))

    (if (or (null? lst) (null? (cdr lst)))
        lst
        (let* ((lst1 (mergesort (split lst)))
               (lst2 (mergesort (split (cdr lst)))))
          (merge lst1 lst2))))

  (mergesort lst))

(define (snow-sort sequence less?)
  (cond ((or (null? sequence)
             (pair? sequence))
         (sort-list sequence less?))
        ((vector? sequence)
         (list->vector (sort-list (vector->list sequence) less?)))
        (else
         (snow-error "vector or list expected"))))

;;;============================================================================
