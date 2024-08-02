;;;============================================================================

;;; File: "hello.scm", Time-stamp: <2007-04-04 17:32:53 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; The universal small example.  A self-contained program that
;; displays "let it snow" and the first few digits of pi.
;;
;; You can run this program like this:
;;
;;   % cd examples
;;   % snowrun -- hello/snow/hello.scm
;;   let it snow
;;   314159265358979323803

(package* hello/v1.0.1

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Display \"let it snow\" and pi.")

 (keywords: example snow)

 (license: lgpl/v2.1)

 (require: pi/v1)) ;; we require the package "pi" which provides
                   ;; the procedure "pi-digits"

;;;============================================================================

(define (hello-world)
  (display "let it snow")
  (newline)
  (display (substring (pi-digits 20) 0 15))
  (newline))

(hello-world)

;;;============================================================================
