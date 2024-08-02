;;;============================================================================

;;; File: "extio.scm", Time-stamp: <2007-09-03 10:49:22 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides extended I/O.

(package* extio/v1.0.3
 (provide:

  (define* (snow-force-output (port _)))
  (define* (snow-pretty-print obj (port _))))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Extended I/O.")

 (keywords: i/o snow)

 (license: lgpl/v2.1))

;;;============================================================================

;;; System dependencies.

(cond-expand

 ((or bigloo
      larceny
      chez
      sisc
      stklos)

  (define* (snow-force-output (port (current-output-port)))
    (flush-output-port port)))

 ((or gambit
      guile
      kawa
      scheme48
      scm
      scsh)

  (define* (snow-force-output (port (current-output-port)))
    (force-output port)))

 ((or chicken
      mit
      mzscheme)

  (define* (snow-force-output (port (current-output-port)))
    (flush-output port)))

 (gauche

  (define* (snow-force-output (port (current-output-port)))
    (flush port))))

(cond-expand

 ((or bigloo
      mit)

  (define* (snow-pretty-print obj (port (current-output-port)))
    (pp obj port)))

 ((or chez
      chicken
      gambit
      mzscheme
      petite
      scm)

  (define* (snow-pretty-print obj (port (current-output-port)))
    (pretty-print obj port)))

 ((or scheme48
      scsh)

  (define* (snow-pretty-print obj (port (current-output-port)))
    (pretty-print obj port 0)
    (newline port)))

 (stklos

  (define* (snow-pretty-print obj (port (current-output-port)))
    (pretty-print obj port: port)))

 (else

  (define* (snow-pretty-print obj (port (current-output-port)))
    (write obj port)
    (newline port))))

;;;============================================================================
