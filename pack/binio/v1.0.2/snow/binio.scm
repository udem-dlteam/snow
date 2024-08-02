;;;============================================================================

;;; File: "binio.scm", Time-stamp: <2007-09-03 13:21:13 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides procedures to do binary I/O, including a subset of SRFI 91.

(package* binio/v1.0.2
 (provide:

  (define (binio-input-port? port))
  (define (binio-output-port? port))
  (define (binio-open-input-file filename))
  (define (binio-open-output-file filename))
  (define (binio-close-input-port port))
  (define (binio-close-output-port port))
  (define* (binio-read-byte (port _)))
  (define* (binio-write-byte n (port _)))
  (define* (binio-read-subu8vector u8vect start end (port _)))
  (define* (binio-write-subu8vector u8vect start end (port _))))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Binary I/O.")

 (keywords: i/o snow)

 (license: lgpl/v2.1)

 (require: homovector/v1))

;;;============================================================================

;;; System dependencies.

(cond-expand

 (srfi-91

  (define binio-read-byte read-byte)
  (define binio-write-byte write-byte))

 (gambit

  (define binio-read-byte read-u8)
  (define binio-write-byte write-u8))

 (gauche

  (use binary.io)

  (define binio-read-byte read-binary-uint8)
  (define binio-write-byte write-binary-uint8))

 (mzscheme

  (define binio-read-byte read-byte)
  (define binio-write-byte write-byte))

 (else

  (define* (binio-read-byte (port (current-input-port)))
    (let ((c (read-char port))) ;; assumes ISO-8859-1 char encoding
      (if (eof-object? c)
          c
          (char->integer c)))) ;; assumes Unicode encoding

  (define* (binio-write-byte n (port (current-output-port)))
    (write-char        ;; assumes ISO-8859-1 char encoding
     (integer->char n) ;; assumes Unicode encoding
     port))))

(cond-expand

 ((or srfi-91
      gambit)

  (define binio-read-subu8vector read-subu8vector)
  (define binio-write-subu8vector write-subu8vector))

 (else

  (define* (binio-read-subu8vector
            u8vect
            start
            end
            (port (current-input-port)))
    (let loop ((i start))
      (if (< i end)
          (let ((n (binio-read-byte port)))
            (if (eof-object? n)
                (- i start)
                (begin
                  (snow-u8vector-set! u8vect i n)
                  (loop (+ i 1)))))
          (- i start))))

  (define* (binio-write-subu8vector
            u8vect
            start
            end
            (port (current-output-port)))
    (let loop ((i start))
      (if (< i end)
          (begin
            (binio-write-byte (snow-u8vector-ref u8vect i) port)
            (loop (+ i 1)))
          (- i start))))))

(cond-expand

 (bigloo

  (define (binio-open-input-file filename)
    (let ((p (open-input-file filename)))
      (if (input-port? p)
          p
          (raise (instantiate::&io-port-error
                  (proc 'binio-open-input-file)
                  (msg "Cannot open file for input")
                  (obj filename))))))

  (define (binio-open-output-file filename)
    (let ((p (open-output-file filename)))
      (if (output-port? p)
          p
          (raise (instantiate::&io-port-error
                  (proc 'binio-open-input-file)
                  (msg "Cannot open file for output")
                  (obj filename)))))))

 (kawa

  (define (binio-open-input-file filename)
    (fluid-let ((port-char-encoding #f))
      (open-input-file filename)))

  (define (binio-open-output-file filename)
    (fluid-let ((port-char-encoding #f))
      (open-output-file filename))))

 (sisc

  (define (binio-open-input-file filename)
    (open-input-file filename "8859_1"))

  (define (binio-open-output-file filename)
    (open-output-file filename "8859_1")))

 (else

  (define (binio-open-input-file filename)
    (open-input-file filename))

  (define (binio-open-output-file filename)
    (open-output-file filename))))

;;;----------------------------------------------------------------------------

(define (binio-input-port? port)
  (input-port? port))

(define (binio-output-port? port)
  (output-port? port))

(define (binio-close-input-port port)
  (close-input-port port))

(define (binio-close-output-port port)
  (close-output-port port))

;;;============================================================================
