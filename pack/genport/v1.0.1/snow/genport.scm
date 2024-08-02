;;;============================================================================

;;; File: "genport.scm", Time-stamp: <2007-04-05 00:51:44 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides a procedural representation of I/O ports.

(package* genport/v1.0.1
 (provide:

  (define-record* genport
    ;; uid: genport-94bfbdf4-4132-40e4-9d0a-35b34c28819a
    end
    read
    write)

  (define (genport-input-port? genport))
  (define (genport-open-input-file filename))
  (define (genport-native-input-port->genport port))
  (define (genport-open-input-subu8vector src-u8vect src-start src-end))
  (define (genport-open-input-u8vector src-u8vect))
  (define (genport-close-input-port genport))
  (define (genport-read-subu8vector u8vect start end genport))
  (define (genport-read-u8vector genport))
  (define (genport-read-file filename))

  (define (genport-output-port? genport))
  (define (genport-open-output-file filename))
  (define (genport-native-output-port->genport port))
  (define (genport-open-output-u8vector))
  (define (genport-get-output-u8vector genport))
  (define (genport-close-output-port genport))
  (define (genport-write-subu8vector u8vect start end genport))
  (define (genport-write-u8vector u8vect genport))
  (define (genport-write-file u8vect filename)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Generic ports.")

 (keywords: i/o snow)

 (license: lgpl/v2.1)

 (require: binio/v1)
 (require: filesys/v1)
 (require: homovector/v1))

;;;============================================================================

(define (genport-input-port? genport)
  (and (genport? genport)
       (procedure? (genport-read genport))))

(define (genport-open-input-file filename)
  (let ((port (binio-open-input-file filename)))
    (make-genport
     (lambda (genport)
       (binio-close-input-port port))
     (lambda (u8vect start end genport)
       (binio-read-subu8vector u8vect start end port))
     #f)))

(define (genport-native-input-port->genport port)
  (make-genport
   (lambda (genport)
     #f)
   (lambda (u8vect start end genport)
     (binio-read-subu8vector u8vect start end port))
   #f))

(define (genport-open-input-subu8vector src-u8vect src-start src-end)
  (let ((pos src-start))
    (make-genport
     (lambda (genport)
       #f)
     (lambda (u8vect start end genport)
       (let ((n (min (- src-end pos) (- end start))))
         (snow-subu8vector-move! src-u8vect pos (+ pos n) u8vect start)
         (set! pos (+ pos n))
         n))
     #f)))

(define (genport-open-input-u8vector src-u8vect)
  (genport-open-input-subu8vector
   src-u8vect
   0
   (snow-u8vector-length src-u8vect)))

(define (genport-close-input-port genport)
  ((genport-end genport) genport))

(define (genport-read-subu8vector u8vect start end genport)
  ((genport-read genport) u8vect start end genport))

(define (genport-read-u8vector genport-in)

  (define block-size 65536)

  (define (read-blocks len)
    (let* ((buf
            (snow-make-u8vector block-size))
           (n
            (genport-read-subu8vector buf 0 block-size genport-in))
           (u8vect
            (if (< n block-size)
                (snow-make-u8vector (+ len n))
                (read-blocks (+ len n)))))
      (snow-subu8vector-move! buf 0 n u8vect len)
      u8vect))

  (read-blocks 0))

(define (genport-read-file filename)
  (let* ((genport-in (genport-open-input-file filename))
         (u8vect (genport-read-u8vector genport-in)))
    (genport-close-input-port genport-in)
    u8vect))

(define (genport-output-port? genport)
  (and (genport? genport)
       (procedure? (genport-write genport))))

(define (genport-open-output-file filename)
  (if (snow-file-exists? filename)
      (snow-delete-file filename))
  (let ((port (binio-open-output-file filename)))
    (make-genport
     (lambda (genport)
       (binio-close-output-port port))
     #f
     (lambda (u8vect start end genport)
       (binio-write-subu8vector u8vect start end port)))))

(define (genport-native-output-port->genport port)
  (make-genport
   (lambda (genport)
     #f)
   #f
   (lambda (u8vect start end genport)
     (binio-write-subu8vector u8vect start end port))))

(define (genport-open-output-u8vector)
  (let* ((bufsize 1024)
         (buf (snow-make-u8vector bufsize 0))
         (pos 0)
         (rev-bufs '()))
    (make-genport
     (lambda (genport)
       (if (not genport)
           (let ((result
                  (snow-apply-u8vector-append
                   (reverse (cons (snow-subu8vector buf 0 pos) rev-bufs)))))
             (set! pos 0)
             (set! rev-bufs '())
             result)
           #f))
     #f
     (lambda (u8vect start end genport)
       (let loop ((i start) (len (- end start)))
         (if (> len 0)
             (if (= pos bufsize)
                 (begin
                   (set! rev-bufs (cons buf rev-bufs))
                   (set! buf (snow-make-u8vector bufsize 0))
                   (set! pos 0)
                   (loop i len))
                 (let* ((n (min len (- bufsize pos)))
                        (new-i (+ i n))
                        (new-pos (+ pos n)))
                   (snow-subu8vector-move! u8vect i new-i buf pos)
                   (set! pos new-pos)
                   (loop new-i (- len n))))
             (- end start)))))))

(define (genport-get-output-u8vector genport)
  ((genport-end genport) #f))

(define (genport-close-output-port genport)
  ((genport-end genport) genport))

(define (genport-write-subu8vector u8vect start end genport)
  ((genport-write genport) u8vect start end genport))

(define (genport-write-u8vector u8vect genport)
  (genport-write-subu8vector u8vect 0 (snow-u8vector-length u8vect) genport))

(define (genport-write-file u8vect filename)
  (let ((genport-out (genport-open-output-file filename)))
    (genport-write-u8vector u8vect genport-out)
    (genport-close-output-port genport-out)))

;;;============================================================================
