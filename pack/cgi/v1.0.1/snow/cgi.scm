;;;============================================================================

;;; File: "cgi.scm", Time-stamp: <2007-04-05 00:50:47 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Contains procedures to support the programming of CGI scripts.

(package* cgi/v1.0.1
 (provide:

  (define (cgi-get-query))
  (define (cgi-send-status status))
  (define (cgi-send-response header content)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Support for CGI scripts.")

 (keywords: net snow)

 (license: lgpl/v2.1)

 (require: hostos/v1)
 (require: homovector/v1)
 (require: mime/v1)
 (require: genport/v1))

;;;============================================================================

; Utilities.

(define (open-stdin)
  (genport-native-input-port->genport (current-input-port)))

(define (close-stdin stdin)
  (genport-close-input-port stdin))

(define (open-stdout)
  (genport-native-output-port->genport (current-output-port)))

(define (close-stdout stdout)
  (genport-close-output-port stdout))

(define (write-u8vector u8vect stdout)
  (genport-write-subu8vector
   u8vect
   0
   (snow-u8vector-length u8vect)
   stdout))

;;;----------------------------------------------------------------------------

(define-macro (max-query-content-length) 0) ;; 0 = no content-length limit

(define (cgi-get-query)
  (let ((request-method
         (snow-getenv "REQUEST_METHOD" "")))
    (cond ((equal? request-method "GET")
           (let* ((qs
                   (snow-getenv "QUERY_STRING" ""))
                  (query-string
                   (snow-ISO-8859-1-string->u8vector qs))
                  (len
                   (snow-u8vector-length query-string))
                  (query
                   (mime-decode-x-www-form-urlencoded query-string 0 len)))
             (if (not query)
                 (cgi-send-status 400) ;; Bad Request
                 query)))
          ((equal? request-method "POST")
           (let* ((content-length
                   (let ((cl (snow-getenv "CONTENT_LENGTH" "")))
                     (or (string->number cl)
                         0)))
                  (content-type
                   (let ((ct (snow-getenv "CONTENT_TYPE" "")))
                     (mime-string->content-type ct))))
             (cond ((not content-length)
                    (cgi-send-status 411)) ;; Length Required
                   ((and (> (max-query-content-length) 0)
                         (> content-length (max-query-content-length)))
                    (cgi-send-status 413)) ;; Request Entity Too Large
                   ((not (and content-type
                              (member (cadr content-type)
                                      '((application . x-www-form-urlencoded)
                                        (multipart . form-data)))))
                    (cgi-send-status 415)) ;; Unsupported Media Type
                   (else
                    (let* ((stdin
                            (open-stdin))
                           (content
                            (snow-make-u8vector content-length))
                           (n
                            (genport-read-subu8vector
                             content
                             0
                             content-length
                             stdin)))
                      (close-stdin stdin)
                      (if (not (= n content-length))
                          (cgi-send-status 400) ;; Bad Request
                          (let ((query
                                 (mime-decode
                                  content
                                  0
                                  content-length
                                  (list content-type))))
                            query)))))))
          (else
           (cgi-send-status 501))))) ;; Not Implemented

(define (cgi-send-status status)
  (let ((header-u8vect
         (snow-ISO-8859-1-string->u8vector
          (string-append "Status: "
                         (number->string status)
                         (mime-eol-str)
                         (mime-eol-str)))))

    (let ((stdout (open-stdout)))
      (write-u8vector header-u8vect stdout)
      (close-stdout stdout))

    #f))

(define (cgi-send-response header content)
  (let ((header-u8vect
         (mime-format-header
          (append header
                  (list (list 'content-length
                              (snow-u8vector-length content)))))))
    (let ((stdout (open-stdout)))
      (write-u8vector header-u8vect stdout)
      (write-u8vector content stdout)
      (close-stdout stdout))))

;;;============================================================================
