;;;============================================================================

;;; File: "http.scm", Time-stamp: <2007-09-03 13:19:39 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Contains procedures to get HTTP documents.

(package* http/v1.0.2
 (provide:

  (define* (http-get-document hostname document (port-num _)))
  (define* (http-post-query hostname document query (port-num _))))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "HTTP protocol.")

 (keywords: net snow)

 (license: lgpl/v2.1)

 (require: homovector/v1)
 (require: hostos/v1)
 (require: genport/v1)
 (require: tcpip/v1)
 (require: string/v1)
 (require: mime/v1))

;;;============================================================================

;;; Utilities.

(define http-port-num 80)

(define (http-read-line genport-in)
  (let ((buf (snow-make-u8vector 1)))
    (let loop ((lst '()))
      (let* ((n (genport-read-subu8vector buf 0 1 genport-in))
             (x (snow-u8vector-ref buf 0)))
        (if (or (= n 0)
                (= x 10))
            (list->string
             (map integer->char
                  (reverse
                   (if (and (pair? lst) (= (car lst) 13))
                       (cdr lst)
                       lst))))
            (loop (cons x lst)))))))

(define (http-read-header genport-in)
  (let ((buf (snow-make-u8vector 1)))
    (let loop ((lst '()) (state 2))
      (let ((n (genport-read-subu8vector buf 0 1 genport-in)))
        (if (= n 0)
            #f
            (let* ((x
                    (snow-u8vector-ref buf 0))
                   (new-lst
                    (cons x lst))
                   (new-state
                    (if (if (even? state)
                            (= x 13)
                            (= x 10))
                        (+ state 1)
                        0)))
              (if (< new-state 4)
                  (loop new-lst new-state)
                  (let* ((header
                          (snow-ISO-8859-1-string->u8vector
                           (list->string
                            (map integer->char
                                 (reverse new-lst)))))
                         (len
                          (snow-u8vector-length header))
                         (h
                          (mime-parse-header header 0 len)))
                    (and h
                         (= (car h) len)
                         (cdr h))))))))))

(define (http-read-response header-in content-in)
  (let loop1 ()
    (let* ((status (http-read-line header-in))
           (rest (snow-remove-string-prefix status "HTTP/1.1 ")))
      (and rest
           (cond ((snow-remove-string-prefix rest "100 ")
                  (let loop2 ()
                    (if (equal? "" (http-read-line header-in))
                        (loop1)
                        (loop2))))
                 ((snow-remove-string-prefix rest "200 ")
                  (let ((header (http-read-header header-in)))
                    (and header
                         (let* ((x
                                 (assq 'content-length header))
                                (content-length
                                 (if x (cadr x) 0))
                                (content
                                 (snow-make-u8vector content-length))
                                (n
                                 (genport-read-subu8vector
                                  content
                                  0
                                  content-length
                                  content-in)))
                           (if (= n content-length)
                               (cons header
                                     (mime-decode
                                      content
                                      0
                                      content-length
                                      header))
                               #f)))))
                 (else
                  #f))))))

;;; System dependencies.

(cond-expand

 ((or chez
      mit ;; work around bug in MIT-Scheme TCP/IP implementation
      scm
      scsh) ;; work around bug in Scsh's TCP/IP implementation

  (define (http-request method hostname document query port-num)
    (let* ((header-filename
            (snow-make-temp-filename))
           (out-filename
            (snow-make-temp-filename))
           (attribs
            (map (lambda (attrib)
                   (let ((name (car attrib))
                         (val (cdr attrib)))
                     (list name
                           val
                           (if (snow-u8vector? val)
                               (let ((fn (snow-make-temp-filename)))
                                 (genport-write-file val fn)
                                 fn)
                               #f))))
                 (or query '())))
           (command
            (string-append  "curl -s -D "
                            header-filename
                            " -o "
                            out-filename
                            (snow-apply-string-append
                             (map (lambda (attrib)
                                    (string-append " -F "
                                                   (symbol->string (car attrib))
                                                   "="
                                                   (if (caddr attrib)
                                                       (string-append
                                                        "@"
                                                        (caddr attrib))
                                                       (cadr attrib))))
                                  attribs))
                            " \"http://"
                            hostname
                            (if (= 80 port-num)
                                ""
                                (string-append ":"
                                               (number->string port-num)))
                            document
                            "\""))
           (status
            (snow-shell-command command))
           (result
            (if (= status 0)
                (let* ((header (genport-open-input-file header-filename))
                       (out (genport-open-input-file out-filename)))
                  (http-read-response header out))
                #f)))
      (snow-delete-file header-filename)
      (snow-delete-file out-filename)
      (for-each
       (lambda (attrib)
         (let ((fn (caddr attrib)))
           (if fn (snow-delete-file fn))))
       attribs)
      result)))

 (else

  (define (http-request method hostname document query port-num)
    (let* ((use-x-www-form-urlencoded?
            (not (eq? method 'post)))
           (encoded-query
            (if query
                (if use-x-www-form-urlencoded?
                    (cons '(content-type (application . x-www-form-urlencoded))
                          (mime-encode-x-www-form-urlencoded query))
                    (mime-encode-multipart query))
                #f))
           (content
            (if (and query
                     (eq? method 'post))
                (cdr encoded-query)
                (snow-make-u8vector 0)))
           (content-length
            (snow-u8vector-length content))
           (header
            (snow-u8vector-append
             (snow-ISO-8859-1-string->u8vector
              (string-append
               (if (eq? method 'post) "POST " "GET ")
               document
               (if (and encoded-query
                        use-x-www-form-urlencoded?
                        (not (eq? method 'post)))
                   (string-append
                    "?"
                    (snow-u8vector->ISO-8859-1-string (cdr encoded-query)))
                   "")
               " HTTP/1.1" (mime-eol-str)))
             (mime-format-header
              (append
               (list (list #f 'connection " close")
                     (list #f 'host (string-append " " hostname))
                     (list 'content-length content-length))
               (if encoded-query
                   (list (car encoded-query))
                   '())))))
           (header-length
            (snow-u8vector-length header)))

      (tcpip-call-with-tcp-client
       hostname
       port-num
       (lambda (in out send-output)
         (let* ((genport-in (genport-native-input-port->genport in))
                (genport-out (genport-native-output-port->genport out)))

           (genport-write-subu8vector header 0 header-length genport-out)
           (genport-write-subu8vector content 0 content-length genport-out)
           (send-output #t)

           (let ((result (http-read-response genport-in genport-in)))
             (genport-close-input-port genport-in)
             result))))))))

;;; Exported procedures.

(define* (http-get-document hostname document (port-num http-port-num))
  (http-request 'get hostname document #f port-num))

(define* (http-post-query hostname document query (port-num http-port-num))
  (http-request 'post hostname document query port-num))

;;;============================================================================
