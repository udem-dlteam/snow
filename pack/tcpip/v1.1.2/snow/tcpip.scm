;;;============================================================================

;;; File: "tcpip.scm", Time-stamp: <2007-09-03 13:18:23 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides procedures to open TCP/IP client connections.

(package* tcpip/v1.1.2
 (provide:

  (define (tcpip-call-with-tcp-client hostname port-num proc))
  (define (tcpip-listener-open port-num))
  (define (tcpip-listener-close listener))
  (define* (tcpip-accept-tcp-client listener (timeout _)))
  (define (tcpip-tcp-client-input-port tcp-client))
  (define (tcpip-tcp-client-output-port tcp-client))
  (define (tcpip-tcp-client-close tcp-client)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Support for TCP/IP networking.")

 (keywords: net snow)

 (license: lgpl/v2.1))

;;;============================================================================

;;; System dependencies.

(cond-expand

 (bigloo

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (let* ((socket
            (make-client-socket hostname port-num))
           (in
            (socket-input socket))
           (out
            (socket-output socket))
           (result
            (proc in
                  out
                  (lambda (end?)
                    (if end?
                        (close-output-port out))))))
      (close-input-port in)
      (close-output-port out)
      (socket-shutdown socket)
      result))

  (define (tcpip-listener-open port-num)
    (make-server-socket port-num))

  (define (tcpip-listener-close listener)
    (socket-shutdown listener))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (if (not timeout)
        (socket-accept listener)
        (snow-error "nonblocking tcpip-accept is not implemented")))

  (define (tcpip-tcp-client-input-port tcp-client)
    (socket-input tcp-client))

  (define (tcpip-tcp-client-output-port tcp-client)
    (socket-output tcp-client))

  (define (tcpip-tcp-client-close tcp-client)
    (socket-shutdown tcp-client #f)))

 (chez

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (snow-error "tcpip-call-with-tcp-client is not implemented"))

  (define (tcpip-listener-open port-num)
    (snow-error "tcpip-listener-open is not implemented"))

  (define (tcpip-listener-close listener)
    (snow-error "tcpip-listener-close is not implemented"))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (snow-error "tcpip-accept is not implemented"))

  (define (tcpip-tcp-client-input-port tcp-client)
    (snow-error "tcpip-tcp-client-input-port is not implemented"))

  (define (tcpip-tcp-client-output-port tcp-client)
    (snow-error "tcpip-tcp-client-output-port is not implemented"))

  (define (tcpip-tcp-client-close tcp-client)
    (snow-error "tcpip-tcp-client-close is not implemented")))

 (chicken

  (use tcp)

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (call-with-values
        (lambda () (tcp-connect hostname port-num))
      (lambda (in out)
        (let ((result
               (proc in
                     out
                     (lambda (end?)
                       (if end?
                           (close-output-port out))))))
          (close-input-port in)
          (close-output-port out)
          result))))

  (define (tcpip-listener-open port-num)
    (tcp-listen port-num))

  (define (tcpip-listener-close listener)
    (tcp-close listener))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (if (or (not timeout)
            (tcp-accept-ready? listener))
        (call-with-values
            (lambda () (tcp-accept listener))
          (lambda (in out) (cons in out)))
        #f))

  (define (tcpip-tcp-client-input-port tcp-client)
    (car tcp-client))

  (define (tcpip-tcp-client-output-port tcp-client)
    (cdr tcp-client))

  (define (tcpip-tcp-client-close tcp-client)
    (close-input-port (tcpip-tcp-client-input-port tcp-client))
    (close-output-port (tcpip-tcp-client-output-port tcp-client))))

 (gambit

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (let* ((port
            (open-tcp-client
             (list server-address: hostname
                   port-number: port-num)))
           (result
            (proc port
                  port
                  (lambda (end?)
                    (if end?
                        (close-output-port port)
                        (force-output port))))))
      (close-port port)
      result))

  (define (tcpip-listener-open port-num)
    (open-tcp-server (list port-number: port-num
                           reuse-address: #t)))

  (define (tcpip-listener-close listener)
    (close-port listener))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (input-port-timeout-set! listener (or timeout (/ 0.0)))
    (let ((port (read listener)))
      (if (eof-object? port)
          #f
          port)))

  (define (tcpip-tcp-client-input-port tcp-client)
    tcp-client)

  (define (tcpip-tcp-client-output-port tcp-client)
    tcp-client)

  (define (tcpip-tcp-client-close tcp-client)
    (close-port tcp-client)))

 (gauche

  (use gauche.net)

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (let* ((socket
            (make-client-socket hostname port-num))
           (in
            (socket-input-port socket))
           (out
            (socket-output-port socket))
           (result
            (proc in
                  out
                  (lambda (end?)
                    (if end?
                        (close-output-port out))))))
      (close-input-port in)
      (close-output-port out)
      (socket-shutdown socket)
      result))

  (define (tcpip-listener-open port-num)
    (make-server-socket 'inet port-num :reuse-addr? #t))

  (define (tcpip-listener-close listener)
    (socket-close listener))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (if (not timeout)
        (socket-accept listener)
        (let ((r (make <sys-fdset>))
              (w (make <sys-fdset>))
              (e (make <sys-fdset>)))
          (set! (sys-fdset-ref r (socket-fd listener)) #t)
          (if (= 0 (sys-select r w e 0))
              #f
              (socket-accept listener)))))

  (define (tcpip-tcp-client-input-port tcp-client)
    (socket-input-port tcp-client))

  (define (tcpip-tcp-client-output-port tcp-client)
    (socket-output-port tcp-client))

  (define (tcpip-tcp-client-close tcp-client)
    (socket-shutdown tcp-client)))

 (guile

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (let* ((hostent
            (gethostbyname hostname))
           (ip-address
            (car (vector-ref hostent 4)))
           (socket
            (socket AF_INET SOCK_STREAM 0))
           (dummy
            (connect socket AF_INET ip-address port-num))
           (result
            (proc socket
                  socket
                  (lambda (end?)
                    (if end?
                        (shutdown socket 1))))))
      (close-port socket)
      result))

  (define (tcpip-listener-open port-num)
    (snow-error "tcpip-listener-open is not implemented"))

  (define (tcpip-listener-close listener)
    (snow-error "tcpip-listener-close is not implemented"))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (snow-error "tcpip-accept is not implemented"))

  (define (tcpip-tcp-client-input-port tcp-client)
    (snow-error "tcpip-tcp-client-input-port is not implemented"))

  (define (tcpip-tcp-client-output-port tcp-client)
    (snow-error "tcpip-tcp-client-output-port is not implemented"))

  (define (tcpip-tcp-client-close tcp-client)
    (snow-error "tcpip-tcp-client-close is not implemented")))

 (kawa

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (let* ((socket
            (make <java.net.Socket> (as <String> hostname) (as <int> port-num)))
           (in
            (make <java.io.InputStreamReader>
              (invoke socket 'getInputStream)
              "8859_1"))
           (out
            (make <gnu.mapping.OutPort>
              (make <java.io.OutputStreamWriter>
                (invoke socket 'getOutputStream)
                "8859_1")))
           (result
            (proc in
                  out
                  (lambda (end?)
                    (if end?
                        (begin
                          (force-output out)
                          (invoke socket 'shutdownOutput)))))))
      (invoke socket 'close)
      result))

  (define (tcpip-listener-open port-num)
    (snow-error "tcpip-listener-open is not implemented"))

  (define (tcpip-listener-close listener)
    (snow-error "tcpip-listener-close is not implemented"))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (snow-error "tcpip-accept is not implemented"))

  (define (tcpip-tcp-client-input-port tcp-client)
    (snow-error "tcpip-tcp-client-input-port is not implemented"))

  (define (tcpip-tcp-client-output-port tcp-client)
    (snow-error "tcpip-tcp-client-output-port is not implemented"))

  (define (tcpip-tcp-client-close tcp-client)
    (snow-error "tcpip-tcp-client-close is not implemented")))

 (larceny

  (require 'socket)

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (let* ((socket
            (make-client-socket hostname port-num))
           (in
            (socket-input-port socket))
           (out
            (socket-output-port socket))
           (result
            (proc in 
                  out 
                  (lambda (end?) 
                    (flush-output-port out)
                    (if end? 
                        (close-output-port out))))))
      (close-input-port in)
      (close-output-port out)
      result))

  (define (tcpip-listener-open port-num)
    (make-server-socket port-num))

  (define (tcpip-listener-close listener)
    ;; TODO: find how to close server sockets
    #f)

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (if (not timeout)
        (server-socket-accept listener)
        (snow-error "nonblocking tcpip-accept is not implemented")))

  (define (tcpip-tcp-client-input-port tcp-client)
    (socket-input-port tcp-client))

  (define (tcpip-tcp-client-output-port tcp-client)
    (socket-output-port tcp-client))

  (define (tcpip-tcp-client-close tcp-client)
    (close-input-port (tcpip-tcp-client-input-port tcp-client))
    (close-output-port (tcpip-tcp-client-output-port tcp-client))))

 (mit

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (let* ((port
            (open-tcp-stream-socket hostname port-num))
           (result
            (proc port
                  port
                  (lambda (end?)
                    (flush-output port)
                    (if end? 
                        (close-output-port port))))))
      (close-port port)
      result))

  (define (tcpip-listener-open port-num)
    (open-tcp-server-socket port-num))

  (define (tcpip-listener-close listener)
    (close-tcp-server-socket listener))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (if (or (not timeout)
            (eq? (test-for-io-on-channel listener 'READ #f) 'READ))
        (tcp-server-connection-accept listener #t #f)
        #f))

  (define (tcpip-tcp-client-input-port tcp-client)
    tcp-client)

  (define (tcpip-tcp-client-output-port tcp-client)
    tcp-client)

  (define (tcpip-tcp-client-close tcp-client)
    (close-port tcp-client)))

 (mzscheme

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (call-with-values
        (lambda () (tcp-connect hostname port-num))
      (lambda (in out)
        (let ((result
               (proc in
                     out
                     (lambda (end?)
                       (if end?
                           (close-output-port out))))))
          (close-input-port in)
          (close-output-port out)
          result))))

  (define (tcpip-listener-open port-num)
    (tcp-listen port-num 100 #t))

  (define (tcpip-listener-close listener)
    (tcp-close listener))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (if (or (not timeout)
            (tcp-accept-ready? listener))
        (call-with-values
            (lambda () (tcp-accept listener))
          (lambda (in out) (cons in out)))
        #f))

  (define (tcpip-tcp-client-input-port tcp-client)
    (car tcp-client))

  (define (tcpip-tcp-client-output-port tcp-client)
    (cdr tcp-client))

  (define (tcpip-tcp-client-close tcp-client)
    (close-input-port (tcpip-tcp-client-input-port tcp-client))
    (close-output-port (tcpip-tcp-client-output-port tcp-client))))

 (scheme48

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (call-with-values
        (lambda () (socket-client hostname port-num))
      (lambda (in out)
        (let ((result
               (proc in
                     out
                     (lambda (end?)
                       (if end?
                           (close-output-port out))))))
          (close-input-port in)
          (close-output-port out)
          result))))

  (define (tcpip-listener-open port-num)
    (open-socket port-num))

  (define (tcpip-listener-close listener)
    (close-socket listener))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (if (not timeout)
        (call-with-values
            (lambda () (socket-accept listener))
          (lambda (in out) (cons in out)))
        (snow-error "nonblocking tcpip-accept is not implemented")))

  (define (tcpip-tcp-client-input-port tcp-client)
    (car tcp-client))

  (define (tcpip-tcp-client-output-port tcp-client)
    (cdr tcp-client))

  (define (tcpip-tcp-client-close tcp-client)
    (close-input-port (tcpip-tcp-client-input-port tcp-client))
    (close-output-port (tcpip-tcp-client-output-port tcp-client))))

 (scm

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (snow-error "tcpip-call-with-tcp-client is not implemented"))

  (define (tcpip-listener-open port-num)
    (snow-error "tcpip-listener-open is not implemented"))

  (define (tcpip-listener-close listener)
    (snow-error "tcpip-listener-close is not implemented"))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (snow-error "tcpip-accept is not implemented"))

  (define (tcpip-tcp-client-input-port tcp-client)
    (snow-error "tcpip-tcp-client-input-port is not implemented"))

  (define (tcpip-tcp-client-output-port tcp-client)
    (snow-error "tcpip-tcp-client-output-port is not implemented"))

  (define (tcpip-tcp-client-close tcp-client)
    (snow-error "tcpip-tcp-client-close is not implemented")))

 (scsh

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (call-with-values
        (lambda () (socket-client hostname port-num))
      (lambda (in out)
        (let ((result
               (proc in
                     out
                     (lambda (end?)
                       (if end?
                           (close-output-port out))))))
          (close-input-port in)
          (close-output-port out)
          result))))

  (define (tcpip-listener-open port-num)
    (open-socket port-num))

  (define (tcpip-listener-close listener)
    (close-socket listener))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (if (not timeout)
        (call-with-values
            (lambda () (socket-accept listener))
          (lambda (in out) (cons in out)))
        (snow-error "nonblocking tcpip-accept is not implemented")))

  (define (tcpip-tcp-client-input-port tcp-client)
    (car tcp-client))

  (define (tcpip-tcp-client-output-port tcp-client)
    (cdr tcp-client))

  (define (tcpip-tcp-client-close tcp-client)
    (close-input-port (tcpip-tcp-client-input-port tcp-client))
    (close-output-port (tcpip-tcp-client-output-port tcp-client))))

 (sisc

  (import networking)

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (let* ((socket
            (open-tcp-socket hostname port-num))
           (in
            (open-socket-input-port socket "8859_1"))
           (out
            (open-socket-output-port socket "8859_1"))
           (result
            (proc in
                  out
                  (lambda (end?)
                    (flush-output-port out)))))
      (close-socket socket)
      result))

  (define (tcpip-listener-open port-num)
    (open-tcp-listener port-num))

  (define (tcpip-listener-close listener)
    (close-socket listener))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
;;;    (set-so-timeout! listener (if timeout 0 9999999));;;;;;;;;;;
    (accept-tcp-socket listener))

  (define (tcpip-tcp-client-input-port tcp-client)
    (open-socket-input-port tcp-client "8859_1"))

  (define (tcpip-tcp-client-output-port tcp-client)
    (open-socket-output-port tcp-client "8859_1"))

  (define (tcpip-tcp-client-close tcp-client)
    (close-socket tcp-client)))

 (stalin

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (snow-error "tcpip-call-with-tcp-client is not implemented"))

  (define (tcpip-listener-open port-num)
    (snow-error "tcpip-listener-open is not implemented"))

  (define (tcpip-listener-close listener)
    (snow-error "tcpip-listener-close is not implemented"))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (snow-error "tcpip-accept is not implemented"))

  (define (tcpip-tcp-client-input-port tcp-client)
    (snow-error "tcpip-tcp-client-input-port is not implemented"))

  (define (tcpip-tcp-client-output-port tcp-client)
    (snow-error "tcpip-tcp-client-output-port is not implemented"))

  (define (tcpip-tcp-client-close tcp-client)
    (snow-error "tcpip-tcp-client-close is not implemented")))

 (stklos

  (define (tcpip-call-with-tcp-client hostname port-num proc)
    (let* ((socket
            (make-client-socket hostname port-num))
           (in
            (socket-input socket))
           (out
            (socket-output socket))
           (result
            (proc in
                  out
                  (lambda (end?)
                    (if end?
                        (close-output-port out))))))
      (close-input-port in)
      (close-output-port out)
      (socket-shutdown socket)
      result))

  (define (tcpip-listener-open port-num)
    (make-server-socket port-num))

  (define (tcpip-listener-close listener)
    (socket-shutdown listener))

  (define* (tcpip-accept-tcp-client listener (timeout #f))
    (if (not timeout)
        (socket-accept listener)
        (snow-error "nonblocking tcpip-accept is not implemented")))

  (define (tcpip-tcp-client-input-port tcp-client)
    (socket-input tcp-client))

  (define (tcpip-tcp-client-output-port tcp-client)
    (socket-output tcp-client))

  (define (tcpip-tcp-client-close tcp-client)
    (socket-shutdown tcp-client #f))))

;;;============================================================================
