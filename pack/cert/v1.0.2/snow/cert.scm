;;;============================================================================

;;; File: "cert.scm", Time-stamp: <2007-04-05 00:50:38 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides procedures to create and manage digital certificates, and
;;; to sign and verify messages.

(package* cert/v1.0.2
 (provide:

  (define (create-certificate-pair purpose))

  (define (certificate-owner cert))

  (define (lookup-certificate owner cert-alist))
  (define (remove-certificate owner cert-alist))

  (define* (certificate= cert1 cert2 (with-signature? _)))

  (define (certs-member cert certs))
  (define (certs-union certs1 certs2))
  (define (certs-intersection certs1 certs2))
  (define (certs-difference certs1 certs2))
  (define (certs-equal? certs1 certs2))

  (define* (certificate->list cert (with-signature? _)))
  (define (list->certificate lst))

  (define (u8vector->cert-alist u8vect))
  (define (cert-alist->u8vector cert-alist))

  (define (cert->base64-string cert))
  (define (base64-string->cert str))
  (define (base64-cert? str))

  (define (write-publ-certs cert-alist filename))
  (define (read-publ-certs filename cont))
  (define (write-priv-certs cert-alist filename password))
  (define (read-priv-certs filename cont))

  (define (create-signature hash publ-priv-sign-cert algorithm))

  (define* (sign-u8vector u8vect publ-priv-sign-cert (algorithm _)))
  (define (verify-u8vector-detailed u8vect signatures cert-alist))
  (define (verify-u8vector u8vect signatures cert-alist))
  (define (filter-certificates-of-class lst classes))

  (define* (sign-certificate cert publ-priv-sign-cert (algorithm _)))
  (define (verify-certificate cert))

  (define* (show-certificate cert (attributes _)))
  (define* (list-certificates certs (attributes _))))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Management of digital certificates, message signing and verification.")

 (keywords: crypto snow)

 (license: lgpl/v2.1)

 (require: ttyui/v1)
 (require: rsa/v1)
 (require: homovector/v1)
 (require: bignum/v1)
 (require: time/v1)
 (require: string/v1)
 (require: list/v1)
 (require: filesys/v1)
 (require: genport/v1)
 (require: cryptio/v1)
 (require: base64/v1)
 (require: digest/v1))

;;;============================================================================

(define-record* validity
  ;; uid: validity-764dcfd7-900d-429d-9e97-11c9f459b9f3
  start
  end)

(define (validity= validity1 validity2)
  (and (bignum= (validity-start validity1)
                (validity-start validity2))
       (bignum= (validity-end validity1)
                (validity-end validity2))))

(define (validity->list validity)
  (list (bignum->base64-string (validity-start validity))
        (bignum->base64-string (validity-end validity))))

(define (list->validity lst)
  (if (not (and (list? lst) (= 2 (length lst))))
      (snow-error "improperly formatted validity period")
      (let* ((start-str (car lst))
             (end-str (cadr lst))
             (start (and (string? start-str) (base64-string->bignum start-str)))
             (end (and (string? end-str) (base64-string->bignum end-str))))
        (if (not (and start end))
            (snow-error "improperly formatted validity period")
            (make-validity start end)))))

;;;----------------------------------------------------------------------------

(define-record* cert
  ;; uid: cert-45402214-4811-47e7-a90d-8a85c36efe23
  owner
  validity
  purpose
  rsa-key
  signature)

(define* (certificate= cert1 cert2 (with-signature? #t))
  (and (string=? (cert-owner cert1)
                 (cert-owner cert2))
       (validity= (cert-validity cert1)
                  (cert-validity cert2))
       (equal? (cert-purpose cert1)
               (cert-purpose cert2))
       (rsa-key= (cert-rsa-key cert1)
                 (cert-rsa-key cert2))
       (if with-signature?
           (equal? (cert-signature cert1)
                   (cert-signature cert2))
           #t)))

(define (certs-member cert certs)
  (let loop ((lst certs))
    (if (pair? lst)
        (if (certificate= cert (car lst) #f)
            lst
            (loop (cdr lst)))
        #f)))

(define (certs-union certs1 certs2)
  (cond ((null? certs1)
         certs2)
        ((certs-member (car certs1) certs2)
         (certs-union (cdr certs1) certs2))
        (else
         (cons (car certs1)
               (certs-union (cdr certs1) certs2)))))

(define (certs-intersection certs1 certs2)
  (cond ((null? certs1)
         '())
        ((certs-member (car certs1) certs2)
         (cons (car certs1)
               (certs-intersection (cdr certs1) certs2)))
        (else
         (certs-intersection (cdr certs1) certs2))))

(define (certs-difference certs1 certs2)
  (cond ((null? certs1)
         '())
        ((certs-member (car certs1) certs2)
         (certs-difference (cdr certs1) certs2))
        (else
         (cons (car certs1)
               (certs-difference (cdr certs1) certs2)))))

(define (certs-equal? certs1 certs2)
  (and (null? (certs-difference certs1 certs2))
       (null? (certs-difference certs2 certs1))))

(define* (certificate->list cert (with-signature? #t))
  (list 'cert-v1
        (cert-owner cert)
        (validity->list (cert-validity cert))
        (cert-purpose cert)
        (rsa-key->list (cert-rsa-key cert))
        (if with-signature? (cert-signature cert) #f)))

(define (list->certificate lst)
  (if (not (and (list? lst) (= 6 (length lst)) (eq? (car lst) 'cert-v1)))
      (snow-error "improperly formatted digital certificate")
      (let* ((owner
              (list-ref lst 1))
             (validity
              (list->validity (list-ref lst 2)))
             (purpose
              (list-ref lst 3))
             (rsa-key
              (list->rsa-key (list-ref lst 4)))
             (signature
              (list-ref lst 5)))
        (if (not (and (string? owner)
                      (list? purpose)
                      (or (not signature)
                          (and (list? signature)
                               (= 4 (length signature))))))
            (snow-error "improperly formatted digital certificate")
            (make-cert
             owner
             validity
             purpose
             rsa-key
             signature)))))

(define (u8vector->cert-alist u8vect)
  (let* ((str (snow-u8vector->ISO-8859-1-string u8vect))
         (x (snow-string->object-list str)))
    (if (not (and (pair? x) (eq? (car x) 'certs-v1)))
        (snow-error "improperly formatted certificate file")
        (map (lambda (cp)
               (cons (list->certificate (car cp))
                     (and (cadr cp) (list->certificate (cadr cp)))))
             (cdr x)))))

(define (cert-alist->u8vector cert-alist)
  (snow-ISO-8859-1-string->u8vector
   (snow-object-list->string
    (cons 'certs-v1
          (map
           (lambda (cp)
             (let ((publ (car cp))
                   (priv (cdr cp)))
               (list (certificate->list publ)
                     (and priv (certificate->list priv)))))
           cert-alist)))))

(define* (cert->u8vector cert (with-signature? #t))
  (let* ((lst (certificate->list cert with-signature?))
         (str (snow-object-list->string lst))
         (u8vect (snow-ISO-8859-1-string->u8vector str)))
    u8vect))

(define (cert->base64-string cert)
  (u8vector->base64-string
   (snow-ISO-8859-1-string->u8vector
    (snow-object-list->string
     (certificate->list cert)))))

(define (base64-string->cert str)
  (list->certificate
   (snow-string->object-list
    (snow-u8vector->ISO-8859-1-string
     (base64-string->u8vector str)))))

(define (base64-cert? str)
  (snow-remove-string-prefix str "Y2Vyd"))

(define (write-publ-certs cert-alist filename)
  (let ((u8vect (cert-alist->u8vector cert-alist)))
    (let ((temp-filename (string-append filename "~")))
      (genport-write-file u8vect temp-filename)
      (snow-rename-file temp-filename filename))))

(define (read-publ-certs filename cont)
  (cont
   (if (snow-file-exists? filename)
       (let ((u8vect (genport-read-file filename)))
         (u8vector->cert-alist u8vect))
       '())
   #f))

(define (get-file-encryption-password filename password)
  (or password
      (begin
        (console-display
         "You do not have a private certificate file.  This file is an encrypted" #\newline
         "file which stores the private certificate(s) which you can use to sign" #\newline
         "the files in Snow packages.  It is encrypted for security, so that only" #\newline
         "you can use the certificate(s)." #\newline
         #\newline
         "You must enter a password for encrypting the private certificate file." #\newline)
        (enter-new-filename-password filename))))

(define (write-priv-certs cert-alist filename password)
  (let* ((u8vect (cert-alist->u8vector cert-alist))
         (password (get-file-encryption-password filename password)))
    (let ((temp-filename (string-append filename "~")))
      (write-encrypted-file u8vect temp-filename password)
      (snow-rename-file temp-filename filename))))

(define (read-priv-certs filename cont)
  (if (snow-file-exists? filename)

      (let* ((password (enter-filename-password filename))
             (u8vect (read-encrypted-file filename password)))
        (if (not u8vect)
            (begin
              (console-display
               "Incorrect password." #\newline)
              #f)
            (cont (u8vector->cert-alist u8vect)
                  password)))

      (cont '()
            #f)))

(define (certificate-owner cert)
  (cert-owner cert))

(define (lookup-certificate owner cert-alist)
  (let loop ((lst cert-alist))
    (if (pair? lst)
        (let* ((x (car lst))
               (c (car x)))
          (if (string=? (cert-owner c) owner)
              x
              (loop (cdr lst))))
        #f)))

(define (remove-certificate owner cert-alist)
  (let loop ((lst cert-alist) (rev-result '()))
    (if (pair? lst)
        (let* ((x (car lst))
               (c (car x)))
          (if (string=? (cert-owner c) owner)
              (loop (cdr lst) rev-result)
              (loop (cdr lst) (cons x rev-result))))
        (reverse rev-result))))

(define root-certificate-alist
  (map (lambda (cert-as-list)
         (let ((cert (list->certificate cert-as-list)))
           (cons cert #f)))
       '(
         (cert-v1
          "Scheme Now! CA <snow at iro.umontreal.ca>"
          ("RdcHVQ==" "WKMKVQ==")
          (snow ca)
          (1024
           "AdUOdLhLCWw0S4uCKNnfVyO4yUvA00ZGYq8pJ6PZdTf6VybvyNnCb7YwI+ryECy2PdHFdHU6hXzAoIKk1+++jN/KWRupIH9oBv8UaxYjWudMJNBUx2AMpSTWqatXlUsUp6/KgtXmDzCFu2qy5EuIz8TxnuApW+UIzctuLK59jj0p"
           "AQAB")
          #f)
         )))

(define (root-certificate? cert)
  (let* ((owner (cert-owner cert))
         (publ-priv (lookup-certificate owner root-certificate-alist)))
    (and publ-priv
         (certificate= (car publ-priv) cert))))

(define (encrypt-hash hash priv-sign-cert)
  (rsa-encrypt-u8vector hash (cert-rsa-key priv-sign-cert) 64))

(define (create-signature hash publ-priv-sign-cert algorithm)
  (let ((publ-sign-cert (car publ-priv-sign-cert))
        (priv-sign-cert (cdr publ-priv-sign-cert)))
    (list (certificate->list publ-sign-cert)
          algorithm
          (snow-u8vector->hex-string hash)
          (snow-u8vector->hex-string (encrypt-hash hash priv-sign-cert)))))

(define* (sign-u8vector u8vect publ-priv-sign-cert (algorithm 'sha-1))
  (let ((hash (digest-u8vector u8vect algorithm 'u8vector)))
    (create-signature hash publ-priv-sign-cert algorithm)))

(define (verify-u8vector-detailed u8vect signatures cert-alist)
  (let loop ((lst signatures)
             (hash-cache '())
             (rev-result '()))
    (if (pair? lst)
        (let* ((signature (car lst))
               (publ-sign-cert-list (list-ref signature 0))
               (algorithm (list-ref signature 1))
               (hash-hex (list-ref signature 2))
               (ehash-hex (list-ref signature 3))
               (publ-sign-cert (list->certificate publ-sign-cert-list))
               (signatory (cert-owner publ-sign-cert))
               (publ-priv (lookup-certificate signatory cert-alist)))
          (cond ((not publ-priv)
                 (loop (cdr lst)
                       hash-cache
                       (cons (cons 'missing-cert signature) rev-result)))
                ((not (certificate= (car publ-priv) publ-sign-cert #f))
                 (loop (cdr lst)
                       hash-cache
                       (cons (cons 'different-cert signature) rev-result)))
                (else
                 (let* ((x
                         (assq algorithm hash-cache))
                        (y
                         (or x
                             (let* ((chash
                                     (digest-u8vector u8vect
                                                      algorithm
                                                      'u8vector))
                                    (chash-hex
                                     (snow-u8vector->hex-string chash)))
                               (list algorithm chash chash-hex))))
                        (chash
                         (cadr y))
                        (chash-hex
                         (caddr y)))
                   (loop (cdr lst)
                         (if x
                             hash-cache
                             (cons y hash-cache))
                         (cons
                          (cons
                           (if (and (string=? hash-hex chash-hex)
                                    (let ((now
                                           (current-time-seconds))
                                          (validity
                                           (cert-validity publ-sign-cert)))
                                      (and (bignum>= now
                                                     (validity-start validity))
                                           (bignum<= now
                                                     (validity-end validity))))
                                    (let* ((ehash
                                            (snow-hex-string->u8vector
                                             ehash-hex))
                                           (rsa-key
                                            (cert-rsa-key publ-sign-cert))
                                           (dhash
                                            (rsa-decrypt-u8vector
                                             ehash
                                             rsa-key))
                                           (dhash-hex
                                            (snow-u8vector->hex-string dhash)))
                                      (equal? chash-hex dhash-hex)))
                               'valid
                               'invalid)
                           signature)
                          rev-result))))))
        (reverse rev-result))))

(define (filter-certificates-of-class lst classes)
  (map (lambda (x) (list->certificate (list-ref (cdr x) 0)))
       (snow-filter (lambda (x) (memq (car x) classes)) lst)))

(define (verify-u8vector u8vect signatures cert-alist)
  (filter-certificates-of-class
   (verify-u8vector-detailed u8vect signatures cert-alist)
   '(valid)))

(define* (sign-certificate cert publ-priv-sign-cert (algorithm 'sha-1))
  (let ((priv-sign-cert (cdr publ-priv-sign-cert)))
    (if (not (memq 'ca (cert-purpose priv-sign-cert)))
        (snow-error "digital certificate can't be signed by"
                    (cert-owner priv-sign-cert))
        (let ((signature
               (sign-u8vector (cert->u8vector cert #f)
                              publ-priv-sign-cert
                              algorithm)))
          (make-cert
           (cert-owner cert)
           (cert-validity cert)
           (cert-purpose cert)
           (cert-rsa-key cert)
           signature)))))

(define (verify-certificate cert)
  (let loop ((c cert) (seen '()))
    (or (root-certificate? c)
        (let ((owner (cert-owner c))
              (signature (cert-signature c)))
          (if (or (not signature)
                  (member owner seen))
              #f
              (let* ((publ-sign-cert
                      (list->certificate (list-ref signature 0)))
                     (x
                      (verify-u8vector
                       (cert->u8vector cert #f)
                       (list signature)
                       (list (cons publ-sign-cert #f)))))
                (if (pair? x)
                    (and (memq 'ca
                               (cert-purpose publ-sign-cert))
                         (memq (list-ref signature 1)
                               '(sha-1 sha-224 sha-256))
                         (loop publ-sign-cert (cons owner seen)))
                    #f)))))))

(define* (show-certificate
          cert
          (attributes '(fingerprint purpose authenticity)))

  (console-display
   "\"" (cert-owner cert) "\"" #\newline)

  (if (memq 'fingerprint attributes)
      (console-display
       "     fingerprint: "
       (digest-u8vector (cert->u8vector cert #f) 'md5 'hex)
       #\newline))

  (if (memq 'purpose attributes)
      (console-display
       "     purpose: "
       (snow-object->string (cert-purpose cert))
       #\newline))

  (if (memq 'authenticity attributes)
      (console-display
       "     authenticity: "
       (if (root-certificate? cert)
           "root certificate"
           (let* ((signature
                   (cert-signature cert))
                  (issuer
                   (and signature
                        (cert-owner (list->certificate (list-ref signature 0))))))
             (cond ((verify-certificate cert)
                    (string-append "issued by \"" issuer "\""))
                   ((not issuer)
                    "*** not signed ***")
                   (else
                    (string-append "*** issued by untrusted \"" issuer "\" ***")))))
       #\newline))

  (if (memq 'ASCII attributes)
      (console-display
       "     ASCII: "
       (cert->base64-string cert)
       #\newline)))

(define* (list-certificates
          certs
          (attributes '(fingerprint purpose authenticity)))
  (let loop ((i 1) (lst certs))
    (if (pair? lst)
        (let* ((x (car lst))
               (publ (car x)))
          (console-display
           (let ((i-str (number->string i)))
             (string-append
              (make-string (max 0 (- 3 (string-length i-str))) #\space)
              i-str
              ") ")))
          (show-certificate publ attributes)
          (loop (+ i 1) (cdr lst))))))

;;;----------------------------------------------------------------------------

(define (enter-certificate-identification)

  (define (proper? str)
    (not (or (snow-string-index str #\")
             (snow-string-index str #\()
             (snow-string-index str #\))
             (snow-string-index str #\<)
             (snow-string-index str #\>))))

  (define (ask-for thing question ok?)
    (let loop ()
      (let ((answer (enter-line-ascii question)))
        (if (not (ok? answer))
            (begin
              (console-display
               "Improper " thing "." #\newline)
              (loop))
            answer))))

  (let loop ()
    (console-display
     "Enter certificate identification information:" #\newline)
    (let* ((name
            (ask-for "name"
                     "Full Name     (e.g. Joe Smith): "
                     proper?))
           (comment
            (ask-for "comment"
                     "Comment       (e.g. secondary): "
                     proper?))
           (email
            (ask-for "email address"
                     "Email Address (e.g. js@foo.us): "
                     (lambda (email)
                       (and (proper? email)
                            (snow-string-index email #\@))))))
      (string-append
       (if (string=? name "")
           ""
           (string-append name " "))
       (if (string=? comment "")
           ""
           (string-append "(" comment ") "))
       (let ((i (snow-string-index email #\@)))
         (string-append
          "<"
          (substring email 0 i)
          " at "
          (substring email (+ i 1) (string-length email))
          ">"))))))

(define (enter-key-size)
  (let loop ()
    (let ((s
           (enter-line-ascii
            "Enter RSA key size in bits (512=default, 1024 or 2048): ")))
      (if (equal? s "")
          512
          (let ((n (string->number s)))
            (if (member n '(512 1024 2048))
                n
                (loop)))))))

(define (generate-rsa-key-pair size)
  (console-display
   "Generating an RSA key pair (this may take a few minutes)..." #\newline)
  (make-rsa-key-pair size #t))

(define (enter-validity)
  (let loop ()
    (let* ((s
            (enter-line-ascii
             "Enter validity period, e.g. 14d (14 days) or 1y (1 year=default): "))
           (x
            (if (equal? s "") "1y" s))
           (time-unit
            (char-downcase
             (string-ref x (- (string-length x) 1))))
           (time-val
            (string->bignum (substring x 0 (- (string-length x) 1)))))
      (if (not (and (memv time-unit '(#\d #\w #\m #\y))
                    time-val
                    (bignum-positive? time-val)))
          (begin
            (console-display
             "Improper validity period." #\newline)
            (loop))
          (let* ((period-in-secs
                  (bignum*
                   time-val
                   (string->bignum
                    (case time-unit
                      ((#\d) "86400")
                      ((#\w) "604800")
                      ((#\m) "2678400")
                      ((#\y) "31536000")))))
                 (start
                  (current-time-seconds))
                 (end
                  (bignum+ start period-in-secs)))
            (make-validity start end))))))

(define (create-certificate-pair purpose)
  (let ((owner (enter-certificate-identification)))
    (and owner
         (let* ((size
                 (enter-key-size))
                (validity
                 (enter-validity))
                (kp
                 (generate-rsa-key-pair size))
                (publ-cert
                 (make-cert
                  owner
                  validity
                  purpose
                  (public-rsa-key kp)
                  #f))
                (priv-cert
                 (make-cert
                  owner
                  validity
                  purpose
                  (private-rsa-key kp)
                  #f)))
           (cons publ-cert
                 priv-cert)))))

;;;============================================================================
