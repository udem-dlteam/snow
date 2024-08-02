;;;============================================================================

;;; File: "snowman-app.scm", Time-stamp: <2007-05-09 09:13:02 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Package manager for the Scheme Now! framework.

(package* snowman-app/v1.0.2
 (provide:

  (define (snowman-main)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Generic Snow package management utility.")

 (keywords: snow)

 (license: lgpl/v2.1)

 (require: hostos/v1)
 (require: base64/v1)
 (require: filesys/v1)
 (require: genport/v1)
 (require: homovector/v1)
 (require: digest/v1)
 (require: cryptio/v1)
 (require: cert/v1)
 (require: extio/v1)
 (require: ttyui/v1)
 (require: http/v1)
 (require: cgi/v1)
 (require: mime/v1)
 (require: string/v1)
 (require: tar/v1) 
 (require: zlib/v1) 
 (require: list/v1) 
 (require: sort/v1) 
 (require: snowfort-app/v1))

;;;============================================================================

(define (snowman-usage)
  (console-display
"Usage:

  % snowman list                     <- list packages available on snowfort

  % snowman install foo              <- install highest version of foo
  % snowman install foo/v2           <- install highest revision of foo/v2
  % snowman install foo bar baz      <- install packages foo, bar and baz
  % snowman install xyz/foo.tgz      <- install snowball stored in a file

  % snowman download foo             <- download highest version of foo

  % snowman upload xyz/foo/v2.0.0    <- upload .tgz of revision 2.0.0 of foo
  % snowman upload xyz/foo           <- upload .tgz of highest version of foo
  % snowman upload xyz/foo.tgz       <- upload snowball stored in a file

  % snowman cert-passwd              <- change certificate file password
  % snowman cert-create              <- create new certificate for signing
  % snowman cert-export [<file>]     <- save/display certificate (in ASCII)
  % snowman cert-import <file|cert>* <- add/replace certificate (from ASCII)
  % snowman cert-remove              <- remove certificate
  % snowman cert-list                <- list certificates

  % snowman verify <file|cert>*      <- verify regular files or certificates

  % snowman sign <file|cert>*        <- sign regular files or certificates

  Sets of suboptions (first one of each choice set is the default):
           list: none
        install: --verify|--skip-verify --highest|--exact --user|--site
       download: --verify|--skip-verify --highest|--exact --user|--site
         upload: none
    cert-passwd: none
    cert-create: none
    cert-export: --user|--site|--sign
    cert-import: --user|--site|--sign
    cert-remove: --user|--site|--sign
      cert-list: --user|--site|--sign
         verify: --user|--site
           sign: --sha-1|--sha-224|--sha-256
")
  (snow-exit 70))

;;;----------------------------------------------------------------------------

(define (process-options options allowed)

  (let ((all-allowed
         (snow-apply-append allowed)))
    (for-each
     (lambda (opt)
       (if (not (memq opt all-allowed))
           (begin
             (console-display
              (string-append
               "Invalid command line option: --"
               (snow-object->string opt))
              #\newline)
             (snowman-usage))))
     options))

  (map (lambda (set)
         (let ((present
                (snow-apply-append
                 (map (lambda (sym)
                        (if (memq sym options) (list sym) '()))
                      set))))
           (cond ((null? present)
                  (car set))
                 ((null? (cdr present))
                  (car present))
                 (else
                  (console-display
                   (string-append
                    "Conflicting command line options:"
                    (snow-apply-string-append
                     (map (lambda (opt)
                            (string-append " --"
                                           (snow-object->string opt)))
                          present)))
                   #\newline)
                  (snowman-usage)))))
       allowed))

;;;----------------------------------------------------------------------------

(define (priv-certs-file dir) (snow-make-filename dir ".priv-certs"))
(define (publ-certs-file dir) (snow-make-filename dir ".publ-certs"))

(define (access-cert-file opts cont)

  (define (return filename reader writer)
    (cont filename
          (lambda (cont)
            (reader filename cont))
          (lambda (cert-alist password)
            (writer cert-alist filename password))))

  (if (memq 'sign opts)
      (return (priv-certs-file (snow-user-root))
              (lambda (filename cont)
                (or (read-priv-certs filename cont)
                    (snow-exit 1)))
              (lambda (cert-alist filename password)
                (snow-create-directory-recursive
                 (snow-filename-directory filename))
                (write-priv-certs cert-alist filename password)))
      (return (publ-certs-file (if (memq 'user opts)
                                   (snow-user-root)
                                   (snow-site-root)))
              (lambda (filename cont)
                (read-publ-certs filename cont))
              (lambda (cert-alist filename password)
                (snow-create-directory-recursive
                 (snow-filename-directory filename))
                (write-publ-certs cert-alist filename)))))

(define (read-certs-of-kind kinds)

  (define (read-certs kind)
    (access-cert-file
     (list kind)
     (lambda (filename read-cert-alist write-cert-alist)
       (read-cert-alist
        (lambda (cert-alist password)
          cert-alist)))))

  (snow-apply-append (map read-certs kinds)))

(define (read-certs-of-kind-and-snow kinds)
  (append
   (snow-certificate-alist)
   (read-certs-of-kind kinds)))

(define (read-all-certs)
  (read-certs-of-kind-and-snow '(user site)))

(define* (list-certs
          cert-alist
          (attributes '(fingerprint purpose authenticity)))
  (list-certificates cert-alist attributes))

(define* (select-cert
          cert-alist
          reason
          (attributes '(fingerprint purpose authenticity)))

  (define (select x)
    (let ((publ (car x)))
      (console-display
       "The certificate \""
       (certificate-owner publ)
       "\" will be "
       reason
       "."
       #\newline)
      x))

  (let ((n (length cert-alist)))
    (cond ((= n 0)
           (console-display
            "The certificate file is missing or empty.  Sorry!" #\newline)
           (snow-exit 1))
          ((= n 1)
           (select (car cert-alist)))
          (else
           (console-display
            "The certificate file contains these certificates:" #\newline)
           (list-certs cert-alist attributes)
           (let loop ()
             (let* ((line
                     (enter-line-ascii
                      (string-append
                       "Please select certificate to be " reason ": ")))
                    (num
                     (string->number line)))
               (if (not (and num
                             (integer? num)
                             (exact? num)
                             (>= num 1)
                             (<= num n)))
                   (begin
                     (console-display
                      "Improper selection." #\newline)
                     (loop))
                   (select (list-ref cert-alist (- num 1))))))))))

;;;----------------------------------------------------------------------------

(define (get-pkg-list-from-snowfort)
  (let ((response
         (http-post-query
          (snowfort-hostname)
          (snowfort-path)
          (list (cons 'operation "list"))
          (snowfort-port-num))))
    (if (and (pair? response)
             (snow-u8vector? (cdr response)))
        (map (lambda (x)
               (make-pkg
                (car x)
                (map (lambda (package-form)
                       (let ((vpkg (package-form->vpkg package-form)))
                         (if (not vpkg)
                             (begin
                               (console-display
                                "Package form syntax error." #\newline)
                               (snow-exit 1))
                             vpkg)))
                     (cdr x))))
             (snow-string->object-list
              (snow-u8vector->ISO-8859-1-string
               (cdr response))))
        (begin
          (console-display
           "Can't get package list from snowfort." #\newline)
          (snow-exit 1)))))

(define (lookup-pkg package-name pkgs)
  (let loop ((lst pkgs))
    (if (pair? lst)
        (let ((pkg (car lst)))
          (if (equal? package-name (pkg-name pkg))
              pkg
              (loop (cdr lst))))
        #f)))

(define (lookup-vpkg version vpkgs)
  (let loop ((lst vpkgs))
    (if (pair? lst)
        (let ((vpkg (car lst)))
          (if (eq? '= (compare-versions version (vpkg-version vpkg)))
              vpkg
              (loop (cdr lst))))
        #f)))

(define (filter-acceptable-vpkgs vpkgs req-version)
  (let loop ((lst vpkgs) (rev-result '()))
    (if (pair? lst)
        (let ((vpkg (car lst)))
          (loop (cdr lst)
                (if (compatible-version-with (vpkg-version vpkg) req-version)
                    (cons vpkg rev-result)
                    rev-result)))
        (reverse rev-result))))

;;;----------------------------------------------------------------------------

(define (snowman-list options args)
  (let ((opts
         (process-options
          options
          '((list)))))
    (if (not (null? args))
        (snowman-usage)
        (let ((snowfort-pkgs (get-pkg-list-from-snowfort)))
          (for-each
           (lambda (pkg)
             (let ((versions (pkg-versions pkg)))
               (if (pair? versions)
                   (let* ((vpkg (car versions))
                          (name (vpkg-name vpkg))
                          (version (vpkg-version vpkg))
                          (description (or (vpkg-description vpkg) "---")))
                     (console-display
                      name "/" (version->string version ".") ": "
                      description #\newline)))))
           snowfort-pkgs)))))

;;;----------------------------------------------------------------------------

(define (get-snowball-from-snowfort package-name version)
  (let ((response
         (http-post-query
          (snowfort-hostname)
          (snowfort-path)
          (list (cons 'operation
                      "download")
                (cons 'pkg
                      (string-append package-name
                                     "/"
                                     (version->string version "."))))
          (snowfort-port-num))))
    (if (and (pair? response)
             (snow-u8vector? (cdr response)))
        (cdr response)
        (begin
          (console-display
           "Can't get snowball "
           (make-snowball-filename package-name version)
           " from snowfort." #\newline)
          (snow-exit 1)))))

(define (save-package dir
                      name
                      version
                      tar-rec-list
                      fail
                      cont)

  (define (create-dir dir)
    (snow-with-exception-catcher
     (lambda (e)
       #f)
     (lambda ()
       (snow-create-directory dir)
       #t)))

  (define (create-files dir tar-rec-list)
    (for-each
     (lambda (tr)
       (let* ((name (tar-rec-name tr))
              (filename (snow-make-filename dir name)))
         (snow-create-directory-recursive
          (snow-filename-directory filename))
         (if (eq? (tar-rec-type tr) 'directory)
             (snow-create-directory-recursive filename)
             (genport-write-file (tar-rec-content tr) filename))))
     tar-rec-list))

  (define (delete-files dir tar-rec-list)
    (for-each
     (lambda (tr)
       (let* ((name (tar-rec-name tr))
              (filename (snow-make-filename dir name)))
         (if (eq? (tar-rec-type tr) 'directory)
             (snow-delete-directory filename)
             (snow-delete-file filename))))
     (reverse tar-rec-list)))

  (define (run-install-script dir)
    (let* ((snow-dir
            (snow-make-filename dir "snow"))
           (install-script-filename-windows
            (snow-make-filename snow-dir "install-script.bat"))
           (install-script-filename-unix
            (snow-make-filename snow-dir "install-script.sh"))
           (status
            (cond ((snow-getenv "USERPROFILE" #f) ;; is this Windows?
                   (if (snow-file-exists? install-script-filename-windows)
                       (snow-shell-command
                        (string-append
                         "cd \"" snow-dir
                         "\" && \"" install-script-filename-windows "\""))
                       0))
                  ((snow-getenv "HOME" #f) ;; is this Unix?
                   (if (snow-file-exists? install-script-filename-unix)
                       (snow-shell-command
                        (string-append
                         "cd \"" snow-dir
                         "\" && chmod +x \"" install-script-filename-unix
                         "\" && \"" install-script-filename-unix "\""))
                       0))
                  (else
                   0))))
      (= status 0)))

  (let* ((package-name-with-version
          (string-append name "/" (version->string version ".")))
         (pkg-dir
          (snow-make-filename dir name))
         (vers-dir
          (snow-make-filename pkg-dir (version->string version ".")))
         (disable-dir
          (snow-make-filename vers-dir ".disable"))
         (new-tar-rec-list
          (cons (car tar-rec-list)
                (cons (make-tar-rec
                       (string-append
                        (string-append name
                                       "/"
                                       (version->string version ".")
                                       "/.disable"))
                       #f #f #f #f 'directory
                       #f #f #f #f #f #f #f #f)
                      (cdr tar-rec-list)))))

    (snow-create-directory-recursive pkg-dir)
    (if (not (create-dir vers-dir))
        (cont (string-append
               "Installation of " package-name-with-version " skipped because it is already installed."))
        (begin
          (create-files dir new-tar-rec-list)
          (if (not (run-install-script vers-dir))
              (begin
                (delete-files dir new-tar-rec-list)
                (fail (string-append
                       "Package " package-name-with-version " install script failed.")))
              (begin
                (snow-delete-directory disable-dir)
                (cont (string-append
                       "Package " package-name-with-version " installed."))))))))

(define (explain-verification-failure x)
  (let ((certs
         (map (lambda (c) (cons c #f)) (vector-ref x 2))))
    (if (not (null? certs))
        (begin
          (console-display
           "The verification may succeed if one of the following certificates is imported:" #\newline)
          (list-certificates
           certs
           '(fingerprint purpose authenticity ASCII))))))

(define (snowman-install-or-download-packages opts args cert-alist)
  (let ((snowfort-pkgs (get-pkg-list-from-snowfort)))

    (define (invalid-package-name package-name)
      (console-display
       package-name " is an invalid package name." #\newline)
      (snow-exit 1))

    (define (package-does-not-exist package-name)
      (console-display
       "Snowfort does not contain package " package-name "." #\newline)
      (snow-exit 1))

    (define (install-or-download vpkg)
      (let* ((name (vpkg-name vpkg))
             (version (vpkg-version vpkg))
             (snowball (get-snowball-from-snowfort name version)))
        (process-snowball snowball)))

    (define (process-snowball snowball)
      (extract-snowball
       snowball
       (lambda (msg)
         (console-display
          msg #\newline)
         (snow-exit 1))
       (lambda (name version package-form sign-certs tar-rec-list)
         (cond ((and (memq 'verify opts)
                     (let ((x
                            (verify-tar-rec-list tar-rec-list cert-alist #t)))
                       (and (not (null? (vector-ref x 1)))
                            x)))
                =>
                (lambda (x)
                  (console-display
                   "Package " name "/" (version->string version ".")
                   " failed verification." #\newline)
                  (explain-verification-failure x)
                  (snow-exit 1)))
               ((memq 'install opts)
                (let* ((dir
                        (if (memq 'user opts)
                            (snow-user-dir)
                            (snow-site-dir)))
                       (pack-dir
                        (snow-make-filename dir "pack")))
                  (save-package pack-dir
                                name
                                version
                                tar-rec-list
                                (lambda (msg)
                                  (console-display msg #\newline)
                                  (snow-exit 1))
                                (lambda (msg)
                                  (console-display msg #\newline)))))
               (else
                (let ((filename (make-snowball-filename name version)))
                  (genport-write-file snowball filename)
                  (console-display
                   "Package " name "/" (version->string version ".")
                   " downloaded to \"" filename "\"." #\newline)))))))

    (for-each
     (lambda (arg)
       (if (and (memq 'install opts)
                (snowball-filename? arg))
           (let ((snowball (genport-read-file arg)))
             (process-snowball snowball))
           (extract-package-name-and-version
            arg
            (lambda ()
              (invalid-package-name arg))
            (lambda (name version)
              (if (not (if (memq 'exact opts)
                           (= 3 (length version))
                           (>= 3 (length version))))
                  (invalid-package-name arg)
                  (let ((pkg (lookup-pkg name snowfort-pkgs)))
                    (if (not pkg)
                        (package-does-not-exist name)
                        (if (memq 'exact opts)
                            (let ((vpkg
                                   (lookup-vpkg version
                                                (pkg-versions pkg))))
                              (if (not vpkg)
                                  (package-does-not-exist arg)
                                  (install-or-download vpkg)))
                            (let ((vpkgs
                                   (filter-acceptable-vpkgs (pkg-versions pkg)
                                                            version)))
                              (if (not (pair? vpkgs))
                                  (package-does-not-exist arg)
                                  (install-or-download (car vpkgs))))))))))))
     args)))

(define (snowman-install options args)
  (let ((opts
         (process-options
          options
          '((install)
            (verify skip-verify)
            (highest exact)
            (user site)))))
    (let ((cert-alist
           (read-certs-of-kind-and-snow
            (if (memq 'user opts)
                '(user site)
                '(site)))))
      (snowman-install-or-download-packages opts args cert-alist))))

(define (snowman-download options args)
  (let ((opts
         (process-options
          options
          '((download)
            (verify skip-verify)
            (highest exact)
            (user site)))))
    (let ((cert-alist
           (read-certs-of-kind-and-snow
            (if (memq 'user opts)
                '(user site)
                '(site)))))
      (snowman-install-or-download-packages opts args cert-alist))))

;;;----------------------------------------------------------------------------

(define (snowman-uninstall-package opts args)
  (snow-error "uninstall command is not yet implemented"))

(define (snowman-uninstall options args)
  (let ((opts
         (process-options
          options
          '((uninstall)
            (highest exact)
            (user site)))))
    (snowman-uninstall-package opts args)))

;;;----------------------------------------------------------------------------

(define (snowman-upload-u8vector u8vect)

  (define (err msg)
    (console-display "Upload failure: " msg #\newline)
    (snow-exit 1))

  (let ((response
         (http-post-query
          (snowfort-hostname)
          (snowfort-path)
          (list (cons 'operation "upload")
                (cons 'snowball u8vect))
          (snowfort-port-num))))
    (if (and (pair? response)
             (string? (cdr response)))
        (let ((lines (snow-string-split (cdr response) #\newline)))
          (if (pair? lines)
              (let ((line1 (car lines))
                    (rest (cdr lines)))
                (cond ((string=? line1 "<html><!-- upload failure")
                       (err (if (pair? rest)
                                (car rest)
                                "unknown error")))
                      ((string=? line1 "<html><!-- upload success")
                       (console-display
                        "Upload success: "
                        (if (pair? rest)
                            (car rest)
                            "no error")
                        #\newline))
                      (else
                       (err "unknown error"))))
              (err "unknown error")))
        (err "unknown error"))))

(define (snowman-upload-tgz filename)
  (cond ((not (snow-file-exists? filename))
         (console-display
          filename " does not exist!" #\newline))
        (else
         (let ((u8vect (genport-read-file filename)))
           (snowman-upload-u8vector u8vect)))))

(define (snowman-upload-dir dir package-name)
  (cond ((not (snow-file-exists? dir))
         (console-display
          dir " does not exist!" #\newline))
        ((not (snow-file-exists?
               (snow-make-filename
                dir
                "snow"
                (string-append package-name (package-file-extension)))))
         (console-display
          dir " does not contain a Snow package." #\newline))
        (else
         (let ((tar-recs (tar-read-file dir)))
           (for-each
            (lambda (tr)
              (let ((name (tar-rec-name tr)))
                (tar-rec-name-set!
                 tr
                 (string-append package-name
                                (snow-remove-string-prefix name dir)))))
            tar-recs)
           (snowman-upload-u8vector
            (gzip-u8vector
             (tar-pack-u8vector tar-recs)))))))

(define (snowman-upload-package opts arg)
  (if (snowball-filename? arg)
      (snowman-upload-tgz arg)
      (let* ((dir
              (if (and (string=? (snow-filename-directory arg) arg)
                       (not (string=? "" arg)))
                  (substring arg 0 (- (string-length arg) 1))
                  arg))
             (base
              (snow-filename-strip-directory dir))
             (parent-dir
              (snow-filename-directory dir))
             (version
              (string->version base)))
        (if version
            (if (or (not (= 3 (length version)))
                    (not (>= (string-length parent-dir) 2)))
                (console-display
                 "Expected argument of the form \"<package>/v<N>.<N>.<N>\""
                 #\newline)
                (let ((package-name
                       (snow-filename-strip-directory
                        (substring parent-dir
                                   0
                                   (- (string-length parent-dir) 1)))))
                  (snowman-upload-dir dir package-name)))
            (let* ((package-name
                    base)
                   (versions
                    (get-package-versions dir package-name '())))
              (if (null? versions)
                  (snowman-upload-dir dir package-name)
                  (let ((highest-version (car versions)))
                    (snowman-upload-dir
                     (snow-make-filename
                      dir
                      (version->string highest-version "."))
                     package-name))))))))

(define (snowman-upload options args)
  (let ((opts
         (process-options
          options
          '((upload)))))
    (for-each
     (lambda (arg)
       (snowman-upload-package opts arg))
     args)))

;;;----------------------------------------------------------------------------

(define (snowman-unupload-package opts arg)
  (snow-error "unupload command is not yet implemented"))

(define (snowman-unupload options args)
  (let ((opts
         (process-options
          options
          '((unupload)))))
    (for-each
     (lambda (arg)
       (snowman-unupload-package opts arg))
     args)))

;;;----------------------------------------------------------------------------

(define (snowman-cert-passwd options args)
  (let ((opts
         (process-options
          options
          '((cert-passwd)))))
    (if (not (null? args))
        (snowman-usage)
        (access-cert-file
         '(sign)
         (lambda (filename read-cert-alist write-cert-alist)
           (read-cert-alist
            (lambda (cert-alist password)
              (let ((new-password
                     (enter-new-filename-password
                      filename
                      "new access control ")))
                (write-cert-alist cert-alist new-password)
                (console-display
                 "Password was changed successfully." #\newline)))))))))

(define (snowman-cert-create options args)
  (let ((opts
         (process-options
          options
          '((cert-create)))))
    (if (not (null? args))
        (snowman-usage)
        (access-cert-file
         '(sign)
         (lambda (filename read-cert-alist write-cert-alist)
           (read-cert-alist
            (lambda (cert-alist password)
              (let* ((cert-pair
                      (create-certificate-pair '(snow)))
                     (owner
                      (certificate-owner (car cert-pair))))
                (if (lookup-certificate owner cert-alist)
                    (begin
                      (console-display
                       "You can't create a certificate with the same identification as an existing one." #\newline)
                      (snow-exit 1))
                    (write-cert-alist
                     (append cert-alist (list cert-pair))
                     password))))))))))

(define (snowman-cert-export options args)
  (let ((opts
         (process-options
          options
          '((cert-export) (user site sign)))))
    (if (not (or (null? args)
                 (null? (cdr args))))
        (snowman-usage)
        (access-cert-file
         opts
         (lambda (filename read-cert-alist write-cert-alist)
           (read-cert-alist
            (lambda (cert-alist password)
              (let* ((publ-priv
                      (select-cert cert-alist "exported" '()))
                     (publ
                      (car publ-priv))
                     (publ-base64
                      (cert->base64-string publ)))
                (if (pair? args)
                    (genport-write-file
                     (snow-ISO-8859-1-string->u8vector publ-base64)
                     (car args))
                    (console-display
                     publ-base64 #\newline))))))))))

(define (snowman-cert-import options args)
  (let ((opts
         (process-options
          options
          '((cert-import) (user site sign)))))
    (let ((certs
           (map (lambda (arg)
                  (if (base64-cert? arg)
                      (base64-string->cert arg)
                      (let ((u8vect (genport-read-file arg)))
                        (base64-string->cert
                         (snow-u8vector->ISO-8859-1-string u8vect)))))
                args)))
      (access-cert-file
       opts
       (lambda (filename read-cert-alist write-cert-alist)
         (read-cert-alist
          (lambda (cert-alist password)
            (for-each
             (lambda (cert)
               (console-display
                "Importing certificate ")
               (show-certificate cert)
               (let* ((owner (certificate-owner cert))
                      (x (lookup-certificate owner cert-alist)))
                 (cond ((and (memq 'sign opts)
                             (not x))
                        (console-display
                         "The signing certificate \"" owner "\" does not exist." #\newline)
                        (snow-exit 1))
                       ((and x
                             (not (certificate= (car x) cert #f)))
                        (console-display
                         (string-append
                          "Certificate \"" owner "\" is inconsistent with installed certificate.")
                         #\newline)
                        (snow-exit 1))
                       ((enter-y-or-n
                         (string-append
                          (if x "Replace" "Add")
                          " certificate \""
                          owner
                          "\" (y/n)? "))
                        (if x
                            (set-car! x cert)
                            (set! cert-alist
                                  (append cert-alist
                                          (list (cons cert #f)))))
                        (write-cert-alist cert-alist password)
                        (console-display
                         "Certificate was imported successfully." #\newline))
                       (else
                        (console-display
                         "Certificate was not imported." #\newline)))))
             certs))))))))

(define (snowman-cert-remove options args)
  (let ((opts
         (process-options
          options
          '((cert-remove) (user site sign)))))
    (if (not (null? args))
        (snowman-usage)
        (access-cert-file
         opts
         (lambda (filename read-cert-alist write-cert-alist)
           (read-cert-alist
            (lambda (cert-alist password)
              (let* ((publ-priv
                      (select-cert cert-alist "removed" '()))
                     (cert
                      (car publ-priv)))
                (console-display
                 "Removing certificate ")
                (show-certificate cert)
                (if (enter-y-or-n "Are you sure (y/n)? ")
                    (begin
                      (write-cert-alist
                       (remove-certificate (certificate-owner cert) cert-alist)
                       password)
                      (console-display
                       "Certificate was removed successfully." #\newline))
                    (console-display
                     "Certificate was not removed." #\newline))))))))))

(define (snowman-cert-list options args)
  (let ((opts
         (process-options
          options
          '((cert-list) (user site sign)))))
    (if (not (null? args))
        (snowman-usage)
        (access-cert-file
         opts
         (lambda (filename read-cert-alist write-cert-alist)
           (read-cert-alist
            (lambda (cert-alist password)
              (list-certs cert-alist))))))))

;;;----------------------------------------------------------------------------

(define (snowman-verify-cert opts base64-cert)
  (let ((cert (base64-string->cert base64-cert)))
    (console-display
     "Certificate ")
    (show-certificate cert)
    (if (verify-certificate cert)
        (console-display
         "passed verification." #\newline)
        (begin
          (console-display
           "failed verification." #\newline)
          (snow-exit 1)))))

(define (snowman-verify-file opts filename cert-alist show-trace)
  (if (not (snow-file-exists? filename))
      (begin
        (console-display
         filename " does not exist!" #\newline)
        (snow-exit 1))
      (let* ((files
              (snow-directory-subfiles filename '(regular)))
             (x
              (verify-files
               files
               cert-alist
               (lambda (file)
                 file)
               (lambda (file)
                 (genport-read-file file))
               (lambda (filename)
                 (and (snow-file-exists? filename)
                      filename))
               show-trace))
             (unver
              (vector-ref x 1)))
        (if (null? unver)
            (console-display
             filename " passed verification." #\newline)
            (begin
              (console-display
               filename " failed verification." #\newline)
              (explain-verification-failure x)
              (snow-exit 1))))))

(define (snowman-verify options args)
  (let ((opts
         (process-options
          options
          '((verify) (user site)))))
    (let ((cert-alist
           (read-certs-of-kind-and-snow
            (if (memq 'user opts)
                '(user site)
                '(site)))))
      (for-each
       (lambda (arg)
         (if (base64-cert? arg)
             (snowman-verify-cert opts arg)
             (snowman-verify-file opts arg cert-alist #t)))
       args))))

;;;----------------------------------------------------------------------------

(define (select-sign-cert)
  (access-cert-file
   '(sign)
   (lambda (filename read-cert-alist write-cert-alist)
     (read-cert-alist
      (lambda (cert-alist password)
        (select-cert cert-alist "used for signing" '()))))))

(define (snowman-sign-cert opts base64-cert publ-priv algorithm)
  (let* ((cert (base64-string->cert base64-cert))
         (signed-cert (sign-certificate cert publ-priv algorithm))
         (base64-signed-cert (cert->base64-string signed-cert)))
    (console-display
     base64-signed-cert #\newline)))

(define (snowman-sign-regular-file filename publ-priv algorithm)
  (let* ((u8vect
          (genport-read-file filename))
         (hash
          (digest-u8vector u8vect algorithm 'u8vector))
         (hash-hex
          (snow-u8vector->hex-string hash))
         (sig-filename
          (string-append filename (signature-file-extension)))
         (current-signatures
          (if (snow-file-exists? sig-filename)
              (let* ((content
                      (genport-read-file sig-filename))
                     (signatures
                      (snow-string->object-list 
                       (snow-u8vector->ISO-8859-1-string content))))
                (let loop ((lst signatures) (rev-result '()))
                  (if (pair? lst)
                      (let* ((s
                              (car lst))
                             (publ-sign-cert
                              (list->certificate (list-ref s 0))))
                        (if (or (string=? (certificate-owner publ-sign-cert)
                                          (certificate-owner (cdr publ-priv)))
                                (not (equal? (list-ref s 1) algorithm))
                                (not (string=? (list-ref s 2) hash-hex)))
                            (loop (cdr lst) rev-result)
                            (loop (cdr lst) (cons s rev-result))))
                      (reverse rev-result))))
              '()))
         (new-signatures
          (append current-signatures
                  (list (create-signature hash publ-priv algorithm)))))
    (if (snow-file-exists? sig-filename)
        (snow-delete-file sig-filename))
    (genport-write-file 
     (snow-ISO-8859-1-string->u8vector
      (snow-object-list->string new-signatures))
     sig-filename)))

(define (snowman-sign-file opts filename publ-priv algorithm)

  (define (process filename)
    (if (not (signature-filename? filename))
        (begin
          (console-display
           "Signing " filename)
          (snowman-sign-regular-file filename publ-priv algorithm)
          (console-display
           " -- done!" #\newline))))

  (if (not (snow-file-exists? filename))
      (begin
        (console-display
         filename " does not exist!" #\newline)
        (snow-exit 1))
      (let ((files (snow-directory-subfiles filename '(regular))))
        (for-each process files))))

(define (snowman-sign options args)
  (let ((opts
         (process-options
          options
          '((sign) (sha-1 sha-224 sha-256)))))
    (let ((publ-priv (select-sign-cert))
          (algorithm (cadr opts)))
      (for-each
       (lambda (arg)
         (if (base64-cert? arg)
             (snowman-sign-cert opts arg publ-priv algorithm)
             (snowman-sign-file opts arg publ-priv algorithm)))
       args))))

;;;----------------------------------------------------------------------------

(define (split-args args cont)
  (let loop ((lst args) (rev-opts '()))

    (define (return)
      (cont (map string->symbol (reverse rev-opts))
            lst))

    (if (pair? lst)
        (let* ((arg (car lst))
               (opt (or (snow-remove-string-prefix arg "--")
                        (and (null? rev-opts)
                             arg))))
          (if opt
              (loop (cdr lst) (cons opt rev-opts))
              (return)))
        (return))))

(define (snowman-main)
  (let ((args (cdr (snow-command-line))))
    (split-args
     args
     (lambda (opts args)
       (cond ((memq 'list opts)        (snowman-list opts args))
             ((memq 'install opts)     (snowman-install opts args))
             ((memq 'uninstall opts)   (snowman-uninstall opts args))
             ((memq 'download opts)    (snowman-download opts args))
             ((memq 'upload opts)      (snowman-upload opts args))
             ((memq 'unupload opts)    (snowman-unupload opts args))
             ((memq 'cert-passwd opts) (snowman-cert-passwd opts args))
             ((memq 'cert-create opts) (snowman-cert-create opts args))
             ((memq 'cert-export opts) (snowman-cert-export opts args))
             ((memq 'cert-import opts) (snowman-cert-import opts args))
             ((memq 'cert-remove opts) (snowman-cert-remove opts args))
             ((memq 'cert-list opts)   (snowman-cert-list opts args))
             ((memq 'verify opts)      (snowman-verify opts args))
             ((memq 'sign opts)        (snowman-sign opts args))
             (else                     (snowman-usage)))))))

;;;============================================================================
