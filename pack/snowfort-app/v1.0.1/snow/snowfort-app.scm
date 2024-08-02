;;;============================================================================

;;; File: "snowfort-app.scm", Time-stamp: <2007-04-05 01:36:26 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Repository manager for the Scheme Now! framework.

(package* snowfort-app/v1.0.1
 (provide:

  (define (snowfort-hostname))
  (define (snowfort-doc-dir)) ;; deprecated
  (define (snowfort-doc-file)) ;; deprecated
  (define (snowfort-port-num))
  (define (snowfort-path))

  (define-record* pkg
    name
    versions)

  (define-record* vpkg
    name
    version
    released
    publ-certs
    maintainer
    author
    homepage
    description
    keywords
    package-form)

  (define (package-form->vpkg package-form))

  (define (valid-package-name? name))
  (define (version->string version separator))
  (define (string->version str))
  (define (make-snowball-filename package-name version))
  (define (snowball-filename? filename))
  (define (get-package-versions dir package-name req-version))
  (define (package-file-extension))
  (define (signature-file-extension))
  (define (signature-filename? filename))
  (define (extract-package-name-and-version str fail cont))
  (define (compare-versions version1 version2))
  (define (compatible-version-with version req-version))
  (define (extract-snowball snowball fail cont))
  (define (verify-files files cert-alist file-name rd-file file-ex? show-trace))
  (define (verify-tar-rec-list tar-rec-list cert-alist show-trace))
  (define (snow-certificate-alist))

  (define (snowfort-main)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Snow package repository server.")

 (keywords: snow)

 (license: lgpl/v2.1)

 (require: bignum/v1)
 (require: homovector/v1)
 (require: cert/v1)
 (require: cgi/v1)
 (require: filesys/v1)
 (require: genport/v1)
 (require: extio/v1)
 (require: zlib/v1)
 (require: tar/v1)
 (require: time/v1)
 (require: list/v1)
 (require: sort/v1)
 (require: ttyui/v1)
 (require: hostos/v1)
 (require: string/v1))

;;;============================================================================

(define snowfort-url
  (snow-getenv "SNOW_URL"
               "http://snow.iro.umontreal.ca"))

(define snowfort-mailing-list-url
  "http://webmail.iro.umontreal.ca/mailman/listinfo/snow-users-list")

(define (repository-dir) "snow-repository")
(define (pack-dir) (snow-make-filename (repository-dir) "pack"))
(define (package-file-extension) ".scm")
(define (signature-file-extension) ".sig")
(define (generic-snow-package-name) "snow-generic")
(define (snow-maintainer) "Scheme Now! <snow at iro.umontreal.ca>")

(define (snow-certificate-alist)
  (map (lambda (cert-as-list)
         (let ((cert (list->certificate cert-as-list)))
           (cons cert #f)))
       '(
         (cert-v1
          "Scheme Now! <snow at iro.umontreal.ca>"
          ("RdcIZg==" "Tz0J5g==")
          (snow)
          (1024
           "AbxbbjLSp8yb5CdAXpJElv0d7TEFsuS8FlzCvrIK8SugCDAySsUPYn3Eq7Lm3Cy6rX1/HkallGxTMX+Czj96VCvRr+BaYmWx0LTjiuhXPDmy9vZv6PVPHCw697aT2HHOmueVAHuN2QxUQnzlxBjf2K0DkH+GS47naE+BtN9uJT0b"
           "AQAB")
          ((cert-v1
            "Scheme Now! CA <snow at iro.umontreal.ca>"
            ("RdcHVQ==" "WKMKVQ==")
            (snow ca)
            (1024
             "AdUOdLhLCWw0S4uCKNnfVyO4yUvA00ZGYq8pJ6PZdTf6VybvyNnCb7YwI+ryECy2PdHFdHU6hXzAoIKk1+++jN/KWRupIH9oBv8UaxYjWudMJNBUx2AMpSTWqatXlUsUp6/KgtXmDzCFu2qy5EuIz8TxnuApW+UIzctuLK59jj0p"
             "AQAB")
            #f)
           sha-1
           "c505f65ab3e52948287d01062c2d4267bc93b694"
           "f76b309d6ef323fcef24814bb6876b8a75abca458346abf2b5a7d513c2e94a3c93a3f8a692fe9f4c246be34115228bc835c5c6771aba11c4bf45c5bd7ffca3355bd8e2de5d6a215559adb1b17f3d580a8f2b4d3ff3a52df7c166cf5da23392665788d41a04ad183c99755b3c6ce76a0ccec6da502c341def766b988622eab541"))
         )))

(define (plain-tag) "<html><!-- plain page")
(define (error-tag) "<html><!-- error")
(define (upload-success-tag) "<html><!-- upload success")
(define (upload-failure-tag) "<html><!-- upload failure")
(define (download-failure-tag) "<html><!-- download failure")
(define (end-of-line) "
")

(define (make-snowball-filename package-name version)
  (string-append package-name "-" (version->string version "_") ".tgz"))

(define (snowball-filename? filename)
  (or (snow-remove-string-suffix filename ".tgz")
      (snow-remove-string-suffix filename ".tar.gz")))

(define (valid-package-name? name)

  (define (lowercase? c) (and (char>=? c #\a) (char<=? c #\z)))
  (define (digit? c) (and (char>=? c #\0) (char<=? c #\9)))
  (define (underscore? c) (char=? c #\_))
  (define (dash? c) (char=? c #\-))

  (let ((len (string-length name)))
    (and (>= len 1)
         (let ((c (string-ref name 0)))
           (or (lowercase? c)
               (underscore? c)))
         (let loop ((i 1))
           (if (< i len)
               (let ((c (string-ref name i)))
                 (and (or (lowercase? c)
                          (digit? c)
                          (underscore? c)
                          (dash? c))
                      (loop (+ i 1))))
               #t)))))

(define (core-package-name? name)
  (and (>= (string-length name) 1)
       (char=? (string-ref name 0) #\_)))

(define (split-url url)

  (define (parse1 url)
    (let* ((s1 (or (snow-remove-string-prefix url "http://")
                   url))
           (i (snow-string-index s1 #\/)))
      (if i
          (parse2 (substring s1 0 i)
                  (substring s1 i (string-length s1)))
          (parse2 s1
                  "/"))))

  (define (parse2 server doc)
    (let ((j (snow-string-index server #\:)))
      (if j
          (let ((n (string->number
                    (substring server (+ j 1) (string-length server)))))
            (and n
                 (list (substring server 0 j) n doc)))
          (list server 80 doc))))

  (parse1 url))

(define conf-snowfort-hostname "snow.iro.umontreal.ca")
(define conf-snowfort-port-num 80)
(define conf-snowfort-doc "/")

(define (snowfort-hostname) conf-snowfort-hostname)
(define (snowfort-doc-dir) "") ;; deprecated
(define (snowfort-doc-file) "") ;; deprecated
(define (snowfort-port-num) conf-snowfort-port-num)
(define (snowfort-path) conf-snowfort-doc)

(let ((x (split-url snowfort-url)))
  (if x
      (begin
        (set! conf-snowfort-hostname (car x))
        (set! conf-snowfort-port-num (cadr x))
        (set! conf-snowfort-doc (caddr x)))))

;;;----------------------------------------------------------------------------

(define (compare-versions version1 version2)
  (cond ((null? version1)
         (if (null? version2)
             '=
             '<))
        ((null? version2)
         '>)
        ((< (car version1) (car version2))
         '<)
        ((> (car version1) (car version2))
         '>)
        (else
         (compare-versions (cdr version1) (cdr version2)))))

(define (compatible-version-with version req-version)

  ;; Check if version is compatible with req-version (i.e. version has
  ;; the same major version as req-version and version has at least the
  ;; same minor and build version as version 2).  version is a full
  ;; version with 3 numbers, while req-version is a partial version
  ;; containing from 0 to 3 numbers.

  (cond ((null? req-version) ;; anything goes for version
         #t)
        ((not (= (car version) (car req-version)))
         #f)
        (else
         (let loop ((lst1 (cdr version)) (lst2 (cdr req-version)))
           (if (pair? lst2)
               (let ((n1 (car lst1)) (n2 (car lst2)))
                 (if (= n1 n2)
                     (loop (cdr lst1) (cdr lst2))
                     (> n1 n2)))
               #t)))))

(define (sort-versions versions)
  (snow-sort
   versions
   (lambda (v1 v2)
     (eq? (compare-versions v1 v2) '>))))

(define (get-package-versions dir package-name req-version)
  (sort-versions
   (snow-apply-append
    (map (lambda (version-str)
           (let ((version (string->version version-str)))
             (if (not (and version
                           (= 3 (length version))
                           (compatible-version-with version req-version)))
                 '()
                 (let* ((version-dir
                         (snow-make-filename dir version-str))
                        (disable-dir
                         (snow-make-filename dir ".disable"))
                        (snow-package-filename
                         (snow-make-filename
                          version-dir
                          "snow"
                          (string-append
                           package-name
                           (package-file-extension)))))
                   (if (and (not (snow-file-exists? disable-dir))
                            (snow-file-exists? snow-package-filename))
                       (list version)
                       '())))))
         (snow-directory-files dir)))))

;;;----------------------------------------------------------------------------

(define (get-package-name root)
  (let ((slash (snow-string-index root #\/)))
    (if (or (not (snow-remove-string-suffix root "/"))
            (not slash))
        #f
        (substring root 0 slash))))

(define (dangerous-unix-filename? filename)
  (let loop ((i 0) (prev #\/))
    (if (< i (string-length filename))
        (let ((c (string-ref filename i)))
          (cond ((char=? c #\/)
                 (or (char=? prev #\/)
                     (loop (+ i 1) c)))
                ((char=? c #\.)
                 (or (char=? prev #\.)
                     (loop (+ i 1) c)))
                (else
                 (and (or (char-alphabetic? c)
                          (char-numeric? c)
                          (char=? c #\-)
                          (char=? c #\_))
                      (loop (+ i 1) c)))))
        #f)))

(define (string-tail-index str i separator)
  (let loop ((i (- i 1)))
    (if (< i 0)
        0
        (let ((c (string-ref str i)))
          (cond ((char=? c separator)
                 (+ i 1))
                (else
                 (loop (- i 1))))))))

(define (version->string version separator)

  (define (version->str version)
    (let loop ((lst version)
               (sep "")
               (result ""))
      (if (pair? lst)
          (loop (cdr lst)
                separator
                (string-append
                 result
                 sep
                 (number->string (car lst))))
          result)))

  (string-append "v" (version->str version)))

(define (string->version str)

  (define (str->version str)
    (let loop ((end (string-length str))
               (version-list '()))
      (if (< end 0)
          version-list
          (let* ((i
                  (string-tail-index str end #\.))
                 (s
                  (substring str i end))
                 (n
                  (string->number s)))
            (if (and n
                     (integer? n)
                     (exact? n)
                     (string=? s (number->string n)))
                (loop (- i 1)
                      (cons n version-list))
                #f)))))

  (let ((len (string-length str)))
    (if (and (>= len 2)
             (char=? (string-ref str 0) #\v))
        (str->version (substring str 1 len))
        #f)))

(define (extract-package-name-and-version str fail cont)
  (let ((i (string-tail-index str (string-length str) #\/)))
    (if (= i 0)
        (if (not (valid-package-name? str))
            (fail)
            (cont str '()))
        (let ((package-name (substring str 0 (- i 1))))
          (if (not (valid-package-name? package-name))
              (fail)
              (let* ((package-version-str
                      (substring str i (string-length str)))
                     (package-version
                      (string->version package-version-str)))
                (if (not package-version)
                    (fail)
                    (cont package-name package-version))))))))

(define (read-package-info str)
  (snow-call-with-input-string
   str
   (lambda (port)

     (define eol-chars '(10 13))

     ;; If the file starts with "#" or "@" we treat it as a script
     ;; and ignore the first line.

     (if (member (peek-char port) '(#\# #\@))
         (let loop ()
           (let ((c (peek-char port)))
             (if (not (eof-object? c))
                 (begin
                   (read-char port)
                   (if (not (memv (char->integer c) eol-chars))
                       (loop)))))))

     ;; Allow any number of strings before the package* form (to
     ;; work around #! ... !# script syntax of Guile and Scsh).

     (let loop ()
       (let ((x (read port)))
         (if (string? x)
             (loop)
             (begin
               (close-input-port port)
               x)))))))

;;;----------------------------------------------------------------------------

(define (signature-filename? filename)
  (let ((ext (snow-filename-extension filename)))
    (string-ci=? ext (signature-file-extension))))

(define (verify-files files cert-alist file-name rd-file file-ex? show-trace)

  (define (verify-regular-file file cont)
    (let* ((filename
            (file-name file))
           (sig-filename
            (string-append filename (signature-file-extension)))
           (sig-file
            (file-ex? sig-filename)))
      (if (not sig-file)
          (cont #f #f #f)
          (let* ((u8vect
                  (rd-file file))
                 (signatures
                  (let ((content
                         (rd-file sig-file)))
                    (snow-string->object-list 
                     (snow-u8vector->ISO-8859-1-string content))))
                 (classified-signatures
                  (verify-u8vector-detailed u8vect signatures cert-alist))
                 (valid-certs
                  (filter-certificates-of-class classified-signatures
                                                '(valid)))
                 (missing-certs
                  (filter-certificates-of-class classified-signatures
                                                '(missing-cert
                                                  different-cert))))
            (cont valid-certs missing-certs signatures)))))

  (let ((verified '())
        (unverified '())
        (missing #f))

    (define (memb cert lst)
      (if (pair? lst)
          (or (certificate= cert (car lst))
              (memb cert (cdr lst)))
          #f))

    (define (intersection-certs certs1 certs2)

      (define (intersect certs1 certs2 result)
        (if (pair? certs1)
            (let ((cert (car certs1)))
              (intersect (cdr certs1)
                         certs2
                         (if (and (memb cert certs2)
                                  (not (memb cert result)))
                             (cons cert result)
                             result)))
            (reverse result)))

      (intersect (or certs1 certs2) certs2 '()))

    (define (process file)
      (let ((filename (file-name file)))
        (if (not (signature-filename? filename))
            (begin
              (if show-trace
                  (console-display filename))
              (verify-regular-file
               file
               (lambda (valid-certs missing-certs signatures)
                 (cond ((not valid-certs)
                        (set! unverified (cons file unverified))
                        (if show-trace
                            (begin
                              (set! missing '())
                              (console-display
                               " -- signature file is missing!" #\newline))))
                       ((null? valid-certs)
                        (set! missing
                              (intersection-certs missing
                                                  missing-certs))
                        (set! unverified (cons file unverified))
                        (if show-trace
                            (if (null? signatures)
                                (console-display
                                 " -- signature file is empty!" #\newline)
                                (console-display
                                 " -- invalid signature by: "
                                 (snow-object->string
                                  (map (lambda (x)
                                         (certificate-owner
                                          (list->certificate (list-ref x 0))))
                                       signatures))
                                 #\newline))))
                       (else
                        (set! verified (cons file verified))
                        (if show-trace
                            (console-display
                             " -- signed by: "
                             (snow-object->string
                              (map certificate-owner valid-certs))
                             #\newline))))))))))

    (for-each process files)

    (vector (reverse verified)
            (reverse unverified)
            (or missing '()))))

(define (verify-tar-rec-list tar-rec-list cert-alist show-trace)
  (let ((tar-recs
         (snow-filter
          (lambda (tr) (not (eq? (tar-rec-type tr) 'directory)))
          tar-rec-list)))
    (verify-files
     tar-recs
     cert-alist
     (lambda (tr)
       (tar-rec-name tr))
     (lambda (tr)
       (tar-rec-content tr))
     (lambda (filename)
       (let loop ((lst tar-recs))
         (if (pair? lst)
             (let ((tr (car lst)))
               (if (string=? filename (tar-rec-name tr))
                   tr
                   (loop (cdr lst))))
             #f)))
     show-trace)))

;;;----------------------------------------------------------------------------

(define (html-escape str)
  (let ((end (string-length str)))
    (let loop ((i 0) (j 0) (rev-pieces '()))

      (define (accum)
        (if (< i j)
            (cons (substring str i j) rev-pieces)
            rev-pieces))

      (define (special x)
        (loop (+ j 1) (+ j 1) (cons x (accum))))

      (if (< j end)
          (let ((c (string-ref str j)))
            (cond ((char=? c #\")
                   (special "&quot;"))
                  ((char=? c #\&)
                   (special "&amp;"))
                  ((char=? c #\<)
                   (special "&lt;"))
                  ((char=? c #\>)
                   (special "&gt;"))
                  ((and (char>=? c #\space)
                        (char<=? c #\~))
                   (loop i (+ j 1) rev-pieces))
                  (else
                   (special
                    (string-append "&#"
                                   (number->string (char->integer c))
                                   ";")))))
          (snow-apply-string-append (reverse (accum)))))))

;;;----------------------------------------------------------------------------

(define (package-link-html name)
  (string-append (snowfort-path) "?viewpkg=" name))

(define (version-html version)
  (version->string version "."))

(define (released-html release-time)
  (if release-time
      (html-escape
       (let* ((ct (current-time-seconds))
              (secs (bignum- ct release-time)))

         (define (express-in time-unit div)
           (string-append
            (bignum->string (bignum-quotient secs (fixnum->bignum div)))
            " " time-unit "s ago"))

         (cond ((bignum-negative? secs)
                "unknown")
               ((bignum< secs (fixnum->bignum 120))
                (express-in "sec" 1))
               ((bignum< secs (fixnum->bignum 7200))
                (express-in "min" 60))
               ((bignum< secs (fixnum->bignum 172800))
                (express-in "hour" 3600))
               (else
                (express-in "day" 86400)))))
      ""))

(define (string-list-html str-lst)
  (if (null? str-lst)
      "---"
      (snow-apply-string-append
       (cons (html-escape (car str-lst))
             (map (lambda (s)
                    (string-append
                     "<br>"
                     (html-escape s)))
                  (cdr str-lst))))))

(define (maint-html maintainer)
  (string-list-html maintainer))

(define (auth-html author)
  (string-list-html author))

(define (homepage-html homepage)
  (if (null? homepage)
      "---"
      (snow-apply-string-append
       (map (lambda (url)
              (string-append
               "<a href=\"" url "\">"
               (html-escape url)
               "</a> "))
            homepage))))

(define (descr-html description)
  (if (not description)
      "---"
      (html-escape description)))

(define (listcat keyword)
  (string-append
   (snowfort-path)
   "?listcat=" (symbol->string keyword)))

(define (keyw-html keywords)
  (if (null? keywords)
      "---"
      (snow-apply-string-append
       (map (lambda (key)
              (string-append
               "<a href=\"" (listcat key) "\">"
               (html-escape
                (symbol->string key))
               "</a> "))
            keywords))))

(define package-table-columns
  '(("Package"     . 120)
    ("Version"     . 70)
    ("Released"    . 100)
    ("Description" . 550)
    ("Keywords"    . 150)))

(define (make-package-table-html caption pkg-list)

  (define (range start end)
    (let loop ((i (- end 1)) (lst '()))
      (if (>= i start)
          (loop (- i 1)
                (cons i lst))
          lst)))

  (let* ((cols
          package-table-columns)
         (col-widths
          (snow-apply-string-append
           (map (lambda (col)
                  (string-append "<col width=\""
                                 (number->string (cdr col))
                                 "\"></col>"))
                cols)))
         (col-titles
          (string-append
           "<tr>"
           (snow-apply-string-append
            (map (lambda (col)
                   (string-append "<th scope=\"col\">" (car col) "</th>"))
                 cols))
           "</tr>")))
    (string-append
     "<table>"
     col-widths
     "<caption>" caption "</caption>"
     "<thead>" col-titles "</thead>"
     "<tbody>"
     (snow-apply-string-append
      (map (lambda (pkg i)
             (string-append
              "<tr class=\""
              (if (even? i) "even" "odd")
              "\" onclick=\"location = '"
              (package-link-html (pkg-name pkg))
              "'\">"
              "<td><a href=\"" (package-link-html (pkg-name pkg)) "\">"
              (pkg-name pkg)
              "</a></td>"
              (let ((versions (pkg-versions pkg)))
                (if (pair? versions)
                    (let ((vpkg (car versions)))
                      (string-append
                       "<td>" (version-html (vpkg-version vpkg)) "</td>"
                       "<td>" (released-html (vpkg-released vpkg)) "</td>"
                       "<td>" (descr-html (vpkg-description vpkg)) "</td>"
                       "<td>" (keyw-html (vpkg-keywords vpkg)) "</td>"))
                     "<td>---</td><td>---</td><td>---</td><td>---</td>"))
              "</tr>"))
           pkg-list
           (range 0 (length pkg-list))))
     "</tbody>"
     "</table>")))

(define (download-form package-name version)
  (string-append
   "<form method=\"post\" action=\""
   (snowfort-path)
   "\">"
   "<input type=\"hidden\" name=\"operation\" value=\"download\">"
   "<input type=\"hidden\" name=\"pkg\" value=\""
   package-name "/" (version->string version ".")
   "\">"
   "<input type=\"submit\" value=\"download "
   (make-snowball-filename package-name version)
   "\"></form>"))

(define (make-package-list-html pkg)
  (string-append
   "<hr>"
   (snow-apply-string-append
    (map (lambda (vpkg)
           (string-append
            (download-form (pkg-name pkg) (vpkg-version vpkg))
            "<dl>"
            "<dt>Version:</dt>"
            "<dd>" (version-html (vpkg-version vpkg)) "</dd>"
            "<dt>Released:</dt>"
            "<dd>" (released-html (vpkg-released vpkg)) "</dd>"
            "<dt>Maintainer:</dt>"
            "<dd>" (maint-html (vpkg-maintainer vpkg)) "</dd>"
            "<dt>Author:</dt>"
            "<dd>" (auth-html (vpkg-author vpkg)) "</dd>"
            "<dt>Homepage:</dt>"
            "<dd>" (homepage-html (vpkg-homepage vpkg)) "</dd>"
            "<dt>Description:</dt>"
            "<dd>" (descr-html (vpkg-description vpkg)) "</dd>"
            "<dt>Keywords:</dt>"
            "<dd>" (keyw-html (vpkg-keywords vpkg)) "</dd>"
            "<dt>Package form:</dt>"
            "<dd><pre>"
            (html-escape
             (snow-call-with-output-string
              (lambda (port)
                (snow-pretty-print (vpkg-package-form vpkg) port))))
            "</pre></dd><hr>"))
         (pkg-versions pkg)))))

(define (extract-attrib attrib package-form)
  (let loop ((lst package-form))
    (if (pair? lst)
        (let ((x (car lst)))
          (if (and (pair? x)
                   (eq? (car x) attrib)
                   (list? x))
              (cdr x)
              (loop (cdr lst))))
        #f)))

(define (package-form->vpkg package-form)
  (if (not (and (>= (length package-form) 2)
                (eq? (car package-form) 'package*)
                (symbol? (cadr package-form))))
      #f
      (extract-package-name-and-version
       (symbol->string (cadr package-form))
       (lambda ()
         #f)
       (lambda (name version)
         (let* ((m
                 (extract-attrib 'maintainer: package-form))
                (maintainer
                 (if (and m
                          (>= (length m) 1)
                          (not (memq #f (map string? m))))
                     m
                     '()))
                (a
                 (extract-attrib 'author: package-form))
                (author
                 (if (and a
                          (>= (length a) 1)
                          (not (memq #f (map string? a))))
                     a
                     '()))
                (h
                 (extract-attrib 'homepage: package-form))
                (homepage
                 (if (and h
                          (>= (length a) 1)
                          (not (memq #f (map string? a))))
                     h
                     '()))
                (d
                 (extract-attrib 'description: package-form))
                (description
                 (if (and d
                          (>= (length d) 1)
                          (string? (car d)))
                     (car d)
                     #f))
                (k
                 (extract-attrib 'keywords: package-form))
                (keywords
                 (if (and k
                          (not (memq #f (map symbol? k))))
                     k
                     '())))
           (make-vpkg
            name
            version
            #f
            #f
            maintainer
            author
            homepage
            description
            keywords
            package-form))))))

(define (get-package-info dir package-name version)
  (let* ((version-str
          (version->string version "."))
         (version-dir
          (snow-make-filename dir package-name version-str))
         (snow-package-filename
          (snow-make-filename
           version-dir
           "snow"
           (string-append package-name (package-file-extension)))))
    (with-input-from-file
        snow-package-filename
      (lambda ()
        (let* ((package-form
                (read))
               (r
                (read))
               (released
                (if (string? r)
                    (string->bignum r)
                    (current-time-seconds)))
               (pc
                (read))
               (publ-certs
                (if (pair? pc)
                    (map list->certificate pc)
                    '()))
               (vpkg
                (package-form->vpkg package-form)))
          (if (not vpkg)
              (snow-raise "error")
              (begin
                (vpkg-released-set! vpkg released)
                (vpkg-publ-certs-set! vpkg publ-certs)
                vpkg)))))))

(define (get-package dir package-name req-version)
  (let ((package-dir
         (snow-make-filename dir package-name)))
    (make-pkg
     package-name
     (if (not (snow-file-exists? package-dir))
         '()
         (let ((versions
                (get-package-versions package-dir package-name req-version)))
           (map (lambda (version)
                  (get-package-info dir package-name version))
                versions))))))

(define (get-packages dir req-version)
  (let* ((package-names
          (snow-directory-files dir))
         (pkgs
          (map (lambda (package-name)
                 (get-package dir package-name req-version))
               package-names)))
    pkgs))

;;;----------------------------------------------------------------------------

(define (send-page content)
  (cgi-send-response
   '((content-type (text . html)))
   (snow-ISO-8859-1-string->u8vector content)))

(define (decorated-page tag message title tab content)
  (string-append
   tag
   (end-of-line)
   message
   (end-of-line)
   "-->"
   (end-of-line)
   "
<head>
<title>
"
   title
"</title>
<link rel=\"StyleSheet\" href=\"snow-css.css\" type=\"text/css\">
</head>

<body id=\"" tab "\">

<div id=\"wrap\">

<div id=\"header\"></div>

<div id=\"nav\">
  <ul>
    <li id=\"t1\">
      <a href=\"" (snowfort-path) "?tab=Home\">Home</a></li>
    <li id=\"t2\">
      <a href=\"" (snowfort-path) "?tab=Installing\">Installing</a></li>
    <li id=\"t3\">
      <a href=\"" (snowfort-path) "?tab=Documentation\">Documentation</a></li>
    <li id=\"t4\">
      <a href=\"" (snowfort-path) "?tab=Packages\">Packages</a></li>
    <li id=\"t5\">
      <a href=\"" (snowfort-path) "?tab=Upload\">Upload</a></li>
    <li id=\"t6\">
      <a href=\"" (snowfort-path) "?tab=Links\">Links</a></li>
    <li id=\"t7\">
      <a href=\"" snowfort-mailing-list-url "\">Mailing list</a></li>
  </ul>
</div>

<div id=\"main\">

"
   content
"
</div>

</div>

</body>
</html>
"))

(define (respond-with-status tag message)
  (send-page
   (decorated-page
    tag
    message
    "Snowfort status"
    "selt0"
    (string-append "Status: " message))))

;;;----------------------------------------------------------------------------

(define (remove-dups keys)
  (let loop ((lst (reverse keys)) (result '()))
    (if (pair? lst)
        (loop (cdr lst)
              (if (memq (car lst) result)
                  result
                  (cons (car lst) result)))
        result)))

(define (get-keys pkg)
  (let* ((vs
          (pkg-versions pkg))
         (cs
          (remove-dups
           (if (null? vs) '() (vpkg-keywords (car vs))))))
    (if (null? cs)
        '(misc)
        cs)))

(define (get-key pkg key)
  (let ((keys (get-keys pkg)))
    (if (or (null? (cdr keys))
            (not (eq? key (car keys))))
        (car keys)
        (cadr keys))))

(define (sort-by-key pkgs key)
  (let ((sorted-key-pkgs
         (snow-sort
          (map (lambda (pkg)
                 (cons (get-key pkg key) pkg))
               pkgs)
          (lambda (x y)
            (string<? (symbol->string (car x))
                      (symbol->string (car y)))))))
    (let loop ((lst (reverse sorted-key-pkgs)) (result '()))
      (if (pair? lst)
          (let* ((x (car lst))
                 (key (car x))
                 (pkg (cdr x)))
            (loop (cdr lst)
                  (if (and (pair? result)
                           (eq? (car (car result)) key))
                      (cons (cons key (cons pkg (cdr (car result))))
                            (cdr result))
                      (cons (cons key (cons pkg '()))
                            result))))
          result))))

(define (categorize-packages keyword)
  (snow-apply-string-append
   (map (lambda (x)
          (let ((key (car x))
                (pkgs (cdr x)))
            (make-package-table-html
             (symbol->string key)
             (snow-sort
              pkgs
              (lambda (x y)
                (string<? (pkg-name x) (pkg-name y)))))))
        (let ((pkgs (get-packages (pack-dir) '())))
          (if (eq? keyword 'all)
              (sort-by-key pkgs #f)
              (let ((key-pkgs
                     (snow-filter
                      (lambda (pkg)
                        (memq keyword (get-keys pkg)))
                      pkgs)))
                (sort-by-key key-pkgs keyword)))))))

(define (do-listcat query key)
  (let ((keyword (string->symbol key)))
    (send-page
     (decorated-page
      (plain-tag)
      ""
      "Snowfort"
      "selt4"
      (string-append
       (if (eq? keyword 'all)
           "<h1>Packages ordered by main keyword</h1>"
           (string-append
            "<h1>Packages with keyword \""
            (symbol->string keyword)
            "\" ordered by subkeyword</h1>"))
       (categorize-packages keyword))))))

;;;----------------------------------------------------------------------------

(define (do-viewpkg query name-with-req-version)
  (extract-package-name-and-version
   name-with-req-version
   (lambda ()
     (respond-with-status (error-tag)
                          "request syntax error"))
   (lambda (name req-version)
     (let ((pkg (get-package (pack-dir) name req-version)))
       (send-page
        (decorated-page
         (plain-tag)
         ""
         "Snowfort"
         "selt4"
         (string-append
          "<h1>Package \"" name "\"</h1>"
          (make-package-list-html pkg))))))))

;;;----------------------------------------------------------------------------

(define (html-Home) "
<center>
<i>Scheme is like a ball of snow. You can add any amount of snow
to it and it still looks like a ball of snow.<br> Moreover, snow is
cleaner than mud.</i>, Marc Feeley
</center>

<br>

<h1>Welcome to Scheme Now!</h1>

<p>
Scheme Now!, also know as Snow, is a repository of Scheme packages
that are <b>portable</b> to several popular implementations of Scheme.
</p>

<p>
Snow is a general framework for developing
and distributing portable Scheme packages.  Snow comes with a
set of core packages that provide portable APIs for practical
programming features such as networking, cryptography, data
compression, file system access, etc.  Snow packages can export
procedures, macros and records.
</p>

<p>
Snow is not dedicated to a single Scheme system, in contrast to other
Scheme package repositories such as
<a href=\"http://www.call-with-current-continuation.org/eggs/\">Eggs</a>
and <a href=\"http://planet.plt-scheme.org/\">PLaneT</a>.  While Snow
depends on non-standard features of the host Scheme system, the APIs Snow
provides can be used in most R4RS Scheme systems.  The Snow framework is
a <b>specification</b> of a package structure and a package
distribution protocol.  The framework is not biased toward an existing
Scheme module system, but can be mapped fairly directly to many
existing module systems allowing Snow packages to be used from code
that is Scheme system specific as well as from other Snow packages.
</p>

<p>
The Snow framework can be <b>implemented</b> in various ways and in
fact we foresee the creation of implementations tailored to each
Scheme system that take advantage of that system's features and module
system.  Such <b>specialized</b> Snow framework implementations have
already been written for some of the popular Scheme systems and
efforts are underway to implement more.  We have also written
a <b>generic</b> Snow framework implementation that works on most
Scheme systems but that does not achieve the same level of integration
with the host Scheme system.  It provides features which are helpful
for testing portability of newly written packages and it is useful
when a specialized implementation of Snow is not yet available for the
host Scheme system.  The generic Snow framework implementation
currently supports a dozen host Scheme systems.
</p>

<p>
Snow's package distribution and installation procedures
are simple and secure, because digital signatures are used to
sign and verify the files in the packages.  New packages can
be submitted to the package repository by anyone and without
prior registration.
</p>

<p>
Here are some central concepts of the Snow framework:
</p>

<ul>

<li>
Snow packages are distributed in compressed tar files
known as <b>snowballs</b>.

<li>
The <b>snowfort</b> is a central package repository which stores
snowballs.  It can be accessed with a web browser or
with a package management tool known as <b>snowman</b>.

</ul>
")

(define (do-tab-Home query)
  (send-page
   (decorated-page
    (plain-tag)
    ""
    "Snowfort"
    "selt1"
    (html-Home))))

(define (do-Main query)
  (do-tab-Home query))

(define (html-Installing)
  (let* ((name (generic-snow-package-name))
         (pkg (get-package (pack-dir) name '()))
         (versions (pkg-versions pkg))
         (version (if (pair? versions) (vpkg-version (car versions)) '(0 0 0)))
         (version-str (version->string version "."))
         (tgz-file (make-snowball-filename name version)))
    (string-append "
<h1>Installing Snow</h1>

The Snow framework is available in several implementations.
<b>Specialized</b> Snow implementations are tailor made for a given
Scheme system.  The <b>generic</b> Snow implementation works on most
Scheme systems but it does not achieve the same level of integration
with the host Scheme system.  It provides features which are helpful
for testing portability of newly written packages and it is useful
when a specialized implementation of Snow is not yet available for the
host Scheme system.

<h2>Specialized Snow implementations</h2>

<p>
The specialized Snow implementations are maintained elsewhere.  Some
Scheme systems have builtin support for the Snow framework while
others need a separate package to be configured and installed.
</p>

<p>
Please see the <a href=\"" (snowfort-path) "?tab=Links\">Links</a>
for a list of the known specialized Snow framework implementations.
</p>

<h2>Generic Snow implementation</h2>

<p>
The generic Snow works on several Scheme systems including Bigloo,
Chez, Chicken, Gambit, Gauche, Guile, Kawa, MzScheme, Scheme48, SCM, Scsh,
and STklos.  Work is underway to support more Scheme systems and to
improve the level of integration with the host Scheme system.
</p>

<h3>Prerequisites</h3>

<p>
By design, the generic Snow framework is implemented using the Snow
framework.  It is mainly written in Scheme and has few dependencies on
the host Scheme system and host operating system.  It does not depend
on utility programs of the host operating system during the normal
course of operation but the installation script does depend on a few
Unix utilities such as <code>sed</code> and <code>make</code>.  To
install on Windows the <a href=\"http://www.cygwin.com\">Cygwin</a>
and <a href=\"http://www.mingw.org\">MSYS/MinGW</a> systems can be used.
</p>

<p>
It is assumed that at least one of the supported Scheme systems is
installed on the target computer.  The installation script will
automatically detect the installed Scheme systems and perform a self
test to see which ones are compatible with Snow.  When there are
compatibility problems the script will flag any Scheme system that
should be reinstalled with a newer version.  The <code>--install-hosts</code>
option of the <code>configure</code> script can be used to automatically
install supported Scheme systems when needed.
</p>

<h3>Steps</h3>

<ol>

<li>
Download the most recent version of Snow: "
(download-form name version)
"
<li>
Run the <code>configure</code> script followed by
\"<code>make install</code>\".  For example:
</p>

<pre>" (html-escape (string-append "
   % gunzip -c " tgz-file " | tar xf -
   % cd " name "/" version-str "
   % ./configure --site-root=$HOME/.snow-site --install-hosts=yes
   % make install
   % export PATH=$HOME/.snow-site/current/bin:$PATH
   % ./test.scm
   % SNOW_HOST=guile ./test.scm
   % SNOW_HOST=all ./test.scm
"))
"</pre>

</ol>

<h3>Options</h3>

<p>
The configure script accepts these options:
</p>

<dl>

<dt><code>--site-root=</code><i>dir</i></dt>
<dd>
use <i>dir</i> as the directory where Snow
stores site-wide packages and the
Snow executables (<code>snow</code>, <code>snowman</code>, etc)
</dd>

<dt><code>--user-root=</code><i>dir</i></dt>
<dd>
use <i>dir</i> as the subdirectory of <code>$HOME</code>
where Snow stores user specific packages and configuration
files (including the certificates for signing and verifying
packages)
</dd>

<dt><code>--host=</code><i>host</i></dt>
<dd>
use <i>host</i> as the default host Scheme
system (<i>host</i> is one of <code>bigloo</code>, <code>chez</code>,
<code>chicken</code>, <code>gambit</code>, ...)
</dd>

<dt><code>--install-hosts=<i>yes-or-no</i></code></dt>
<dd>
force the automatic installation of the host Scheme systems supported
by the generic Snow that are not currently installed or whose
version is too old to support the generic Snow
</dd>

</dl>

<p>
The defaults are:
</p>

<p>
<code>site-root</code>     = <code>/usr/share/snow</code> <br>
<code>user-root</code>     = <code>.snow</code> <br>
<code>host</code>          = will be requested interactively <br>
<code>install-hosts</code> = no <br>
</dt>
</p>

<p>
The <code>--site-root</code> option is useful to experiment with Snow
before performing a site-wide installation (which normally requires
root privileges for writing into <code>/usr/share</code>).
</p>

<p>
The <code>--install-hosts</code> option is useful to test
the portability of newly written packages, particularly
in conjunction with the <code>SNOW_HOST=all</code> option.
</p>

<h3>Maintenance</h3>

<p>
After an initial manual installation of the generic Snow,
subsequent runs of the configure script will default the
configure options to the initial configure options.
New versions can be installed by using the
<code>snowman</code> package management program with the command
\"<code>snowman install snow-generic</code>\".  When a new
version is installed, it is necessary to reinstall all non-core
Snow packages because the new version of the generic Snow implementation
may use a different filesystem structure for installed packages.
</p>
")))

(define (do-tab-Installing query)
  (send-page
   (decorated-page
    (plain-tag)
    ""
    "Snowfort"
    "selt2"
    (html-Installing))))

(define (html-Documentation) (string-append "
<h1>Documentation</h1>

<h2>Snow framework specification v1.1</h2>

<h3>Notice</h3>

<p>
<i>
The primary objective of the Snow framework is to facilitate the
sharing of code among users of several Scheme systems.  For
this reason the current design often takes a \"least common
denominator\" approach to portability.  It would be unreasonably
restrictive to support all Scheme systems this way.  The Snow
framework targets the most popular and mature Scheme systems
which typically contain many more features than required by the
Scheme standard (e.g. modules, FFIs, libraries, etc).  In this context the
\"least common denominator\" is actually quite rich.  Be advised
that the Snow framework specification is expected to evolve to
take into account the evolution of these Scheme systems and their
level of conformance to the Scheme standards.  Experience with
the framework and feedback from users and implementors will also
fuel changes to the specification.  Please join the mailing
list if you are interested in participating in any aspect of
this effort (including contributing packages, suggesting improvements to
the Snow specification, improving the Snow implementations, etc).
</i>
</p>

<h3>Overview</h3>

<p>
A Snow package is a piece of software offering a certain
functionality.  This functionality is accessible to other
packages through the package's API.  A package may depend on the
functionality of other packages for its implementation (but circular
dependencies are forbidden).  The package is
identified by a name and version.
</p>

<p>
A package's API and dependencies are specified using the
<code>package*</code> special form.  This form includes the name
and version of the package, the set of named procedures, named
macros, and named record types that are provided by the package,
and the set of packages required for the implementation of the
package.  A source code file is composed of a
<code>package*</code> special form at the top followed by the
implementation of the package (procedure definitions and commands).
The name of the source code file containing the <code>package*</code>
special form is derived from the package name with the extension
<code>.scm</code>.
</p>

<p>
A package contains at least the source code file containing
the <code>package*</code> special form, and possibly other related
files, including source code files (in Scheme or other languages),
data files and documentation.
</p>

<p>
Here's the source code of a simple package, which is stored in the
file \"<code>simple.scm</code>\":
</p>

<pre>" (html-escape "
  (package* simple/v1.0.0

   (provide:
     (define (square x))
     (define (cube x))
     (define (inc)))

   (require: power/v1))

  (define (square x) (fast-expt x 2))

  (define (cube x) (fast-expt x 3))

  (define counter 0)

  (define (inc)
    (set! counter (+ counter 1))
    counter)
")
"</pre>

<p>
This file specifies the API of package <code>simple</code>
version 1.0.0 in the <code>provide:</code> clause of the
<code>package*</code> form.  The file also gives the
implementation of the API in the <code>require:</code> forms and
in the part of the file after the <code>package*</code>
form (this information is used by the generic Snow framework
implementation, and may be used by other Snow framework
implementations).
</p>

<p>
Package <code>simple</code> version 1.0.0 provides three
procedures: <code>square</code>, <code>cube</code> and
<code>inc</code>.  The package <code>power</code> version 1 is
required in the implementation of those procedures (it provides
the procedure <code>fast-expt</code> needed by the implementation
of the procedures <code>square</code> and <code>cube</code>).
The source code of package <code>power</code>, stored in the file
\"<code>power.scm</code>\", could be:
</p>

<pre>" (html-escape "
  (package* power/v1.0.0
   (provide:
     (define (fast-expt x n))))

  (define (fast-expt x n)
    (cond ((= n 0)
           1)
          ((odd? n)
           (* x (fast-expt x (- n 1))))
          (else
           (fast-expt (* x x) (quotient n 2)))))
")
"</pre>

<h3>Package names</h3>

<p>
The name of a package is a symbol.  Because the name is also used as a
basis for the name of the file containing the <code>package*</code>
special form and portability of file names is an issue, the package
name is restricted as follows:
</p>

<ol>

<li>
It must be composed of lowercase letters (a-z), digits (0-9),
the special characters \"_\" (underscore) and \"-\" (dash).

<li>
It must start with a lowercase letter or an underscore.

</ol>

<h3>Package version numbers</h3>

<p>
The version number of a package is a symbol containing 3 non-negative
integer fields separated by dots prefixed with \"v\" i.e. vX.Y.Z .
Field X is the major version number, field Y is the minor version
number, and field Z is the build number.  When a new version of a
package is released one of these fields is incremented and the ones
that follow, if any, are set to zero.  For proper operation of the
system a package writer must follow these rules when assigning a
version number to a package P:
</p>

<ol>

<li>
Z is incremented when the API is identical to the previous
version.  This is useful when the implementation of the package
changes (for performance reasons, or for repairing bugs).  For
example, if we assume there are no bugs in versions 2.1.5 and
2.1.9 of package P, then they can be used interchangeably.  If
there is a choice between packages with identical major and minor
version numbers, the one with the greater build version number is
normally preferred because it usually includes improvements to
the implementation.

<li>
Y is incremented when the API changes in a backward compatible
way.  This means that the new API offers a strict superset of the
API of the previous version.  For example, version 2.3.0 of P can
be used instead of version 2.Y.Z for Y &lt;= 2 and any Z, because it
offers strictly more functionality.  If there is a choice between
packages with identical major version numbers but different minor
version numbers, the one with the greater minor version number is
normally preferred because it usually includes improvements to
the implementation.

<li>
X is incremented when the API changes in a non-backward
compatible way.  This means that the API is not a strict superset
of the API of the previous version.  In other words the two
versions are not interchangeable in all contexts (but it may be
the case that for some uses of package P the two versions are
interchangeable, i.e. when only the functionality in the
intersection of their APIs is used).  It is possible that two
different major versions are completely incompatible (for example
when they are designed independently).  Because of the general
incompatibility of versions with different major version numbers,
it is highly recommended that package writers hold off changes in
the major version number until absolutely necessary.  It is
better to adopt a practice of deprecating parts of the
API (i.e. marking them as eventually removed from the API) and
only removing them, with a change in major version number, when a
reasonable amount of time has passed.  This leaves time for the
maintainers of other packages which depend on P to modify their
implementation to avoid the deprecated parts.

</ol>

<h3>Grammar</h3>

<p>
The file containing the <code>package*</code> special form must obey
the following syntax:
</p>

<pre>" (html-escape "
<package-file> --> <package-form> <package-file-body>

<package-form> --> ( package* <package-name-with-full-version>
                     <package-form-body> )

<package-form-body> --> <package-attributes> <implementation-requirements>

<package-attributes> --> [ <provide-form> ] <package-meta-data>*

<package-meta-data> --> ( maintainer: <string>+ )
                     |  ( author: <string>+ )
                     |  ( homepage: <string>+ )
                     |  ( description: <string>+ )
                     |  ( keywords: <symbol>+ )
                     |  ( license: <symbol>+ )

<provide-form> --> ( provide: <provide-form-body> )

<provide-form-body> --> <interface-definitions>

<interface-definitions> --> <interface-def>*

<interface-def> --> <procedure-prototype>
                 |  <macro-def>
                 |  <record-def>

<procedure-prototype> --> ( define ( <var-name> <R5RS def formals> ) )
                       |  ( define* ( <var-name> <prototype formals> ) )

<procedure-def> --> ( define ( <var-name> <R5RS def formals> )
                       <body> )
                 |  ( define* ( <var-name> <SRFI 89 extended def formals> )
                       <body> )

<macro-def> --> ( define-syntax <macro-name>
                   <expression> )
             |  ( define-macro ( <macro-name> <R5RS def formals> )
                   <body> )
             |  ( define-macro* ( <macro-name> <SRFI 89 extended def formals> )
                   <body> )

<record-def> --> ( define-record* <record-name>
                    <record-body> )

<record-body> --> <record-clause>*

<record-clause> --> <record-field>

<record-field> --> <field-name>

<implementation-requirements> --> <requirements>

<requirements> --> <require-form>*

<require-form> --> ( require: <package-name-with-required-version>
                      <require-form-body> )

<require-form-body> --> <empty>

<package-file-body> --> <package command or def>*

<package command or def> --> <expression>
                          |  <implementation-def>
                          |  <include>
                          |  <test>

<implementation-def> --> <var-def>
                      |  <macro-def>
                      |  <record-def>

<var-def> --> ( define <var-name> <expression> )
           |  <procedure-def>

<include> --> ( include* <unix-relative-path-string> )

<test> --> ( test* <test-body> )

<test-body> --> <package-file-body-containing-expect-expressions>

<package-file-body-containing-expect-expressions> --> <package-file-body>

<expect-expression> --> ( expect* <expect-test> )

<expect-test> --> <expression>

<field-name> --> <name>
<var-name> --> <name>
<package-name> --> <name>
<package-name-with-full-version> --> <package-name>/<full-version>
<package-name-with-required-version> --> <package-name>/<required-version>
<macro-name> --> <name>
<record-name> --> <name>
<name> --> <identifier>

<full-version> --> v<N>.<N>.<N>
<required-version> --> v<N>
                    |  v<N>.<N>
                    |  v<N>.<N>.<N>

<prototype formals> --> <SRFI 89 extended def formals>
<SRFI 89 extended def formals> --> <extended def formals> from SRFI 89
<R5RS def formals> --> <def formals> from R5RS
")
"</pre>

<h4>Package meta-data</h4>

<p>
The <code>&lt;package-meta-data&gt;</code> forms give various meta-data
of the package: the maintainer(s), the author(s), the homepage(s), the description
of the package, the license, and the keywords associated with the package.
Although this meta-data is optional, providing it is
recommended since it helps to classify the packages on the
snowfort, and to assign maintenance and authorship
responsibilities.  The names of the maintainer(s) and
author(s) are given in the same syntax as the names of
certificates used for signing packages (full name followed by
obfuscated email address, e.g. \"John Smith &lt;js at
acme.com&gt;\").  The homepages are URLs connected with the
package.  The first string of the package description
should fit on one line and summarize the function of the package.
Short symbols should be used to denote the keywords associated with
the package and when possible existing keywords should
be used (e.g.  <code>net</code>, <code>crypto</code>,
<code>i/o</code>, etc).  Here's an example using all the meta-data
tags:
</p>

<pre>" (html-escape "
  (package* hello/v1.0.0
   (provide:
     (define (hello port)))

   (maintainer: \"Scheme Now! <snow at iro.umontreal.ca>\")
   (author: \"Marc Feeley <feeley at iro.umontreal.ca>\")
   (homepage: \"http://snow.iro.umontreal.ca\")
   (description: \"Display classic greeting.\")
   (keywords: example i/o)
   (license: lgpl/v2.1))

  (define (hello port)
    (display \"hello\" port))
")
"</pre>

<h4>Formal parameters of procedures and macros</h4>

<p>
The <code>&lt;SRFI 89 extended def formals&gt;</code> obeys the
syntax and semantics of <a
href=\"http://srfi.schemers.org/srfi-89/srfi-89.html\">SRFI
89 (Optional positional and named parameters)</a>.  In the case
of a <code>&lt;prototype formals&gt;</code>, the default
expression of any optional parameter in the parameter list must
be replaced with an underscore (the default expression must
however be specified fully in the
<code>&lt;package-file-body&gt;</code>).  For example:
</p>

<pre>" (html-escape "
  (package* hello/v1.0.0
   (provide:
     (define* (hello (port _)))))

  (define* (hello (port (current-output-port)))
    (display \"hello\" port))
")
"</pre>

<h4>Definition of procedures, macros and records</h4>

<p>
The <code>define</code> and <code>define*</code> forms must be
used to define toplevel procedures and variables in
<code>&lt;package-file-body&gt;</code> and
<code>&lt;interface-definitions&gt;</code>.  It is good style to
use <code>define*</code> only when there are optional parameters.
<p>

<p>
Macros are defined with the <code>define-syntax</code>,
<code>define-macro</code>, and <code>define-macro*</code> forms,
which must appear at the toplevel of
<code>&lt;package-file-body&gt;</code> or
<code>&lt;interface-definitions&gt;</code>.  The body of the
<code>define-syntax</code> form must be a
<code>syntax-rules</code> form.  The lexical environment of the
<code>syntax-rules</code> depends on the package that require
it (because the macro definition is effectively copied to the top
of each package that requires the package providing the macro
definition).  The body of the <code>define-macro</code> and
<code>define-macro*</code> forms is
an expression evaluated at macro expansion time to generate the
S-expression which replaces the macro call.  The evaluation
environment of the body is the R4RS global environment extended
with the macro parameters.  The global environment must not be
mutated.  Package writers are advised that <code>define-syntax</code> is
currently less portable than <code>define-macro</code>.  To maximize
portability macros should be defined with <code>define-macro</code> and
<code>define-macro*</code> when possible.
</p>

<p>
Records are defined with the <code>define-record*</code> form,
which must appear at the toplevel of
<code>&lt;package-file-body&gt;</code> or
<code>&lt;interface-definitions&gt;</code>.  The arguments are,
in order, the name of the record type and the name of each field.
The <code>define-record*</code> form defines a record
constructor, a type predicate, and getter and setter procedures
for each field.  The names of these procedures are generated as
follows for the declaration <code>(define-record* <i>R</i>
<i>A</i> <i>B</i>)</code>:
</p>

<ul>

<li> <code>(make-<i>R</i> a b)</code>: construct an instance of record
type <i>R</i> whose fields are initialized with the parameters.

<li> <code>(<i>R</i>? obj)</code>: return a boolean indicating if
the object <code>obj</code> is an instance of record
type <i>R</i>.

<li> <code>(<i>R</i>-<i>A</i> r)</code>: return the content of
field <i>A</i> of <code>r</code> which must be an instance of record
type <i>R</i>.

<li> <code>(<i>R</i>-<i>A</i>-set! r x)</code>: stores <code>x</code> in
field <i>A</i> of <code>r</code> which must be an instance of record
type <i>R</i>.  An unspecified result is returned.

<li> <code>(<i>R</i>-<i>B</i> r)</code>: getter of field <code>B</code>.

<li> <code>(<i>R</i>-<i>B</i>-set! r x)</code>: setter of field <code>B</code>.

</ul>

<p>
The <code>provide:</code> clause of the <code>package*</code>
form defines the API of the package.  It can contain
<code>define</code> forms, <code>define*</code> forms,
<code>define-syntax</code> forms, <code>define-macro</code> forms,
<code>define-macro*</code> forms, and <code>define-record*</code>
forms.  In the case of <code>define</code>, <code>define*</code>,
<code>define-syntax</code>, <code>define-macro</code> and
<code>define-macro*</code> forms, the identifier being defined is
exported by the package.  In the case of <code>define-record*</code>
forms, all the procedures generated are exported by the package.  All
names exported by a package are accessible in the
<code>&lt;package-file-body&gt;</code>.
</p>

<p>
Here is an example combining various forms of definitions:
</p>

<pre>" (html-escape "
  (package* mixed/v1.0.0
   (provide:

     (define* (put (x _) (y _)))
     (define (get))
     (define (add))
     (define (sub))

     (define-record* point x y)

     (define-syntax pop
       (syntax-rules ()
        ((pop var)
         (let ((top (car var)))
           (set! var (cdr var))
           top))))

     (define-macro (push val var)
       `(set! ,var (cons ,val ,var)))))

  (define stack '())

  (define* (put (x 0) (y 0))
    (push (make-point x y) stack))

  (define (get)
    (pop stack))

  (define (binary-op fn)
    (let* ((b (pop stack))
           (a (pop stack)))
      (push (make-point (fn (point-x a) (point-x b))
                        (fn (point-y a) (point-y b)))
            stack)))

  (define (add) (binary-op +))
  (define (sub) (binary-op -))
")
"</pre>

<h4><code>cond-expand</code> form</h4>

<p>
The <code>cond-expand</code> form specified by <a
href=\"http://srfi.schemers.org/srfi-0/srfi-0.html\">SRFI
0 (Feature-based conditional expansion construct)</a> can appear
inside a <code>&lt;package-form-body&gt;</code>,
<code>&lt;provide-form-body&gt;</code>, and
<code>&lt;package-file-body&gt;</code>.  Inside a
<code>&lt;package-form-body&gt;</code> and
<code>&lt;provide-form-body&gt;</code>, the
<code>cond-expand</code> forms may only test the host Scheme
system (currently one of the symbols
<code>bigloo</code>,
<code>chez</code>,
<code>chicken</code>,
<code>gambit</code>,
<code>gauche</code>,
<code>guile</code>,
<code>kawa</code>,
<code>larceny</code>,
<code>mit</code>,
<code>mzscheme</code>,
<code>petite</code>,
<code>scheme48</code>,
<code>scm</code>,
<code>scsh</code>,
<code>sisc</code>,
<code>stalin</code>,
and <code>stklos</code>).  Inside a
<code>&lt;package-file-body&gt;</code> the
<code>cond-expand</code> forms may test the host Scheme
system and any other feature.
</p>

<h4><code>include*</code> form</h4>

<p>
When used in a <code>&lt;package-file-body&gt;</code>, the
<code>include*</code> form is equivalent to a <code>begin</code>
form whose body is the sequence of expressions in the file
referred to by <code>&lt;unix-relative-path-string&gt;</code>.  A
<code>&lt;unix-relative-path-string&gt;</code> is a string
expressing with the Unix notation a path relative to the current
file.  This path will be converted to the OS specific path when
the file is included.  The path must not be empty or start or end
with a slash.  Repeated slashes are treated like a single slash.
For portability the path must contain only the following ASCII
characters: lowercase letters, digits, \".\", \"/\", \"_\",
\"-\", and \" \" (space).
</p>

<h4><code>test*</code> and <code>expect*</code> forms</h4>

<p>
The <code>test*</code> form can be used to include self test code
within the package body.  The definitions and expressions inside
<code>test*</code> forms are normally ignored.  However,
<code>test*</code> forms are transformed into <code>begin</code> forms
when they are activated in a Snow framework implementation specific
way (for example the <code>SNOW_TEST</code> environment variable
of the generic Snow framework implementation).
</p>

<p>
The <code>expect*</code> form can only appear inside
<code>test*</code> forms.  The <code>expect*</code> form
takes a single argument, an expression whose value is expected
to be non-false.  The test is said to have failed when the
expression evaluates to <code>#f</code> or an exception is raised.
The result of the self tests (number of failed
tests, etc) is reported in a system dependent way.
</p>

<p>
For example:
</p>

<pre>" (html-escape "
  (package* math/v1.0.0
   (provide: (define (square x))))

  (define (square x) (* x x))

  (test*
   (define (dec n) (- n 1))
   (expect* (= 100 (square 10)))
   (expect* (= 81 (square (dec 10)))))
")
"</pre>

<h4>Package dependencies</h4>

<p>
The packages required by a package are indicated in
<code>require:</code> forms (one per required package).  Each
package requirement specifies with a
<code>&lt;required-version&gt;</code> a set of package versions
that are acceptable.  Only one version from this set will be
linked with the package (i.e. the <i>linked</i> package).  The
major version of the linked package will match the major version
of the <code>&lt;required-version&gt;</code>.  If the
<code>&lt;required-version&gt;</code> has a minor version number
then the linked package will have at least that minor version
number.  If the <code>&lt;required-version&gt;</code> has minor
and build version numbers then the linked package will have at
least that minor version number, and if the minor versions are
equal, at least that build version number.  Among the versions of
a package that are installed, the one with the highest acceptable
version number will be used.  It is an error if none of the
installed versions of a package are acceptable.
</p>

<h2>Package distribution specification</h2>

<h3>Snowball structure</h3>

<p>
In general Snow packages are composed of a set of files.  These files
are packaged into a compressed archive (<b>snowball</b>).  The
snowball for package <i>P</i> version <i>vX.Y.Z</i> is a file in
\".tar.gz\" format containing all the files of the package organized in
a hierarchy.  It contains the root directory <i>P/vX.Y.Z</i> in which
there is a <code>snow</code> subdirectory and possibly subdirectories
associated with each specialized Snow implementation
(i.e. <code>bigloo</code>, <code>chez</code>, <code>chicken</code>,
<code>gambit</code>, etc).
</p>

<p>
The <code>snow</code> subdirectory contains at least the file
<i>P</i><code>.scm</code> which contains the
<code>package*</code> special form (with the package name
<i>P/vX.Y.Z</i>).  Generic data files needed by the package are
normally put in the <code>snow</code> subdirectory.  The
<code>snow</code> subdirectory may also contain the executable
scripts <code>install-script.sh</code> and
<code>install-script.bat</code> (either one or both).  If
present, the script will be run when the package is
installed (<code>install-script.sh</code> is run on Unix
environments and <code>install-script.bat</code> is run on
Windows environments).  The script is run with a current
directory equal to the <code>snow</code> subdirectory.  The
package installation will be aborted if the script exits with a
non zero status.
</p>

<p>
For example, here are the steps for creating the snowball
<code>mypkg-v1_0_0.tar.gz</code>:
</p>

<pre>" (html-escape "
  % mkdir mypkg
  % mkdir mypkg/v1.0.0
  % mkdir mypkg/v1.0.0/snow
  % cat > mypkg/v1.0.0/snow/mypkg.scm
  (package* mypkg/v1.0.0
   (provide: (define (hello))))
  (define (hello) (display \"hello\"))
  % tar cf mypkg-v1_0_0.tar mypkg/v1.0.0
  % gzip -9 mypkg-v1_0_0.tar
")
"</pre>

<p>
The specialized subdirectories of the root directory are reserved for
the use of the corresponding specialized Snow implementation.  Please
consult the documentation of those implementations for details.
</p>

<h3>Snowfort protocol</h3>

<p>
The central package repository (<b>snowfort</b>) will use the
filename <i>P-vX_Y_Z</i><code>.tgz</code> to store the snowball of
package <i>P</i> version <i>vX.Y.Z</i>.  For convenience it is not
necessary to include the package version number subdirectory when
uploading a snowball to the snowfort.  However, when a snowball is
downloaded from the snowfort, the snowball format specified above will
be used (if needed the snowball structure will be changed to put all
files in the directory <i>P/vX.Y.Z</i>).
</p>

<p>
Packages can be uploaded,
downloaded and browsed manually (with a web browser and utility
programs such as
<code>wget</code> and <code>curl</code>) or with the Snow package manager
utility (<b>snowman</b>) which includes package signing and
verification features.  The HTTP protocol is used to interact with the snowfort.
A transaction with the snowfort is implemented by an HTML form with an
\"operation\" field indicating the operation requested.  Three operations
are supported: \"list\" (get the list of packages), \"download\"
(get a snowball from the snowfort), and \"upload\" (transfer a snowball
to the snowfort).
<p>

<p>
Here is how a minimal snowball for the package
\"mypkg\" given above can be created, uploaded and downloaded manually
from the snowfort, and how a list of packages can be obtained:
</p>

<pre>" (html-escape (string-append "
  % mkdir mypkg
  % mkdir mypkg/snow
  % cat > mypkg/snow/mypkg.scm
  (package* mypkg/v1.0.0
   (provide: (define (hello))))
  (define (hello) (display \"hello\"))
  % tar cf mypkg.tar mypkg
  % gzip -9 mypkg.tar
  % curl -s -F operation=upload -F snowball=@mypkg.tar.gz " snowfort-url "
  <html><!-- upload success
  snowball mypkg-v1_0_0.tgz uploaded successfully!
  ...
  % curl -s -F operation=download -F pkg=mypkg/v1.0.0 " snowfort-url " | gunzip -c | tar tf -
  mypkg/v1.0.0/
  mypkg/v1.0.0/snow/
  mypkg/v1.0.0/snow/mypkg.scm
  % curl -s -F operation=list " snowfort-url "
  (\"extio\" (package* extio/v1.0.0 ...
  (\"cert\" (package* cert/v1.0.0 ...
  ...
"))
"</pre>

<h1>Using Snow</h1>

<h2>Specialized Snow implementations</h2>

<p>
Each specialized Snow implementation defines how Snow is used with
that implementation.  It is expected that the usage procedures will
roughly follow those of the generic Snow implementation with
extensions that address implementation dependent issues.
</p>

<p>
Please see the <a href=\"" (snowfort-path) "?tab=Links\">Links</a>
for a list of the known specialized Snow framework implementations.
</p>

<h2>Generic Snow implementation</h2>

<p>
With the generic Snow framework, package management (installation,
uploading, signing, etc) is performed with the <code>snowman</code>
utility program.  The usage summary follows:
</p>

<pre>" (html-escape "
Usage:

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
       download: --verify|--skip-verify --highest|--exact
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
"</pre>

<h3>Installing packages with <code>snowman</code></h3>

<p>
The \"<code>snowman install</code> <i>P</i>\" command is the easiest way
to install package <i>P</i> from the snowfort.  An HTTP request will be
sent to the snowfort to get the snowball of the highest available version
of package <i>P</i>, the files in the snowball will be checked for
valid digital signatures, and finally the snowball will be installed
in the file system.  Multiple packages can be installed by putting all
of the package names on the command line.  For example:
</p>

<pre>" (html-escape "
  % snowman install simple power
")
"</pre>

<p>
A version of package <i>P</i> can be requested by adding the version
to the package name, i.e. <i>P/vX</i>, <i>P/vX.Y</i>, or
<i>P/vX.Y.Z</i>.  By default, the highest available version
compatible with the requested version will be installed.  For example:
</p>

<pre>" (html-escape "
  % snowman install simple/v1
")
"</pre>

<p>
To install a specific version the option <code>--exact</code> must be used
and the full version must be requested, i.e. <i>P/vX.Y.Z</i>.
For example:
</p>

<pre>" (html-escape "
  % snowman install --exact simple/v1.0.0
")
"</pre>

<p>
The installation will be aborted if some files in the snowball
have invalid signatures or they are signed by people you have not
chosen to trust by adding their certificate to your trusted
certificate database.  Note that the certificate of \"Scheme Now!
&lt;snow at iro.umontreal.ca&gt;\" is implicitly trusted so all
of the Snow core packages can be installed securely with no extra
configuration steps.  To skip the signature verification, use the
option <code>--skip-verify</code>.  For example:
</p>

<pre>" (html-escape "
  % snowman install --skip-verify simple
")
"</pre>

<p>
The package is normally installed in the user specific directory which
was chosen when the generic Snow framework implementation was installed
(<code>$HOME/.snow</code> by default).  Use the option
<code>--site</code> to install in the site-wide directory, which
is typically accessible to all users and is
<code>/usr/share/snow</code> by default.  For example:
</p>

<pre>" (html-escape "
  % snowman install --site simple
")
"</pre>

<p>
If a snowball is stored in a file <i>F</i> with extension
<code>.tgz</code> or <code>.tar.gz</code>, then it can be
installed with the command \"<code>snowman install</code>
<i>F</i>\".  This is useful for package maintainers to locally
test the installation of a snowball before it is uploaded to the
snowfort.  It is also useful in combination with the
\"<code>snowman download</code> <i>P</i>\" command which only
downloads the snowball from the snowfort.  For example:
</p>

<pre>" (html-escape "
  % snowman download simple
  % snowman install simple-v1_0_0.tgz
")
"</pre>

<h3>Package signing, verification and uploading with <code>snowman</code></h3>

<p>
The Snow framework uses
<a href=\"http://en.wikipedia.org/wiki/Digital_signature\">digital signatures</a>
to authenticate snowballs.  In this approach each Snow
package maintainer is assigned a certificate pair composed of a private
and public certificate.  The package maintainer must keep the private
certificate secret and give the public certificate to anyone needing
to authenticate the maintainer's snowballs (this can be done in person,
via email, putting it on a public web page, etc).  The package maintainer
uses the private certificate to sign snowballs.  The installers of
a snowball use the maintainer's public certificate to verify the
snowball's authenticity.  It is the responsibility of the installers
to take the appropriate measures to ensure that the public certificate
they obtain really is assigned to the maintainer.
</p>

<p>
To simplify the management of certificates by <code>snowman</code>
the Snow framework maintains three certificate databases:
</p>

<ol>

<li> A database of public certificates trusted by the site
(<code>/usr/share/snow/.publ-certs</code> by default).
This database is selected with the <code>--site</code> option.

<li> A database of public certificates trusted by the user
(<code>$HOME/.snow/.publ-certs</code> by default).
This database is selected with the <code>--user</code> option,
which is the default.

<li> A database of the certificate pairs of the user
(<code>$HOME/.snow/.priv-certs</code> by default).
This database is selected with the <code>--sign</code> option.
This database is encrypted for security reasons.

</ol>

<p>
Several commands are available to manage the certificate databases.
The \"<code>snowman cert-create</code> <i>P</i>\" command creates
a new certificate pair and stores it in the user's database of
certificate pairs.  The certificate is identified with a
name, an optional comment, and an email address (e.g.
\"Marc Feeley &lt;feeley at iro.umontreal.ca&gt;\").  The command
will request this information interactively.  The size of the RSA key
and the certificate's validity period must also be entered.  For example:
</p>

<pre>" (html-escape "
  % snowman cert-create
  Enter certificate identification information:
  Full Name     (e.g. Joe Smith): Eliza Hacker
  Comment       (e.g. secondary): 
  Email Address (e.g. js@foo.us): eliza@gmail.com  
  Enter RSA key size in bits (512=default, 1024 or 2048): 1024
  Enter validity period, e.g. 14d (14 days) or 1y (1 year=default): 2y
  Generating an RSA key pair (this may take a few minutes)...
  .+++++++++
  +++++
  .++++++
  You do not have a private certificate file.  This file is an encrypted
  file which stores the private certificate(s) which you can use to sign
  the files in Snow packages.  It is encrypted for security, so that only
  you can use the certificate(s).
  
  You must enter a password for encrypting the private certificate file.
  Please enter access control password for file /u/eliza/.snow/.priv-certs:
  Password: zzz111@@@
  Please enter the password again.
  Password: zzz111@@@
")
"</pre>

<p>
Note that the user's database of certificate pairs is password
protected.  The password must be at least 8 characters long and
contain at least one letter, one digit, and one special
character.  To change the password the \"<code>snowman
cert-passwd</code>\" command must be used.
</p>

<pre>" (html-escape "
  % snowman cert-passwd
  Please enter access control password for file /u/eliza/.snow/.priv-certs:
  Password: zzz111@@@
  Please enter new access control password for file /u/eliza/.snow/.priv-certs:
  Password: ejkdi34!
  Please enter the password again.
  Password: ejkdi34!
  Password was changed successfully.
")
"</pre>

<p>
The \"<code>snowman cert-list</code>\" command displays the content
of one of the certificate databases (the user's public certificates
by default).  Use the <code>--site</code> and <code>--sign</code> options
to select a different database.  For example:
</p>

<pre>" (html-escape "
  % snowman cert-list --sign
  Please enter access control password for file /u/eliza/.snow/.priv-certs:
  Password: ejkdi34!
    1) \"Eliza Hacker <eliza at gmail.com>\"
       fingerprint: 5e57ea66239b1909c6f3a007884f931c
       purpose: (snow)
       authenticity: *** not signed ***
")
"</pre>

<p>
The \"<code>snowman cert-export</code>\" command displays a
public certificate in ASCII form.  Conversely, the
\"<code>snowman cert-import</code>\" command adds to a
certificate database the public certificate given on the command
line.  A package maintainer must use the \"<code>snowman
cert-export --sign</code>\" command to produce the public
certificate in a form that can be distributed to the package
installers.  The ASCII form of a certificate always starts with
the characters \"Y2Vyd\". The package installers must use the
\"<code>snowman cert-import Y2Vyd...</code>\" command to add
the certificate to their database of trusted public certificates.
For example:
</p>

<pre>" (html-escape "
  % snowman cert-export --sign
  Please enter access control password for file /u/eliza/.snow/.priv-certs:
  Password: ejkdi34!
  The certificate \"Eliza Hacker <eliza at gmail.com>\" will be exported.
  Y2VydC12MQoiRWxpemEgSGFja2VyIDxlbGl6YSBhdCBnbWFpbC5jb20+IgooIlJlUjE2dz09IiAiU2FiYzZ3PT0iKQooc25vdykKKDEwMjQgIkFYSkhpWlI2Nzc2MnFqQ0tqMkNPejdDM24wc3lIRkZyeFVvcVBOdFVTWFBnQWluMW5GVVpVL1h3ZjhocCszOTM1ejNucVNvblNxMEd1QmJ6cDVwK0NJL3BPRms5UHhxSkZobUNNS1c4UkoxaW5DNGJnTytXZ1Z3WU9Eam9jVEJBcU9qRm5maDgwdk5YNXNXeUZscDVTOGgwSUJMYnljdUhCUi9EMUN5SWlBM2YiICJBUUFCIikKI2YK
  % snowman cert-import Y2VydC12MQoiRWxpemEgSGFja2VyIDxlbGl6YSBhdCBnbWFpbC5jb20+IgooIlJlUjE2dz09IiAiU2FiYzZ3PT0iKQooc25vdykKKDEwMjQgIkFYSkhpWlI2Nzc2MnFqQ0tqMkNPejdDM24wc3lIRkZyeFVvcVBOdFVTWFBnQWluMW5GVVpVL1h3ZjhocCszOTM1ejNucVNvblNxMEd1QmJ6cDVwK0NJL3BPRms5UHhxSkZobUNNS1c4UkoxaW5DNGJnTytXZ1Z3WU9Eam9jVEJBcU9qRm5maDgwdk5YNXNXeUZscDVTOGgwSUJMYnljdUhCUi9EMUN5SWlBM2YiICJBUUFCIikKI2YK
  Importing certificate \"Eliza Hacker <eliza at gmail.com>\"
       fingerprint: 5e57ea66239b1909c6f3a007884f931c
       purpose: (snow)
       authenticity: *** not signed ***
  Add certificate \"Eliza Hacker <eliza at gmail.com>\" (y/n)? y
  Certificate was imported successfully.
")
"</pre>

<p>
The \"<code>snowman cert-list</code>\" command displays the
content of a certificate database.
The \"<code>snowman cert-remove</code>\" command must be used to
remove a public certificate from a certificate database.
For example:
</p>

<pre>" (html-escape "
  % snowman cert-list
    1) \"Eliza Hacker <eliza at gmail.com>\"
       fingerprint: 5e57ea66239b1909c6f3a007884f931c
       purpose: (snow)
       authenticity: *** not signed ***
  % snowman cert-remove
  The certificate \"Eliza Hacker <eliza at gmail.com>\" will be removed.
  Removing certificate \"Eliza Hacker <eliza at gmail.com>\"
       fingerprint: 5e57ea66239b1909c6f3a007884f931c
       purpose: (snow)
       authenticity: *** not signed ***
  Are you sure (y/n)? y
  Certificate was removed successfully.
")
"</pre>

<p>
Each file contained in a snowball may be signed using one or more
certificates, or be left unsigned.  The signatures for file
<i>F</i> are stored in file <i>F</i><code>.sig</code> (the
<code>.sig</code> file extension is reserved for signature files
and should not be used for regular files).  The signature files
must be created in a package's directory before that package's
snowball is created.  This is done with the \"<code>snowman
sign</code> <i>P</i>\" command where <i>P</i> is the package's
directory.  For example:
</p>

<pre>" (html-escape "
  % mkdir mypkg
  % mkdir mypkg/v1.0.0
  % mkdir mypkg/v1.0.0/snow
  % cat > mypkg/v1.0.0/snow/mypkg.scm
  (package* mypkg/v1.0.0
   (provide: (define (hello))))
  (define (hello) (display \"hello\"))
  % snowman sign mypkg
  Please enter access control password for file /u/eliza/.snow/.priv-certs:
  Password: ejkdi34!
  The certificate \"Eliza Hacker <eliza at gmail.com>\" will be used for signing.
  Signing mypkg/v1.0.0/snow/mypkg.scm -- done!
  % tar cf mypkg-v1_0_0.tar mypkg/v1.0.0
  % gzip -9 mypkg-v1_0_0.tar
  % gunzip -c mypkg-v1_0_0.tar.gz | tar tf -
  mypkg/v1.0.0/
  mypkg/v1.0.0/snow/
  mypkg/v1.0.0/snow/mypkg.scm
  mypkg/v1.0.0/snow/mypkg.scm.sig
")
"</pre>

<p>
The \"<code>snowman sign</code> <i>P</i>\" command creates or
updates the signature files for all the regular files in directory
<i>P</i>.  If a signature file already exists for a file, then a
signature is added to the signature file and any inconsistent signature
is removed from the signature file.  It is thus possible for several
maintainers to sign the files in a snowball before it is uploaded
to the snowfort (the first maintainer signs the files and sends a snowball
to the second maintainer which also signs the files, etc).
</p>

<p>
The verification of the files in a snowball is done by default by
the \"<code>snowman install</code> <i>P</i>\" command.  The
verification of regular files can also be done explicitly using
the \"<code>snowman verify</code> <i>F</i>\" command.  If
<i>F</i> is a regular file then only that file is verified (using
the signature file <i>F</i><code>.sig</code>).  If <i>F</i> is a
directory then all the regular files it contains are verified.
For example:
</p>

<pre>" (html-escape "
  % snowman verify mypkg
  mypkg/v1.0.0/snow/mypkg.scm -- signed by: (\"Eliza Hacker <eliza at gmail.com>\")
  mypkg passed verification.
")
"</pre>

<p>
The verification will fail if any of the files does not have
a valid signature.  In this case the command may suggest
which certificates can be imported into the trusted certificate
database (with the \"<code>snowman cert-import Y2Vyd...</code>\"
command) to allow the verification to succeed.  <b>This should be
done with caution.  You should independently verify the
authenticity of the certificate before importing it.</b>
For example:
</p>

<pre>" (html-escape "
  % snowman verify mypkg
  mypkg/v1.0.0/snow/mypkg.scm -- invalid signature by: (\"Marc Feeley <feeley at iro.umontreal.ca>\")
  mypkg failed verification.
  The verification may succeed if one of the following certificates is imported:
    1) \"Marc Feeley <feeley at iro.umontreal.ca>\"
       fingerprint: 63c885d801e3bcfe53e773dc199ec481
       purpose: (snow)
       authenticity: issued by \"Scheme Now! CA <snow at iro.umontreal.ca>\"
       ASCII: Y2VydC12MQoiTWFyYyBGZWVsZXkgPGZlZWxleSBhdCBpcm8udW1vbnRyZWFsLmNhPiIKKCJSZGNNYVE9PSIgIlI3Zy82UT09IikKKHNub3cpCig1MTIgIkFWbnRqQ3ZTL3J4cXJRM0hOOHdaSE9iN3cyRG1DTEpqcXIvRFJDS1pLWVJNRjJaYUdScGRiSXpzWGovS2pFVUVzMHZEN1d2ek45cVlOVHdSSUNQYlVXOD0iICJBUUFCIikKKChjZXJ0LXYxICJTY2hlbWUgTm93ISBDQSA8c25vdyBhdCBpcm8udW1vbnRyZWFsLmNhPiIgKCJSZGNIVlE9PSIgIldLTUtWUT09IikgKHNub3cgY2EpICgxMDI0ICJBZFVPZExoTENXdzBTNHVDS05uZlZ5TzR5VXZBMDBaR1lxOHBKNlBaZFRmNlZ5YnZ5Tm5DYjdZd0krcnlFQ3kyUGRIRmRIVTZoWHpBb0lLazErKytqTi9LV1J1cElIOW9CdjhVYXhZald1ZE1KTkJVeDJBTXBTVFdxYXRYbFVzVXA2L0tndFhtRHpDRnUycXk1RXVJejhUeG51QXBXK1VJemN0dUxLNTlqajBwIiAiQVFBQiIpICNmKSBzaGEtMSAiYTdhMDA0NmIzNjQwYWM4MGIzNjkxZjljMTlhZWZmMTkyMzU5ZWYyYyIgIjRkNjg3ZjVkNDQwMzZjM2M4M2YzZjk4ZTQzOWIxMjlhN2M1NzYzMGYzNWY1YTY2YjlkZjMxMjZmY2VlMmQxMTJjNDU5MWUyNzA4NDY4NmE5OWU2MDQxN2IxNWJlNzQ3ZmRkZTRkOWUyOGJkMmFhNDdiMGQ4NDMyNmI2ODZhNWFlOTkwN2UzNGNkZDBkNDcxNWQzOGM1YjA0YWZiZjNmYjI0OTAxY2FjMjQwN2RmMWIzOWViYjEzMmEwNTZjMzU0NTYxMjcyZmFkYmQ0MDA2ODhjNjAzNTY4NDUxZDNlMTRkMDYwYWNmNjdiNWRhMTc3OGE0YmU2OGJlMjI2ZWRmY2EiKQo=
")
"</pre>

<p>
Snowballs can be uploaded to the snowfort using the
\"<code>snowman upload</code> <i>P</i>\" command.  <i>P</i> is
either a snowball with extension <code>.tgz</code> or
<code>.tar.gz</code>, or a package directory.
For example:
</p>

<pre>" (html-escape "
  % snowman upload mypkg
  Upload success: snowball mypkg-v1_0_0.tgz uploaded successfully!
")
"</pre>

<h3>Package maintenance</h3>

<p>
The Snow package maintenance model identifies packages using a
name and a major version number, e.g. <i>P/vX</i>.  The various
revisions (or instances) of the package are identified with that
name and major version number, and some minor version number and
build number.  Maintenance responsibilities for maintainers only
encompass the revisions of a package with a specific major
version number.  This view is consistent with the fact that the
maintainer is responsible for preserving backward compatibility
within a given major version number throughout the life of that
package.  Given that there is no required relation between the
APIs of packages with the same name and different major version
number, they are considered independent and can be maintained by
different sets of maintainers.
</p>

<p>
The Snow framework realizes this model by allowing anyone to
upload a snowball for package <i>P/vX</i>, and become its
maintainer, if no package with that name and major version number
currently exists on the snowfort.  If <i>P/vX</i> exists then the
current maintainers are the only ones permitted to upload
revisions, and only if at least one of them signs the files in
the snowball.  Maintainers are identified using their public
certificates.  The current maintainers of package <i>P/vX</i> are
indicated in the <code>package*</code> form of the highest
revision of the package (<i>P/vX.Y.Z</i>).  If a revision
adds a new maintainer to the <code>package*</code> form, then
that maintainer must sign the package in addition to one of
the current maintainers.
</p>

<p>
It is possible for a package <i>P/vX</i> to have no maintainer,
either because the <code>package*</code> form does not contain a
maintainer meta-data clause, or because none of the persons
indicated in the maintainer meta-data clause have signed the last
revision.  In this case there are no restrictions on the
uploading of revisions.  Note that anyone could become the
maintainer of such a package by uploading a signed revision
with a <code>package*</code> form containing their name in the
maintainer meta-data clause.
</p>

<h3>Script syntax</h3>

<p>
The generic Snow can be used to write scripts.  A script has the
same syntax as a package, except the first line is a shell command
which executes the program \"<code>snow</code>\".
Here's the source code of a minimal script, which is stored in the
file \"<code>go.scm</code>\":
</p>

<pre>" (html-escape "
  \":\";exec snow -- \"$0\" \"$@\"

  (package* go/v1.0.0
   (require: hostos/v1)
   (require: fixnum/v1))

  (define (double x)
    (snow-fxarithmetic-shift-left x 1))

  (test* (expect* (= 8 (double 4))))

  (write (double (string->number (cadr (snow-command-line)))))
  (newline)
")
"</pre>

<p>
Scripts are normally executed with the host Scheme system which
was configured when the generic Snow implementation was
installed.  This can be changed by setting the
<code>SNOW_HOST</code> environment variable to the name of the
required host Scheme system (i.e. <code>bigloo</code>,
<code>chez</code>, <code>chicken</code>, <code>gambit</code>,
etc).  The special name <code>all</code> will execute the
script with all the installed Scheme systems.  This is useful for
testing the portability of a package.  The environment variable
<code>SNOW_TEST</code> can be set to the name(s) of the package(s)
whose self tests must be executed.  The environment variable
<code>SNOW_DEBUG</code> when set to 1 will cause some debugging
information to be output.  For example:
</p>

<pre>" (html-escape "
  % chmod +x go.scm 
  % ./go.scm 500
  1000
  % SNOW_HOST=mzscheme ./go.scm 500
  1000
  % SNOW_HOST=all ./go.scm 500
  ------------------------------------------------------------ bigloo
  1000
  ------------------------------------------------------------ chicken
  1000
  ------------------------------------------------------------ gambit
  1000
  ------------------------------------------------------------ mzscheme
  1000
  ------------------------------------------------------------ petite
  1000
  ------------------------------------------------------------ scm
  1000
  ------------------------------------------------------------ scsh
  1000
  ------------------------------------------------------------ stklos
  1000
  % SNOW_TEST=\"fixnum go\" SNOW_HOST=mzscheme ./go.scm 500
  1000
  *** SNOW TESTS: passed all 7 tests.
")
"</pre>

<p>
The <code>SNOW_PATH</code> environment variable contains the
colon separated list of directories that will be searched to find
packages.  If <code>SNOW_PATH</code> is not set, the default
is to search in order: the current directory, the user specific
directory, and the site specific directory.  When the package
<i>P</i> is searched, for each directory <i>D</i>
in <code>SNOW_PATH</code> the following files will be
visited to find the package file:
</p>

<ol>

<li> <i>D</i><code>/</code><i>P</i><code>.scm</code>

<li> <i>D</i><code>/</code><i>P</i><code>/snow/</code><i>P</i><code>.scm</code>

<li> <i>D</i><code>/</code><i>P</i><code>/</code><i>vX.Y.Z</i><code>/snow/</code><i>P</i><code>.scm</code>

</ol>

<h3>Compiling packages</h3>

<p>
On systems which allow dynamic loading of compiled code (such as
Chicken, Gambit and STklos), individual packages can be compiled
separately with the command \"<code>snow --compile ...</code>\".
For example, to compile the package <code>foo</code> contained in
the source code file <code>foo.scm</code> using the Gambit system,
the following can be done:
</p>

<pre>" (html-escape "
  % SNOW_HOST=gambit snow --compile foo.scm
")
"</pre>
"))

(define (do-tab-Documentation query)
  (send-page
   (decorated-page
    (plain-tag)
    ""
    "Snowfort"
    "selt3"
    (html-Documentation))))

(define (do-tab-Packages query)
  (do-listcat query "all"))

(define (html-Upload) "
<h1>Snowball submission</h1>

<p>
Please select the snowball you wish to upload and click
on the Upload button.
</p>

<form enctype=\"multipart/form-data\" action=\"snowfort.cgi\" method=\"post\">
  <p>
    <input type=\"hidden\" name=\"operation\" value=\"upload\">
    <label for=\"snowball\">Snowball: </label>
      <input type=\"file\" name=\"snowball\">
    <input type=\"submit\" value=\"Upload\">
  </p>
</form>
")

(define (do-tab-Upload query)
  (send-page
   (decorated-page
    (plain-tag)
    ""
    "Snowfort"
    "selt5"
    (html-Upload))))

(define (html-Links) "
<h1>Links</h1>

<h2>Specialized Snow framework implementations</h2>

<p>
Here is a list of the known specialized Snow framework implementations:
</p>

<ul>

<li>
<b>Bigloo Snow framework</b>: builtin support since version 3.0 (see
<a href=\"http://www-sop.inria.fr/mimosa/fp/Bigloo/\">Bigloo home page</a>).

<li>
<b>Chicken Snow framework</b>: work in progress.

<li>
<b>Gambit Snow framework</b>: work in progress.

<li>
<b>MzScheme Snow framework</b>: work in progress.

<li>
<b>STklos Snow framework</b>: work in progress.

</ul>

<h2>People</h2>

<p>
The following people have participated in the design and implementation of
Snow:
</p>

<ul>

<li>
<a href=\"http://www.barzilay.org/\">Eli Barzilay</a>

<li>
<a href=\"http://theschemeway.blogspot.com/\">Dominique Boucher</a>

<li>
<a href=\"http://www.iro.umontreal.ca/~feeley\">Marc Feeley</a> (project leader)

<li>
<a href=\"http://people.cs.uchicago.edu/~robby/\">Robby Findler</a>

<li>
<a href=\"http://saxo.essi.fr/~gallesio/\">Erick Gallesio</a>

<li>
<a href=\"http://www.iro.umontreal.ca/~germaing/\">Guillaume Germain</a>

<li>
<a href=\"http://jay.teammccarthy.org/Welcome.html\">Jay McCarthy</a>

<li>
Ryan Newton

<li>
<a href=\"http://www-sop.inria.fr/mimosa/Manuel.Serrano/\">Manuel Serrano</a>

<li>
<a href=\"http://synthcode.com/\">Alex Shinn</a>

<li>
<a href=\"http://www.ccs.neu.edu/home/shivers/\">Olin Shivers</a>

<li>
<a href=\"http://www.call-with-current-continuation.org/index.html\">Felix Winkelmann</a>

</ul>
")

(define (do-tab-Links query)
  (send-page
   (decorated-page
    (plain-tag)
    ""
    "Snowfort"
    "selt6"
    (html-Links))))

(define (do-tab query tab)
  (cond ((string=? tab "Home")
         (do-tab-Home query))
        ((string=? tab "Installing")
         (do-tab-Installing query))
        ((string=? tab "Documentation")
         (do-tab-Documentation query))
        ((string=? tab "Packages")
         (do-tab-Packages query))
        ((string=? tab "Upload")
         (do-tab-Upload query))
        ((string=? tab "Links")
         (do-tab-Links query))
        (else
         (respond-with-status (error-tag)
                              "request syntax error"))))

;;;----------------------------------------------------------------------------
              
(define (extract-files files package-name fail cont)
  (let* ((snow-file (string-append "snow/" package-name ".scm"))
         (f1 (assoc snow-file files))
         (snow-file-sig (string-append snow-file (signature-file-extension)))
         (f2 (assoc snow-file-sig files)))

    (define (snow-file-missing-err)
      (string-append
       "snowball structure error (file "
       snow-file
       " is missing)"))

    (define (snow-file-err)
      (string-append
       "snowball structure error (file "
       snow-file
       " syntax error)"))

    (define (snow-file-sig-err)
      (string-append
       "snowball structure error (file "
       snow-file-sig
       " syntax error)"))

    (define (signature-err)
      "snowball structure error (invalid signature)")

    (if (not f1)
        (fail (snow-file-missing-err))
        (let* ((snow-file-content
                (tar-rec-content (cdr f1)))
               (package-form
                (snow-with-exception-catcher
                 (lambda (exc)
                   (snow-file-err))
                 (lambda ()
                   (let ((pi
                          (read-package-info
                           (snow-u8vector->ISO-8859-1-string
                            snow-file-content))))
                     (if (not (list? pi))
                         (snow-raise "error")
                         pi)))))
               (sign-certs
                (if f2
                    (snow-with-exception-catcher
                     (lambda (exc)
                       (snow-file-sig-err))
                     (lambda ()
                       (map (lambda (signature)
                              (list->certificate (list-ref signature 0)))
                            (snow-string->object-list 
                             (snow-u8vector->ISO-8859-1-string
                              (tar-rec-content (cdr f2)))))))
                    '())))
          (cond ((string? package-form)
                 (fail package-form))
                ((string? sign-certs)
                 (fail sign-certs))
                ((not (and (>= (length package-form) 2)
                           (eq? (car package-form) 'package*)
                           (symbol? (cadr package-form))))
                 (fail (snow-file-err)))
                (else
                 (extract-package-name-and-version
                  (symbol->string (cadr package-form))
                  (lambda ()
                    (fail (snow-file-err)))
                  (lambda (name version)
                    (if (not (and (string=? package-name name)
                                  (= 3 (length version))))
                        (fail (snow-file-err))
                        (let ((version-str
                               (version->string version ".")))
                          (cont
                           name
                           version
                           package-form
                           sign-certs
                           (map
                            (lambda (x)
                              (let ((tr (cdr x)))
                                (tar-rec-name-set!
                                 tr
                                 (string-append
                                  name
                                  "/"
                                  version-str
                                  "/"
                                  (car x)))
                                tr))
                            files))))))))))))

(define (extract-tar-recs tar-recs fail cont)
  (let* ((root
          (tar-rec-name (car tar-recs)))
         (package-name
          (get-package-name root))
         (files
          (map (lambda (tr)
                 (cons (let* ((filename
                               (tar-rec-name tr))
                              (name
                               (snow-remove-string-prefix
                                filename
                                root)))
                         (if (or (not name)
                                 (dangerous-unix-filename? name))
                             #f
                             name))
                       tr))
               tar-recs)))
    (cond ((or (not package-name)
               (assq #f files))
           (fail "snowball structure error (invalid filenames)"))
          ((not (valid-package-name? package-name))
           (fail "snowball structure error (invalid package name)"))
          (else
           (extract-files files package-name fail cont)))))

(define (extract-snowball snowball fail cont)
  (let ((tar-recs
         (snow-with-exception-catcher
          (lambda (exc)
            (cond ((zlib-condition? exc)
                   (string-append
                    "snowball cannot be uncompressed ("
                    (zlib-condition-msg exc)
                    ")"))
                  ((tar-condition? exc)
                   (string-append
                    "snowball cannot be unpacked ("
                    (tar-condition-msg exc)
                    ")"))
                   (else
                    "snowball cannot be uncompressed and unpacked")))
          (lambda ()
            (let* ((genport-snowball-tar-gz
                    (genport-open-input-u8vector snowball))
                   (genport-snowball-tar
                    (gunzip-genport genport-snowball-tar-gz)))
              (tar-unpack-genport genport-snowball-tar))))))
    (if (string? tar-recs)
        (fail tar-recs)
        (extract-tar-recs tar-recs fail cont))))

(define (save-snowball name
                       version
                       package-form
                       tar-rec-list
                       publ-certs
                       fail
                       cont)

  (define (create-dir dir)
    (snow-with-exception-catcher
     (lambda (e)
       #f)
     (lambda ()
       (snow-create-directory dir)
       #t)))

  (let* ((tgz-name
          (make-snowball-filename name version))
         (pkg-dir
          (snow-make-filename (pack-dir) name))
         (vers-dir
          (snow-make-filename pkg-dir (version->string version ".")))
         (disable-dir
          (snow-make-filename vers-dir ".disable"))
         (snow-dir
          (snow-make-filename vers-dir "snow"))
         (tgz-file
          (snow-make-filename snow-dir tgz-name))
         (pkg-file
          (snow-make-filename snow-dir (string-append name ".scm"))))

    (create-dir pkg-dir)
    (if (not (create-dir vers-dir))
        (fail (string-append "snowball " tgz-name " already exists"))
        (begin
          (create-dir disable-dir)
          (create-dir snow-dir)
          (with-output-to-file
              pkg-file
            (lambda ()
              (write package-form)
              (newline)
              (write
               (let ((ct (current-time-seconds)))
                 (bignum->string ct)))
              (newline)
              (write (map certificate->list publ-certs))
              (newline)))
          (let ((genport-out (genport-open-output-file tgz-file)))
            (let* ((snowball
                    (gzip-u8vector
                     (tar-pack-u8vector tar-rec-list)))
                   (len
                    (snow-u8vector-length snowball))
                   (n
                    (genport-write-subu8vector snowball 0 len genport-out)))
              (genport-close-output-port genport-out)
              (if (= len n)
                  (begin
                    (snow-delete-directory disable-dir)
                    (cont (string-append
                           "snowball "
                           tgz-name
                           " uploaded successfully!")))
                  (begin
                    (snow-delete-file tgz-file)
                    (snow-delete-file pkg-file)
                    (snow-delete-directory snow-dir)
                    (snow-delete-directory disable-dir)
                    (snow-delete-directory vers-dir)
                    (fail (string-append
                           "snowball "
                           tgz-name
                           " could not be saved to repository"))))))))))

(define (upload-snowball snowball fail cont)
  (extract-snowball
   snowball
   fail
   (lambda (name version package-form sign-certs tar-rec-list)
     (let* ((pkg
             (get-package (pack-dir) name (list (car version))))
            (versions
             (pkg-versions pkg))
            (highest-vpkg
             (and (pair? versions)
                  (car versions)))
            (previous-publ-certs
             (if highest-vpkg
                 (vpkg-publ-certs highest-vpkg)
                 '())))
       (if (and (pair? previous-publ-certs)
                (let ((x (verify-tar-rec-list
                          tar-rec-list
                          (map (lambda (c) (cons c #f)) previous-publ-certs)
                          #f)))
                  (not (null? (vector-ref x 1))))) ;; some unverified files?
           (fail "package is not signed by a current maintainer")
           (let* ((m
                   (extract-attrib 'maintainer: package-form))
                  (maintainer
                   (if (and m
                            (>= (length m) 1)
                            (not (memq #f (map string? m))))
                       m
                       '()))
                  (publ-certs
                   (snow-filter
                    (lambda (c) (member (certificate-owner c) maintainer))
                    (certs-union previous-publ-certs sign-certs))))
             (save-snowball name
                            version
                            package-form
                            tar-rec-list
                            publ-certs
                            fail
                            cont)))))))

(define (do-operation-upload query)
  (let ((x (assoc 'snowball query)))
    (if (not x)
        (respond-with-status (upload-failure-tag)
                             "request syntax error")
        (let ((snowball (cdr x)))
          (if (not (snow-u8vector? snowball))
              (respond-with-status (upload-failure-tag)
                                   "snowball must be in .tar.gz format")
              (upload-snowball
               snowball
               (lambda (result)
                 (respond-with-status (upload-failure-tag)
                                      result))
               (lambda (result)
                 (respond-with-status (upload-success-tag)
                                      result))))))))

(define (do-operation-download query)

  (define (err)
    (respond-with-status (download-failure-tag)
                         "request syntax error"))

  (let* ((p
          (assoc 'pkg query))
         (name-with-version
          (and p
               (string? (cdr p))
               (cdr p))))
    (if (not name-with-version)
        (err)
        (extract-package-name-and-version
         name-with-version
         err
         (lambda (name version)
           (if (not (= 3 (length version)))
               (err)
               (let* ((tgz-name
                       (make-snowball-filename name version))
                      (pkg-dir
                       (snow-make-filename (pack-dir) name))
                      (vers-str
                       (version->string version "."))
                      (vers-dir
                       (snow-make-filename pkg-dir vers-str))
                      (snow-dir
                       (snow-make-filename vers-dir "snow"))
                      (tgz-file
                       (snow-make-filename snow-dir tgz-name)))
                 (if (not (snow-file-exists? tgz-file))
                     (respond-with-status (download-failure-tag)
                                          (string-append
                                           "snowball "
                                           tgz-name
                                           " does not exist"))
                     (let ((content (genport-read-file tgz-file)))
                       (cgi-send-response
                        (list '(content-type (application . x-gzip))
                              (list 'content-disposition
                                    'attachment
                                    (cons 'filename tgz-name)))
                        content))))))))))

(define (do-operation-list query)
  (cgi-send-response
   (list '(content-type (application . binary))
         (list 'content-disposition
               'attachment
               (cons 'filename "snow_package_list")))
   (snow-ISO-8859-1-string->u8vector
    (snow-object-list->string
     (let ((pkgs (get-packages (pack-dir) '())))
       (map (lambda (pkg)
              (cons (pkg-name pkg)
                    (map (lambda (vpkg)
                           (vpkg-package-form vpkg))
                         (pkg-versions pkg))))
            pkgs))))))

(define (do-operation query op)
  (cond ((string=? op "upload")
         (do-operation-upload query))
        ((string=? op "download")
         (do-operation-download query))
        ((string=? op "list")
         (do-operation-list query))
        (else
         (respond-with-status (error-tag)
                              "request syntax error"))))

;;;----------------------------------------------------------------------------

(define (handle-query)
  (let ((query (cgi-get-query)))
    (cond ((or (not query)
               (null? query))
           (do-Main query))
          ((assoc 'tab query)
           =>
           (lambda (x)
             (do-tab query (cdr x))))
          ((assoc 'listcat query)
           =>
           (lambda (x)
             (do-listcat query (cdr x))))
          ((assoc 'viewpkg query)
           =>
           (lambda (x)
             (do-viewpkg query (cdr x))))
          ((assoc 'operation query)
           =>
           (lambda (x)
             (do-operation query (cdr x))))
          (else
           (respond-with-status (error-tag)
                                "request syntax error")))))

(define (snowfort-main)
  (handle-query))

;;(define (snowfort-main)
;;  ((call-with-current-continuation
;;    (lambda (unwind)
;;      (with-exception-handler
;;       (lambda (e)
;;         (##continuation-capture
;;          (lambda (cont)
;;            (unwind
;;             (lambda ()
;;               (cgi-send-response
;;                '((content-type (text . html)))
;;                (snow-ISO-8859-1-string->u8vector
;;                 (with-output-to-string
;;                   ""
;;                   (lambda ()
;;                     (display "<pre>\n")
;;                     (##display-exception-in-context e cont (current-output-port))
;;                     (display "</pre>\n"))))))))))
;;       (lambda ()
;;         (let ((result (handle-query)))
;;           (lambda () result))))))))

;;;============================================================================
