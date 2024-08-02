;;;============================================================================

;;; File: "snow.scm", Time-stamp: <2007-09-03 13:50:02 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Implementation of the Scheme Now! framework.

;;;============================================================================

;; A Scheme Now! package is a file of Scheme source code obeying the
;; following syntax:
;;
;; <package-file> --> <package-form> <package-file-body>
;;
;; <package-form> --> ( package* <package-name-with-full-version>
;;                      <package-form-body> )
;;
;; <package-form-body> --> <package-attributes> <implementation-requirements>
;;
;; <package-attributes> --> [ <provide-form> ] <package-meta-data>*
;;
;; <package-meta-data> --> ( maintainer: <string>+ )
;;                      |  ( author: <string>+ )
;;                      |  ( homepage: <string>+ )
;;                      |  ( description: <string>+ )
;;                      |  ( keywords: <symbol>+ )
;;                      |  ( license: <symbol>+ )
;;
;; <provide-form> --> ( provide: <provide-form-body> )
;;
;; <provide-form-body> --> <interface-definitions>
;;
;; <interface-definitions> --> <interface-def>*
;;
;; <interface-def> --> <procedure-prototype>
;;                  |  <macro-def>
;;                  |  <record-def>
;;
;; <procedure-prototype> --> ( define ( <var-name> <R5RS def formals> ) )
;;                        |  ( define* ( <var-name> <prototype formals> ) )
;;
;; <procedure-def> --> ( define ( <var-name> <R5RS def formals> )
;;                        <body> )
;;                  |  ( define* ( <var-name> <SRFI 89 extended def formals> )
;;                        <body> )
;;
;; <macro-def> --> ( define-syntax <macro-name>
;;                    <expression> )
;;              |  ( define-macro ( <macro-name> <R5RS def formals> )
;;                    <body> )
;;              |  ( define-macro* ( <macro-name> <SRFI 89 extended def formals> )
;;                    <body> )
;;
;; <record-def> --> ( define-record* <record-name>
;;                     <record-body> )
;;
;; <record-body> --> <record-clause>*
;;
;; <record-clause> --> <record-field>
;;
;; <record-field> --> <field-name>
;;
;; <implementation-requirements> --> <requirements>
;;
;; <requirements> --> <require-form>*
;;
;; <require-form> --> ( require: <package-name-with-required-version>
;;                       <require-form-body> )
;;
;; <require-form-body> --> <empty>
;;
;; <package-file-body> --> <package command or def>*
;;
;; <package command or def> --> <expression>
;;                           |  <implementation-def>
;;                           |  <include>
;;                           |  <test>
;;
;; <implementation-def> --> <var-def>
;;                       |  <macro-def>
;;                       |  <record-def>
;;
;; <var-def> --> ( define <var-name> <expression> )
;;            |  <procedure-def>
;;
;; <include> --> ( include* <unix-relative-path-string> )
;;
;; <test> --> ( test* <test-body> )
;;
;; <test-body> --> <package-file-body-containing-expect-expressions>
;;
;; <package-file-body-containing-expect-expressions> --> <package-file-body>
;;
;; <expect-expression> --> ( expect* <expect-test> )
;;
;; <expect-test> --> <expression>
;;
;; <field-name> --> <name>
;; <var-name> --> <name>
;; <package-name> --> <name>
;; <package-name-with-full-version> --> <package-name>/<full-version>
;; <package-name-with-required-version> --> <package-name>/<required-version>
;; <macro-name> --> <name>
;; <record-name> --> <name>
;; <name> --> <identifier>
;;
;; <full-version> --> v<N>.<N>.<N>
;; <required-version> --> v<N>
;;                     |  v<N>.<N>
;;                     |  v<N>.<N>.<N>
;;
;; <prototype formals> --> <SRFI 89 extended def formals>
;; <SRFI 89 extended def formals> --> <extended def formals> from SRFI 89
;; <R5RS def formals> --> <def formals> from R5RS
;;
;; Note that a <package-form-body> and a <provide-form-body> can
;; contain arbitrarily nested cond-expand forms which test only the
;; host Scheme implementation (i.e. bigloo, chicken, gambit, etc).

;;;============================================================================

;;; System dependencies.

;;;----------------------------------------------------------------------------

;; Macro evaluation environment.
;;
;; We will define multiple macros which have to share some code.  In
;; order to do this, we have to evaluate the definitions for the shared
;; code in the macro evaluation environment.  This is achieved with
;; eval.

(cond-expand

 (mit

  (define-macro (_snow:eval-in-macro-environment . exprs)
    (eval (cons 'begin exprs) user-initial-environment)))

 (scm

  (define-macro (_snow:eval-in-macro-environment . exprs)
    (eval (cons 'begin exprs))))

 (stklos

  (define-macro (_snow:eval-in-macro-environment . exprs)
    (eval (cons 'begin exprs)
          (find-module 'STklos))))

 (else

  (define-macro (_snow:eval-in-macro-environment . exprs)
    (if (pair? exprs)
        (eval (if (null? (cdr exprs)) (car exprs) (cons 'begin exprs))
              (interaction-environment))
        #f))))

(define-macro (_snow:eval-in-macro-environment-no-result . exprs)
  `(_snow:eval-in-macro-environment
    ,@exprs
    (cond-expand
     ((or guile
          kawa)
      #f)
     (else
      '(begin)))))

(define-macro (define^ pattern . body)
  `(_snow:eval-in-macro-environment-no-result
    (define ,pattern ,@body)))

(cond-expand

 (scm

  (_snow:eval-in-macro-environment-no-result (require 'defmacroexpand))

  (define^ (_snow:macro-expand expr)
    (defmacro:expand* expr)))

 (else

  (define^ (_snow:macro-expand expr)
    expr)))

;; Determine host Scheme system.

(cond-expand
 (bigloo   (define^ _snow:host 'bigloo))
 (chez     (define^ _snow:host 'chez))
 (chicken  (define^ _snow:host 'chicken))
 (gambit   (define^ _snow:host 'gambit))
 (gauche   (define^ _snow:host 'gauche))
 (guile    (define^ _snow:host 'guile))
 (kawa     (define^ _snow:host 'kawa))
 (larceny  (define^ _snow:host 'larceny))
 (mit      (define^ _snow:host 'mit))
 (mzscheme (define^ _snow:host 'mzscheme))
 (rscheme  (define^ _snow:host 'rscheme))
 (scheme48 (define^ _snow:host 'scheme48))
 (scm      (define^ _snow:host 'scm))
 (scsh     (define^ _snow:host 'scsh))
 (sisc     (define^ _snow:host 'sisc))
 (stalin   (define^ _snow:host 'stalin))
 (stklos   (define^ _snow:host 'stklos)))

;; Error handling.

(cond-expand

 ((or bigloo
      chez
      chicken
      gambit
      gauche
      guile
      kawa
      larceny
      mzscheme
      rscheme
      scheme48
      scm
      scsh
      sisc
      stklos)

  (define^ (_snow:exit status)
    (exit status)))

 (mit

  (define^ (_snow:exit status)
    (%exit status))))

(define^ (_snow:error msg . args)
  (display "*** SNOW ERROR -- ")
  (display msg)
  (for-each (lambda (x) (display " ") (write x)) args)
  (newline)
  (_snow:exit 1))

;; Access to environment variables.

(cond-expand

 ((or bigloo
      chez
      chicken
      guile
      larceny
      mzscheme
      rscheme
      scm
      scsh
      stklos)

  (define^ (_snow:getenv var)
    (getenv var)))

 (gambit

  (define^ (_snow:getenv var)
    (getenv var #f)))

 (gauche

  (define^ (_snow:getenv var)
    (sys-getenv var)))

 (kawa

  (define^ (_snow:getenv var)
    (let ((val (invoke-static <java.lang.System> 'getenv var)))
      (if (string? val)
          val
          #f))))

 (mit

  (define^ (_snow:getenv var)
    (get-environment-variable var)))

 (scheme48

  (define^ (_snow:getenv var)
    (lookup-environment-variable var)))

 (sisc

  (import os)

  (define^ _snow:environment
    (let ()

      (define (read-char port)
        (let ((b (read-byte port)))
          (if (eof-object? b)
              b
              (integer->char b))))

      (let* ((process
              (spawn-process "printenv"))
             (port
              (get-process-stdout process))
             (alist
              (let loop1 ((alist '()))
                (let ((c (read-char port)))
                  (if (eof-object? c)
                      alist
                      (let loop2 ((name (list c)))
                        (let ((c (read-char port)))
                          (cond ((eof-object? c)
                                 alist)
                                ((char=? #\= c)
                                 (let loop3 ((val '()))
                                   (let ((c (read-char port)))
                                     (cond ((eof-object? c)
                                            alist)
                                           ((char=? #\newline c)
                                            (loop1
                                             (cons (cons (list->string
                                                          (reverse name))
                                                         (list->string
                                                          (reverse val)))
                                                   alist)))
                                           (else
                                            (loop3 (cons c val)))))))
                                (else
                                 (loop2 (cons c name)))))))))))
        (close-input-port port)
        (wait-for-process process)
        (reverse alist))))

  (define^ (_snow:getenv var)
    (let ((x (assoc var _snow:environment)))
      (if x
          (cdr x)
          #f)))))

;; File system access.

(cond-expand

 (bigloo

  (define^ (_snow:directory-files dir)
    (directory->list dir))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (directory? filename))

  (define^ (_snow:delete-file filename)
    (delete-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (rename-file orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (make-directory dir))

  (define^ (_snow:delete-directory dir)
    (delete-directory dir)))

 (chez

  (define^ (_snow:directory-files dir)

    (define (read-line port)
      (let ((c (read-char port)))
        (if (char? c)
            (let loop ((lst (list c)))
              (let ((c (read-char port)))
                (if (and (char? c) (not (char=? c #\newline)))
                    (loop (cons c lst))
                    (list->string (reverse lst)))))
            c)))

    (define (read-lines port)
      (let loop ((lst '()))
        (let ((s (read-line port)))
          (if (string? s)
              (loop (cons s lst))
              (reverse lst)))))

    (let* ((p (process (string-append "ls -a \"" dir "\"")))
           (i (car p))
           (o (cadr p))
           (files (read-lines i)))
      (close-input-port i)
      (close-output-port o)
      (remove "."
              (remove ".."
                      files))))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (file-exists? (string-append filename "/.")))

  (define^ (_snow:delete-file filename)
    (delete-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (system (string-append "mv \"" orig-filename "\" \"" new-filename "\"")))

  (define^ (_snow:create-directory dir)
    (system (string-append "mkdir \"" dir "\"")))

  (define^ (_snow:delete-directory dir)
    (system (string-append "rmdir \"" dir "\""))))

 (chicken

  (_snow:eval-in-macro-environment-no-result (use posix) #f)

  (define^ (_snow:directory-files dir)
    (directory dir #t))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (directory? filename))

  (define^ (_snow:delete-file filename)
    (delete-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (rename-file orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (create-directory dir))

  (define^ (_snow:delete-directory dir)
    (delete-directory dir)))

 (gambit

  (define^ (_snow:directory-files dir)
    (directory-files (list path: dir ignore-hidden: 'dot-and-dot-dot)))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (eq? (file-type filename) 'directory))

  (define^ (_snow:delete-file filename)
    (delete-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (rename-file orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (create-directory dir))

  (define^ (_snow:delete-directory dir)
    (delete-directory dir)))

 (gauche

  (_snow:eval-in-macro-environment-no-result (use srfi-1))

  (define^ (_snow:directory-files dir)
    (delete "."
            (delete ".."
                    (sys-readdir dir))))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (file-is-directory? filename))

  (define^ (_snow:delete-file filename)
    (sys-unlink filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (sys-rename orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (sys-mkdir dir #o777))

  (define^ (_snow:delete-directory dir)
    (sys-rmdir dir)))

 (guile

  (define^ (_snow:directory-files dir)
    (let ((d (opendir dir)))
      (let loop ((lst '()))
        (let ((entry (readdir d)))
          (if (eof-object? entry)
              (begin
                (closedir d)
                (delete "."
                        (delete ".."
                                lst)))
              (loop (cons entry lst)))))))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (file-exists? (string-append filename "/.")))

  (define^ (_snow:delete-file filename)
    (delete-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (rename-file orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (mkdir dir))

  (define^ (_snow:delete-directory dir)
    (rmdir dir)))

 (kawa

  (define^ (_snow:directory-files dir)
    (directory-files dir))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (file-directory? filename))

  (define^ (_snow:delete-file filename)
    (delete-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (rename-file orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (if (file-exists? dir)
        (_snow:error "could not create directory")
        (create-directory dir)))

  (define^ (_snow:delete-directory dir)
    (if (not (file-exists? dir))
        (_snow:error "could not delete directory")
        (delete-file dir))))

 (larceny

  (define^ (_snow:directory-files dir)
    (filter (lambda (ent)
              (not (or (equal? ent ".")
                       (equal? ent ".."))))
            (list-directory dir)))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (file-exists? (string-append filename "/.")))

  (define^ (_snow:delete-file filename)
    (delete-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (rename-file orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (system (string-append "mkdir \"" dir "\"")))

  (define^ (_snow:delete-directory dir)
    (system (string-append "rmdir \"" dir "\""))))

 (mit

  (define^ (_snow:directory-files dir)
    (delete "."
            (delete ".."
                    (map file-namestring
                         (directory-read (string-append dir "/"))))))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (file-directory? filename))

  (define^ (_snow:delete-file filename)
    (delete-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (rename-file orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (make-directory dir))

  (define^ (_snow:delete-directory dir)
    (delete-directory dir)))

 (mzscheme

  (define^ (_snow:directory-files dir)
    (map path->string (directory-list dir)))

  (define^ (_snow:file-exists? filename)
    (or (file-exists? filename)
        (directory-exists? filename)))

  (define^ (_snow:file-directory? filename)
    (and (not (file-exists? filename))
         (directory-exists? filename)))

  (define^ (_snow:delete-file filename)
    (delete-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (rename-file-or-directory orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (make-directory dir))

  (define^ (_snow:delete-directory dir)
    (delete-directory dir)))

 (rscheme

  (define^ (_snow:directory-files dir)
    (_snow:error "snow-directory-files is not implemented"))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (file-exists? (string-append filename "/.")))

  (define^ (_snow:delete-file filename)
    (delete-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (rename-file orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (system (string-append "mkdir \"" dir "\"")))

  (define^ (_snow:delete-directory dir)
    (system (string-append "rmdir \"" dir "\""))))

 (scheme48

  (define^ (_snow:directory-files dir)
    (list-directory dir))

  (define^ (_snow:file-exists? filename)
    (accessible? filename (access-mode read)))

  (define^ (_snow:file-directory? filename)
    (_snow:file-exists? (string-append filename "/.")))

  (define^ (_snow:delete-file filename)
    (unlink filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (system (string-append "mv \"" orig-filename "\" \"" new-filename "\"")))

  (define^ (_snow:create-directory dir)
    (system (string-append "mkdir \"" dir "\"")))

  (define^ (_snow:delete-directory dir)
    (system (string-append "rmdir \"" dir "\""))))

 (scm

  (_snow:eval-in-macro-environment-no-result (require 'directory))

  (define^ (_snow:directory-files dir)
    (let ((rev-filenames '()))
      (directory-for-each
       (lambda (filename)
         (set! rev-filenames (cons filename rev-filenames)))
       dir)
      (reverse rev-filenames)))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (file-exists? (string-append filename "/.")))

  (define^ (_snow:delete-file filename)
    (system (string-append "rm -f \"" filename "\"")))

  (define^ (_snow:rename-file orig-filename new-filename)
    (system (string-append "mv \"" orig-filename "\" \"" new-filename "\"")))

  (define^ (_snow:create-directory dir)
    (system (string-append "mkdir \"" dir "\"")))

  (define^ (_snow:delete-directory dir)
    (system (string-append "rmdir \"" dir "\""))))

 (scsh

  (define^ (_snow:directory-files dir)
    (directory-files dir #t))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (file-directory? filename))

  (define^ (_snow:delete-file filename)
    (delete-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (rename-file orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (create-directory dir))

  (define^ (_snow:delete-directory dir)
    (delete-directory dir)))

 (sisc

  (import file-manipulation) 

  (define^ (_snow:directory-files dir)
    (directory-list dir))

  (define^ (_snow:file-exists? filename)
    (file-exists? filename))

  (define^ (_snow:file-directory? filename)
    (file-is-directory? filename))

  (define^ (_snow:delete-file filename)
    (file-delete! filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (file-rename! orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (make-directory! dir))

  (define^ (_snow:delete-directory dir)
    (file-delete! dir)))

 (stklos

  (define^ (_snow:directory-files dir)
    (delete "."
            (delete ".."
                    (exec-list (string-append "ls -a \"" dir "\"")))))

  (define^ (_snow:file-exists? filename)
    (or (file-exists? filename)
        (file-is-directory? filename)))

  (define^ (_snow:file-directory? filename)
    (file-is-directory? filename))

  (define^ (_snow:delete-file filename)
    (remove-file filename))

  (define^ (_snow:rename-file orig-filename new-filename)
    (rename-file orig-filename new-filename))

  (define^ (_snow:create-directory dir)
    (system (string-append "mkdir \"" dir "\"")))

  (define^ (_snow:delete-directory dir)
    (system (string-append "rmdir \"" dir "\"")))))

(define^ _snow:extension-separator #\.)
(define^ _snow:directory-separator #\/)

(define^ (_snow:filename-extension-index filename)
  (let ((end (string-length filename)))
    (let loop ((i (- end 1)))
      (if (< i 0)
          end
          (let ((c (string-ref filename i)))
            (cond ((char=? c _snow:extension-separator)
                   i)
                  ((char=? c _snow:directory-separator)
                   end)
                  (else
                   (loop (- i 1)))))))))

(define^ (_snow:filename-extension filename)
  (substring filename
             (_snow:filename-extension-index filename)
             (string-length filename)))

(define^ (_snow:filename-strip-extension filename)
  (substring filename
             0
             (_snow:filename-extension-index filename)))

(define^ (_snow:string-tail-index str i separator)
  (let loop ((i (- i 1)))
    (if (< i 0)
        0
        (let ((c (string-ref str i)))
          (cond ((char=? c separator)
                 (+ i 1))
                (else
                 (loop (- i 1))))))))

(define^ (_snow:filename-directory-index filename)
  (_snow:string-tail-index
   filename
   (string-length filename)
   _snow:directory-separator))

(define^ (_snow:filename-directory filename)
  (substring filename
             0
             (_snow:filename-directory-index filename)))

(define^ (_snow:filename-strip-directory filename)
  (substring filename
             (_snow:filename-directory-index filename)
             (string-length filename)))

(define^ (_snow:make-filename part1 . parts)
  (let loop ((filename part1) (lst parts))
    (if (pair? lst)
        (loop (string-append filename
                             (string _snow:directory-separator)
                             (car lst))
              (cdr lst))
        filename)))

;; Pretty printing.

(cond-expand

 (bigloo

  (define^ (_snow:pretty-print obj)
    (pp obj)))

 ((or chez
      chicken
      gambit
      mzscheme
      rscheme
      scm
      stklos)

  (define^ (_snow:pretty-print obj)
    (pretty-print obj)))

 ((or mit
      sisc)

  (define^ (_snow:pretty-print obj)
    (pretty-print obj)
    (newline)))

 ((or scsh
      scheme48)

  (define^ (_snow:pretty-print obj)
    (pretty-print obj (current-output-port) 0)
    (newline)))

 (else

  (define^ (_snow:pretty-print obj)
    (write obj)
    (newline)
    (newline))))

;; String port I/O.

(cond-expand

 ((or chicken
      gauche
      guile
      mit
      rscheme
      scm
      stklos)

  (define^ (_snow:call-with-input-string str proc)
    (call-with-input-string str proc))

  (define^ (_snow:call-with-output-string proc)
    (call-with-output-string proc)))

 (gambit

  (define^ (_snow:call-with-input-string str proc)
    (call-with-input-string str proc))

  (define^ (_snow:call-with-output-string proc)
    (call-with-output-string "" proc)))

 (else

  (cond-expand

   ((or scheme48
        scsh)

    (define^ open-input-string make-string-input-port)
    (define^ open-output-string make-string-output-port)
    (define^ get-output-string string-output-port-output))

   (else))

  (define^ (_snow:call-with-input-string str proc)
    (let ((port (open-input-string str)))
      (call-with-values
          (lambda () (proc port))
        (lambda vals
          (close-input-port port)
          (apply values vals)))))

  (define^ (_snow:call-with-output-string proc)
    (let ((port (open-output-string)))
      (proc port)
      (let ((str (get-output-string port)))
        ;; work around a Scsh bug:
        ;; (close-output-port port)
        str)))))

;; Keywords.

(cond-expand

 ((or chez
      larceny
      mit
      rscheme
      scheme48
      scm
      scsh
      sisc)

  (define^ (keyword? obj)
    (and (symbol? obj)
         (let* ((s (symbol->string obj))
                (n (string-length s)))
           (and (>= n 1)
                (char=? (string-ref s (- n 1)) #\:))))) ;; add a colon

  (define^ (keyword->string k)
    (let* ((s (symbol->string k))
           (n (string-length s)))
      (substring s 0 (- n 1)))) ;; remove the colon

  (define^ (string->keyword s)
    (string->symbol (string-append s ":"))))

 (gauche

  (define^ (string->keyword str)
    (make-keyword str)))

 (guile

  (define^ (string->keyword str)
    (symbol->keyword (string->symbol str)))

  (define^ (keyword->string key)
    (symbol->string (keyword->symbol key))))

 (else

  (begin)))

(cond-expand

 ((or chez
      gauche
      guile
      mit
      mzscheme
      rscheme
      scheme48
      scm
      scsh
      sisc)

  (define^ (_snow:source-code-keyword? obj)
    (and (symbol? obj)
         (let* ((s (symbol->string obj))
                (n (string-length s)))
           (and (>= n 1)
                (char=? (string-ref s (- n 1)) #\:))))) ;; add a colon

  (define^ (_snow:source-code-keyword->keyword k)
    (let* ((s (symbol->string k))
           (n (string-length s)))
      (string->keyword (substring s 0 (- n 1)))))

  (define^ (_snow:keyword->source-code-keyword k)
    (string->symbol (string-append (keyword->string k) ":"))))

 (else

  (define^ (_snow:source-code-keyword? obj)
    (keyword? obj))

  (define^ (_snow:source-code-keyword->keyword k)
    k)

  (define^ (_snow:keyword->source-code-keyword k)
    k)))

;; Define which hosts will load the source code file "X.scm" when the
;; call (load "X") is evaluated and the filename "X" has no
;; extension.

(define^ _snow:loads-source-when-no-ext
  '(chicken
    gambit
    gauche
    larceny
    mit
    stklos))

;;;============================================================================

;;; System independent part.

;;;----------------------------------------------------------------------------

;; Utilities.

(define^ (_snow:range start end)
  (if (< start end)
      (cons start (_snow:range (+ start 1) end))
      '()))

(define^ (_snow:split-string str separator)
  (let loop ((end (string-length str))
             (lst '()))
    (if (< end 0)
        lst
        (let ((i (_snow:string-tail-index str end separator)))
          (loop (- i 1)
                (if (= i end)
                    lst
                    (cons (substring str i end)
                          lst)))))))

(define^ (_snow:split-path path default)
  (if (not path)
      default
      (_snow:split-string path #\:)))

(define^ (_snow:apply-append lists)
  (if (pair? lists)
      (append (car lists)
              (_snow:apply-append (cdr lists)))
      '()))

(define^ (_snow:apply-string-append strings)
  (if (pair? strings)
      (string-append (car strings)
                     (_snow:apply-string-append (cdr strings)))
      ""))

(define^ (_snow:sym . args)
  (string->symbol
   (_snow:apply-string-append
    (map (lambda (s) (if (string? s) s (symbol->string s)))
         args))))

(define^ (_snow:make-begin exprs)
  (if (pair? exprs)
      (if (null? (cdr exprs))
          (car exprs)
          (cons 'begin exprs))
      (case _snow:host;;;;;;;;;;
        ((guile) ;; Workaround Guile bug, i.e. (begin) is illegal
         #f)
        (else
         '(begin)))))

(define^ (_snow:sort-list lst <?)

  (define (mergesort lst)

    (define (merge lst1 lst2)
      (cond ((null? lst1) lst2)
            ((null? lst2) lst1)
            (else
             (let ((e1 (car lst1)) (e2 (car lst2)))
               (if (<? e1 e2)
                   (cons e1 (merge (cdr lst1) lst2))
                   (cons e2 (merge lst1 (cdr lst2))))))))

    (define (split lst)
      (if (or (null? lst) (null? (cdr lst)))
          lst
          (cons (car lst) (split (cddr lst)))))

    (if (or (null? lst) (null? (cdr lst)))
        lst
        (let* ((lst1 (mergesort (split lst)))
               (lst2 (mergesort (split (cdr lst)))))
          (merge lst1 lst2))))

  (mergesort lst))

;;;----------------------------------------------------------------------------

;; Version manipulation.

(define^ (_snow:make-verspack name version) (cons name version))
(define^ (_snow:verspack-name x)    (car x))
(define^ (_snow:verspack-version x) (cdr x))

(define^ (_snow:verspack-eqv? vp1 vp2)
  (and (eq? (_snow:verspack-name vp1)
            (_snow:verspack-name vp2))
       (= (car (_snow:verspack-version vp1))
          (car (_snow:verspack-version vp2)))))

(define^ (_snow:verspack-member vp lst)
  (let loop ((lst lst))
    (if (pair? lst)
        (if (_snow:verspack-eqv? vp (car lst))
            lst
            (loop (cdr lst)))
        #f)))

(define^ (_snow:verspack->string vp)
  (string-append
   (symbol->string (_snow:verspack-name vp))
   "/"
   (_snow:version->string (_snow:verspack-version vp))))

(define^ (_snow:package-name-with-major-version package-info)
  (let ((verspack (_snow:package-info-verspack package-info)))
    (string->symbol
     (_snow:verspack->string
      (_snow:make-verspack (_snow:verspack-name verspack)
                           (list (car (_snow:verspack-version verspack))))))))
                          
(define^ (_snow:version->string version)

  (define (version->str version)
    (let loop ((lst version)
               (sep "")
               (result ""))
      (if (pair? lst)
          (loop (cdr lst)
                "."
                (string-append
                 result
                 sep
                 (number->string (car lst))))
          result)))

  (string-append "v" (version->str version)))

(define^ (_snow:string->version str)

  (define (str->version str)
    (let loop ((end (string-length str))
               (version-list '()))
      (if (< end 0)
          version-list
          (let* ((i
                  (_snow:string-tail-index str end #\.))
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

(define^ (_snow:compare-versions version1 version2)
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
         (_snow:compare-versions (cdr version1) (cdr version2)))))

(define^ (_snow:sort-versions versions)
  (_snow:sort-list
   versions
   (lambda (v1 v2)
     (eq? (_snow:compare-versions v1 v2) '>))))

(define^ (_snow:least-upper-bound version1 version2)
  (cond ((null? version1)
         version2)
        ((null? version2)
         version1)
        ((or (< (car version1) (car version2))
             (> (car version1) (car version2)))
         #f) ; incompatible versions
        (else
         (let loop ((v1 (cdr version1))
                    (v2 (cdr version2)))
           (cond ((null? v1)
                  version2)
                 ((null? v2)
                  version1)
                 ((< (car v1) (car v2))
                  version2)
                 ((> (car v1) (car v2))
                  version1)
                 (else
                  (loop (cdr v1)
                        (cdr v2))))))))

(define^ (_snow:compatible-version-with version req-version)

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

(define^ (_snow:combine-requirements verspacks)

  (define (comb lst)
    (if (pair? lst)
        (let loop ((pr1 (car lst)) (lst1 (cdr lst)) (lst2 '()))
          (if (pair? lst1)
              (let ((pr2 (car lst1)))
                (if (eq? (_snow:verspack-name pr1)
                         (_snow:verspack-name pr2))
                    (let ((lub (_snow:least-upper-bound
                                (_snow:verspack-version pr1)
                                (_snow:verspack-version pr2))))
                      (if lub
                          (loop (_snow:make-verspack
                                 (_snow:verspack-name pr1)
                                 lub)
                                (cdr lst1)
                                lst2)
                          (loop pr1
                                (cdr lst1)
                                (cons pr2 lst2))))
                    (loop pr1
                          (cdr lst1)
                          (cons pr2 lst2))))
              (cons pr1 (comb (reverse lst2)))))
        '()))

  (comb verspacks))

;;;----------------------------------------------------------------------------

;; Environment configuration (set to default values).

(define^ _snow:debug 0)
(define^ _snow:tests-enabled? #f)
(define^ _snow:test-packages '())
(define^ _snow:version "v1.1.2")
(define^ _snow:user-root #f)
(define^ _snow:site-root #f)
(define^ _snow:user-dir #f)
(define^ _snow:site-dir #f)
(define^ _snow:path '("."))
(define^ _snow:package-extension ".scm")
(define^ _snow:snowlib (_snow:make-verspack 'snowlib '(1 0 0)))
(define^ _snow:snow-builtin-packages (list _snow:snowlib))

(define^ (_snow:setup-configuration)

  (define (get var)
    (let ((str (_snow:getenv var)))
      (and str
           (not (string=? str ""))
           str)))

  (let* ((debug
          (let ((x (string->number (or (get "SNOW_DEBUG") "0"))))
            (if (or (not x)
                    (not (integer? x))
                    (not (exact? x)))
                _snow:debug
                x)))
         (test-packages
          (_snow:call-with-input-string
           (or (get "SNOW_TEST")
               "")
           (lambda (port)
             (let loop ((lst '()))
               (let ((x (read port)))
                 (if (eof-object? x)
                     (reverse lst)
                     (loop (cons x lst))))))))
         (home-dir
          (or (get "HOME")
              "."))
         (path
          (_snow:split-path
           (get "SNOW_PATH")
           _snow:path))
         (user-root
          (or (get "SNOW_USER_ROOT")
              (_snow:make-filename
               home-dir
               ".snow")))
         (site-root
          (or (get "SNOW_SITE_ROOT")
              "/Users/feeley/snow-site"))
         (user-dir
          (or (get "SNOW_USER_DIR")
              (_snow:make-filename
               user-root
               "v1.1.2")))
         (site-dir
          (or (get "SNOW_SITE_DIR")
              (_snow:make-filename
               site-root
               "v1.1.2")))
         (path-global
          (_snow:split-path
           (get "SNOW_PATH_GLOBAL")
           (append path
                   (list (_snow:make-filename user-dir "pack")
                         (_snow:make-filename site-dir "pack"))))))

    (set! _snow:debug debug)
    (set! _snow:test-packages test-packages)
    (set! _snow:user-root user-root)
    (set! _snow:site-root site-root)
    (set! _snow:user-dir user-dir)
    (set! _snow:site-dir site-dir)
    (set! _snow:path path-global)

    #f))

(_snow:eval-in-macro-environment (_snow:setup-configuration))

(define^ (_snow:trace-expansion expansion)
  (if (>= _snow:debug 2)
      (_snow:pretty-print expansion))
  expansion)

(define^ (_snow:trace-expansion-noisy expansion)
  (if (>= _snow:debug 1)
      (_snow:pretty-print expansion))
  expansion)

;;;----------------------------------------------------------------------------

;; Topological sorting.

(define^ (_snow:make-gnode obj links) ;; dependence graph node
  (cons obj links))

(define^ (_snow:gnode-obj x) ;; node's object
  (car x))

(define^ (_snow:gnode-id x) ;; node's identity
  (_snow:package-info-verspack (_snow:gnode-obj x)))

(define^ (_snow:gnode-links x) ;; identity of nodes that must come before node
  (cdr x))

(define^ (_snow:filter keep? lst)
  (cond ((null? lst)       '())
        ((keep? (car lst)) (cons (car lst) (_snow:filter keep? (cdr lst))))
        (else              (_snow:filter keep? (cdr lst)))))

(define^ (_snow:topological-sort graph)

  (define (every? pred? lst)
    (or (null? lst)
        (and (pred? (car lst))
             (every? pred? (cdr lst)))))

  ;; Operations on sets.

  (define (set-empty)
    '())

  (define (set-empty? s)
    (null? s))

  (define (set-singleton x)
    (list x))

  (define (set-union s1 s2)
    (cond ((null? s1)
           s2)
          ((set-member? (car s1) s2)
           (set-union (cdr s1) s2))
          (else
           (cons (car s1) (set-union (cdr s1) s2)))))

  (define (set-union-multi lst)
    (if (null? lst)
        (set-empty)
        (set-union (car lst) (set-union-multi (cdr lst)))))

  (define (set-intersection s1 s2)
    (cond ((null? s1)
           '())
          ((set-member? (car s1) s2)
           (cons (car s1) (set-intersection (cdr s1) s2)))
          (else
           (set-intersection (cdr s1) s2))))

  (define (set-diff s1 s2)
    (cond ((null? s1)
           '())
          ((set-member? (car s1) s2)
           (set-diff (cdr s1) s2))
          (else
           (cons (car s1) (set-diff (cdr s1) s2)))))

  (define (set-equal? s1 s2)
    (and (set-empty? (set-diff s1 s2))
         (set-empty? (set-diff s2 s1))))

  (define (set-member? x s)
    (_snow:verspack-member x s))

  (define (set->list s)
    s)

  (define (list->set lst)
    lst)

  (define (transitive-closure graph) ;; a graph is a list of nodes

    (define changed? #f)

    (define (closure links)
      (set-union-multi
        (cons links
              (map (lambda (id) (gnode-find-links id graph))
                   (set->list links)))))

    (let ((new-graph
            (map (lambda (x)
                   (let ((new-links (closure (_snow:gnode-links x))))
                     (if (not (= (length (set->list new-links))
                                 (length (set->list (_snow:gnode-links x)))))
                         (set! changed? #t))
                     (_snow:make-gnode (_snow:gnode-obj x) new-links)))
                 graph)))
      (if changed?
          (transitive-closure new-graph)
          new-graph)))

  (define (gnode-find-links id graph)
    (if (null? graph)
        (set-empty)
        (let ((node (car graph)))
          (if (_snow:verspack-eqv? (_snow:gnode-id node) id)
              (_snow:gnode-links node)
              (gnode-find-links id (cdr graph))))))

  (define (gnodes-remove graph gnodes)
    (if (null? graph)
        '()
        (let ((node (car graph)))
          (if (memq node gnodes)
              (gnodes-remove (cdr graph) gnodes)
              (cons node (gnodes-remove (cdr graph) gnodes))))))

  (define (remove-no-links graph)
    (let ((nodes-with-no-links
           (_snow:filter (lambda (x) (set-empty? (_snow:gnode-links x)))
                         graph)))
      (if (null? nodes-with-no-links)
          #f
          nodes-with-no-links)))

  (define (remove-cycle graph)

    (define (remove lst)
      (let* ((node (car lst))
             (links (_snow:gnode-links node)))

        (define (equal-links? x)
          (set-equal? (_snow:gnode-links x) links))

        (define (member-links? x)
          (set-member? (_snow:gnode-id x) links))

        (if (member-links? node)
            (let ((link-graph (_snow:filter member-links? graph)))
              (if (every? equal-links? link-graph)
                  link-graph
                  (remove (cdr lst))))
            (remove (cdr lst)))))

    (remove graph))

  (define (sort graph)

    (define (remove nodes)
      (let ((ids (list->set (map _snow:gnode-id nodes))))
        (map (lambda (x)
               (_snow:make-gnode
                (_snow:gnode-obj x)
                (set-diff (_snow:gnode-links x) ids)))
             (gnodes-remove graph nodes))))

    (if (null? graph)
        '()
        (let ((nodes-with-no-links (remove-no-links graph)))
          (if nodes-with-no-links
              (let* ((objs-with-no-links
                      (map _snow:gnode-obj nodes-with-no-links))
                     (rest
                      (sort (remove nodes-with-no-links))))
                (append objs-with-no-links
                        rest))
              (let* ((nodes-in-cycle
                      (remove-cycle graph))
                     (objs-in-cycle
                      (map _snow:gnode-obj nodes-in-cycle))
                     (rest
                      (sort (remove nodes-in-cycle))))
                (if (<= (length objs-in-cycle) 1)
                    (append objs-in-cycle
                            rest)
                    (cons objs-in-cycle
                          rest)))))))

  (sort (transitive-closure graph)))

;;;----------------------------------------------------------------------------

;; Searching the filesystem for a required package.

(define^ _snow:package-versions-cache '())

(define^ (_snow:package-versions-cached dir package-name-str)
  (let ((x (assoc dir _snow:package-versions-cache)))
    (if x
        (cdr x)
        (let* ((files
                (_snow:directory-files dir))
               (versions
                (_snow:sort-versions
                 (_snow:apply-append
                  (map (lambda (version-str)
                         (let* ((version-dir
                                 (_snow:make-filename dir version-str))
                                (disable-file
                                 (_snow:make-filename version-dir ".disable"))
                                (snow-package-filename
                                 (_snow:make-filename
                                  version-dir
                                  "snow"
                                  (string-append
                                   package-name-str
                                   _snow:package-extension)))
                                (version
                                 (_snow:string->version version-str)))
                           (if (and version
                                    (= 3 (length version))
                                    (_snow:file-exists? snow-package-filename)
                                    (not (_snow:file-exists? disable-file)))
                               (list version)
                               '())))
                       files)))))
          (set! _snow:package-versions-cache
                (cons (cons dir versions)
                      _snow:package-versions-cache))
          versions))))

(define^ (_snow:lookup-source-code-file filename-no-ext version)
  (let ((filename (string-append filename-no-ext _snow:package-extension)))
    (and (_snow:file-exists? filename)
         (not (_snow:file-directory? filename))
         (cons filename version))))

(define^ (_snow:lookup-package-in-filesys package-name-with-major-version dir)
  (let* ((package-name-str
          (symbol->string (car package-name-with-major-version)))
         (filename-no-ext
          (_snow:make-filename dir package-name-str)))
    (or (_snow:lookup-source-code-file filename-no-ext #f)
        (if (and (_snow:file-exists? filename-no-ext)
                 (_snow:file-directory? filename-no-ext))
            (or (let ((snow-dir (_snow:make-filename filename-no-ext "snow")))
                  (and (_snow:file-exists? snow-dir)
                       (_snow:file-directory? snow-dir)
                       (_snow:lookup-source-code-file
                        (_snow:make-filename snow-dir package-name-str)
                        #f)))
                (let loop ((versions
                            (_snow:package-versions-cached
                             filename-no-ext
                             package-name-str)))
                  (if (pair? versions)
                      (let ((version (car versions)))
                        (if (= (car version)
                               (cdr package-name-with-major-version))
                            (let* ((version-str
                                    (_snow:version->string version))
                                   (snow-dir
                                    (_snow:make-filename filename-no-ext
                                                         version-str
                                                         "snow")))
                              (or (_snow:lookup-source-code-file
                                   (_snow:make-filename snow-dir
                                                        package-name-str)
                                   version)
                                  (loop (cdr versions))))
                            (loop (cdr versions))))
                      #f)))
            #f))))

(define^ _snow:package-cache '())

(define^ (_snow:locate-package verspack cont)
  (let* ((package-name-with-major-version
          (cons (_snow:verspack-name verspack)
                (car (_snow:verspack-version verspack))))
         (x
          (assoc package-name-with-major-version _snow:package-cache)))
    (if x
        (cont (cdr x))
        (let loop ((search-path _snow:path))
          (if (not (pair? search-path))
              (_snow:syntax-err
               (string-append "can't find package "
                              (symbol->string
                               (car package-name-with-major-version))
                              "/v"
                              (number->string
                               (cdr package-name-with-major-version))))
              (let* ((dir
                      (car search-path))
                     (filename-and-version
                      (_snow:lookup-package-in-filesys
                       package-name-with-major-version
                       dir))
                     (filename
                      (and filename-and-version
                           (car filename-and-version)))
                     (version
                      (and filename-and-version
                           (cdr filename-and-version))))
                (if filename
                    (_snow:read-package-info
                     filename
                     (lambda (header)
                       (_snow:parse-package-header
                        (car package-name-with-major-version)
                        version
                        filename
                        header
                        (lambda (pi)
                          (let ((major-version
                                 (car (_snow:verspack-version
                                       (_snow:package-info-verspack pi)))))
                            (if (= major-version
                                   (cdr package-name-with-major-version))
                                (begin
                                  (set! _snow:package-cache
                                        (cons
                                         (cons package-name-with-major-version
                                               pi)
                                         _snow:package-cache))
                                  (cont pi))
                                (loop (cdr search-path))))))))
                    (loop (cdr search-path)))))))))

(define^ (_snow:read-package-info filename cont)
  (if (>= _snow:debug 2)
      (begin
        (display "Reading package ")
        (display filename)
        (newline)))
  ((with-input-from-file filename
     (lambda ()

       (define eol-chars '(10 13))

       ;; If the file starts with "#" or "@" we treat it as a script
       ;; and ignore the first line.

       (if (member (peek-char) '(#\# #\@))
           (let loop ()
             (let ((c (peek-char)))
               (if (not (eof-object? c))
                   (begin
                     (read-char)
                     (if (not (memv (char->integer c) eol-chars))
                         (loop)))))))

       ;; Allow any number of strings before the package* form (to
       ;; work around #! ... !# script syntax of Guile and Scsh).

       (let loop ()
         (let ((x (read)))
           (if (string? x)
               (loop)
               (lambda () (cont x)))))))))

(define^ (_snow:parse-package-info verspack-or-filename cont)
  (if (string? verspack-or-filename)
      (let* ((filename
              verspack-or-filename)
             (package-name
              (string->symbol
               (_snow:filename-strip-extension
                (_snow:filename-strip-directory filename)))))
        (_snow:read-package-info
         filename
         (lambda (header)
           (_snow:parse-package-header
            package-name
            #f
            filename
            header
            cont))))
      (_snow:locate-package verspack-or-filename cont)))

(define^ (_snow:valid-package-name? name)

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

(define^ (_snow:extract-verspack
          package-name-with-version
          full-version?
          fail
          cont)

  (define (err)
    (fail
     (if full-version?
         "package name must be a symbol of the form XXX/vN.N.N"
         "package name must be a symbol of the form XXX/vN, XXX/vN.N, or XXX/vN.N.N")))

  (if (not (symbol? package-name-with-version))
      (err)
      (let* ((str (symbol->string package-name-with-version))
             (i (_snow:string-tail-index str (string-length str) #\/)))
        (if (or (not i)
                (= i 0))
            (err)
            (let ((package-name-str
                   (substring str 0 (- i 1))))
              (if (not (_snow:valid-package-name? package-name-str))
                  (fail
                   (string-append "invalid package name " package-name-str))
                  (let* ((package-name
                          (string->symbol package-name-str))
                         (package-version-str
                          (substring str i (string-length str)))
                         (package-version
                          (_snow:string->version package-version-str)))
                    (if (not package-version)
                        (err)
                        (cont (_snow:make-verspack
                               package-name
                               package-version))))))))))

(define^ (_snow:parse-package-header
          expected-package-name
          expected-version
          filename
          header
          cont)

  (define meta-data-attributes
    '(maintainer:
      author:
      homepage:
      description:
      keywords:
      license:))

  (define (parse-after-intf verspack intf-reqs intf-defs after-intf)
    (let loop ((lst after-intf))
      (if (and (pair? lst)
               (pair? (car lst))
               (memq (car (car lst)) meta-data-attributes))
          (loop (cdr lst))
          (_snow:extract-requirements
           (list 'filename= filename)
           lst
           (lambda (impl-reqs after-reqs)
             (if (not (null? after-reqs))
                 (_snow:syntax-err
                  "package form syntax error"
                  (list 'filename= filename))
                 (_snow:parse-definitions
                  verspack
                  intf-defs
                  (lambda (defs uses-define-syntax?)
                    (cont
                     (_snow:make-package-info
                      verspack
                      filename
                      (append _snow:snow-builtin-packages intf-reqs)
                      (append _snow:snow-builtin-packages impl-reqs)
                      defs
                      uses-define-syntax?))))))))))

  (if (not (and (list? header)
                (>= (length header) 2)
                (eq? (car header) 'package*)))
      (_snow:syntax-err
       (string-append
        "a package* form was expected at the top of "
        (if filename
            (string-append "file " filename)
            "main program")))
      (let ((package-name-with-version (cadr header)))
        (_snow:extract-verspack
         package-name-with-version
         #t
         (lambda (msg)
           (_snow:syntax-err
            msg
            (list 'filename= filename
                  'package-name-with-version= package-name-with-version)))
         (lambda (verspack)
           (cond ((not (eq? (_snow:verspack-name verspack)
                            expected-package-name))
                  (_snow:syntax-err
                   "package name and filename must match"
                   (list 'filename= filename
                         'package-name-with-version=
                         package-name-with-version)))
                 ((and expected-version
                       (not (equal? (_snow:verspack-version verspack)
                                    expected-version)))
                  (_snow:syntax-err
                   "package version is inconsistent"
                   (list 'filename= filename
                         'package-name-with-version=
                         package-name-with-version
                         'expected=
                         (string->symbol
                          (_snow:verspack->string
                           (_snow:make-verspack expected-package-name
                                                expected-version))))))
                 (else
                  (_snow:expand-cond-expand
                   (cddr header)
                   (lambda (rest)
                     (cond ((not (and (pair? rest)
                                      (pair? (car rest))
                                      (eq? (car (car rest)) 'provide:)))
                            (parse-after-intf
                             verspack
                             '()
                             '()
                             rest))
                           (else
                            (let ((provide-form (car rest))
                                  (after-intf (cdr rest)))
                              (if (not (and (list? provide-form)
                                            (>= (length provide-form) 1)))
                                  (_snow:syntax-err
                                   "provide form syntax error"
                                   (list 'filename= filename))
                                  (_snow:expand-cond-expand
                                   (cdr provide-form)
                                   (lambda (rest)
                                     (_snow:extract-requirements
                                      (list 'filename= filename)
                                      rest
                                      (lambda (intf-reqs intf-defs)
                                        (parse-after-intf
                                         verspack
                                         intf-reqs
                                         intf-defs
                                         after-intf))))))))))))))))))

(define^ (_snow:expand-cond-expand exprs cont)

  (define (test feature cont)

    (define (err)
      (_snow:syntax-err "cond-expand feature expression syntax-error"))

    (cond ((eq? feature 'else)
           (cont #t))
          ((symbol? feature)
           (cont (eq? feature _snow:host)))
          ((and (pair? feature)
                (list? feature))
           (case (car feature)
             ((and)
              (let loop ((lst (cdr feature)))
                (if (pair? lst)
                    (test (car lst)
                          (lambda (val) (if val (loop (cdr lst)) (cont #f))))
                    (cont #t))))
             ((or)
              (let loop ((lst (cdr feature)))
                (if (pair? lst)
                    (test (car lst)
                          (lambda (val) (if val (cont #t) (loop (cdr lst)))))
                    (cont #f))))
             ((not)
              (if (= (length feature) 2)
                  (test (cadr feature)
                        (lambda (val) (cont (not val))))
                  (err)))
             (else
              (err))))
          (else
           (err))))

  (if (pair? exprs)
      (let ((expr (car exprs)))
        (cond ((and (pair? expr)
                    (eq? (car expr) 'cond-expand))
               (let loop ((clauses (cdr expr)))
                 (if (pair? clauses)
                     (let ((clause (car clauses)))
                       (if (not (and (pair? clause)
                                     (list? clause)))
                           (_snow:syntax-err "cond-expand syntax error")
                           (test (car clause)
                                 (lambda (val)
                                   (if val
                                       (_snow:expand-cond-expand
                                        (append (cdr clause) (cdr exprs))
                                        cont)
                                       (loop (cdr clauses)))))))
                     (_snow:syntax-err "unfulfilled cond-expand"))))
              ((and (pair? expr)
                    (eq? (car expr) 'test*))
               (_snow:expand-cond-expand
                (if _snow:tests-enabled?
                    (append (cdr expr) (cdr exprs))
                    (cdr exprs))
                cont))
              (else
               (_snow:expand-cond-expand
                (cdr exprs)
                (lambda (rest)
                  (cont (cons expr rest)))))))
      (cont exprs)))

(define^ (_snow:extract-requirements where forms cont)
  (let loop ((lst forms)
             (rev-requirements '()))

    (define (done)
      (cont (_snow:combine-requirements (reverse rev-requirements))
            lst))

    (if (pair? lst)
        (let ((form (car lst)))
          (if (and (list? form)
                   (>= (length form) 1)
                   (eq? (car form) 'require:))
              (if (not (= (length form) 2))
                  (_snow:syntax-err "require form syntax error" where)
                  (_snow:extract-verspack
                   (cadr form)
                   #f
                   (lambda (msg)
                     (_snow:syntax-err msg where))
                   (lambda (verspack)
                     (let ((package-name-str
                            (symbol->string (_snow:verspack-name verspack))))
                       (if (not (_snow:valid-package-name? package-name-str))
                           (_snow:syntax-err
                            (string-append
                             "invalid package name "
                             package-name-str)
                            where)
                           (loop (cdr lst)
                                 (cons verspack
                                       rev-requirements)))))))
              (done)))
        (done))))

(define^ (_snow:make-package-info
          verspack
          filename
          intf-reqs
          impl-reqs
          intf-defs
          uses-define-syntax?)
  (vector verspack
          filename
          intf-reqs
          impl-reqs
          intf-defs
          uses-define-syntax?))

(define^ (_snow:package-info-verspack info)
  (vector-ref info 0))

(define^ (_snow:package-info-filename info)
  (vector-ref info 1))

(define^ (_snow:package-info-intf-reqs info)
  (vector-ref info 2))

(define^ (_snow:package-info-impl-reqs info)
  (vector-ref info 3))

(define^ (_snow:package-info-intf-defs info)
  (vector-ref info 4))

(define^ (_snow:package-info-uses-define-syntax? info)
  (vector-ref info 5))

(define^ (_snow:lookup-package verspack package-list)
  (let loop ((lst package-list))
    (if (pair? lst)
        (let ((pi (car lst)))
          (if (_snow:verspack-eqv? verspack (_snow:package-info-verspack pi))
              pi
              (loop (cdr lst))))
        #f)))

(define^ (_snow:parse-definitions verspack forms cont)

  (define (err)
    (_snow:syntax-err
     "expected a define, define*, define-syntax, define-macro, define-macro* or define-record* form"))

  (let loop ((lst forms)
             (uses-define-syntax? #f)
             (rev-defs '()))
    (cond ((pair? lst)
           (let ((form (car lst)))
             (if (not (and (list? form)
                           (>= (length form) 2)))
                 (err)
                 (let ((form-name (car form)))
                   (case form-name

                     ((define define*)
                      (_snow:parse-define-var-or-macro*
                       form-name
                       'var
                       #t
                       verspack
                       (cadr form)
                       (cddr form)
                       (lambda (def)
                         (loop (cdr lst)
                               uses-define-syntax?
                               (cons def rev-defs)))))

                     ((define-syntax define-macro define-macro*)
                      (_snow:parse-define-var-or-macro*
                       form-name
                       (if (eq? form-name 'define-syntax)
                           'syntax
                           'macro)
                       #f
                       verspack
                       (cadr form)
                       (cddr form)
                       (lambda (def)
                         (loop (cdr lst)
                               (or uses-define-syntax?
                                   (eq? form-name 'define-syntax))
                               (cons def rev-defs)))))

                     ((define-record*)
                      (_snow:parse-define-record*
                       verspack
                       (cadr form)
                       (cddr form)
                       (lambda (def)
                         (loop (cdr lst)
                               uses-define-syntax?
                               (cons def rev-defs)))))

                     (else
                      (err)))))))
          ((not (null? lst))
           (_snow:syntax-err "package form provide form syntax error"))
          (else
           (cont (reverse rev-defs) uses-define-syntax?)))))

(define^ (_snow:order-packages package-list kind)

  ;; Order the packages according to the requirements.

  (_snow:topological-sort
   (map (lambda (pi)
          (_snow:make-gnode pi (_snow:package-dependencies pi kind)))
        package-list)))

(define^ (_snow:get-req-packages-aux
          all-packages
          req-packages
          reqs
          kind
          deep?
          cont)

  (define (add-reqs pi)
    (if deep?
        (append (_snow:package-dependencies pi kind)
                (cdr reqs))
        (cdr reqs)))

  (if (pair? reqs)

      (let ((verspack (car reqs)))

        (define (check-version pi all-packages req-packages reqs)
          (if (and (not (string? verspack))
                   (not (_snow:compatible-version-with
                         (_snow:verspack-version
                          (_snow:package-info-verspack pi))
                         (_snow:verspack-version verspack))))
              (_snow:syntax-err
               (string-append
                "required package "
                (_snow:verspack->string verspack)
                " has a higher version than "
                (_snow:verspack->string
                 (_snow:package-info-verspack pi))
                " which is the highest available (check the snowfort for a higher version)"))
              (_snow:get-req-packages-aux
               all-packages
               req-packages
               reqs
               kind
               deep?
               cont)))

        (cond ((_snow:lookup-package verspack req-packages)
               =>
               (lambda (pi)
                 ;; package is already in list of required packages
                 (check-version
                  pi
                  all-packages
                  req-packages
                  (cdr reqs))))
              ((_snow:lookup-package verspack all-packages)
               =>
               (lambda (pi)
                 ;; package has already been read
                 (check-version
                  pi
                  all-packages
                  (cons pi req-packages)
                  (add-reqs pi))))
              (else
               ;; package must be located in the file system and read
               (_snow:parse-package-info
                verspack
                (lambda (pi)
                  (check-version
                   pi
                   (cons pi all-packages)
                   (cons pi req-packages)
                   (add-reqs pi)))))))

      (let* ((op (_snow:order-packages req-packages kind))
             (cycles (_snow:filter pair? op)))
        (if (null? cycles)
            (cont all-packages op)
            (_snow:syntax-err
             "mutually dependent packages:"
             (map (lambda (lst)
                    (map (lambda (pi)
                           (string->symbol
                            (_snow:verspack->string
                             (_snow:package-info-verspack pi))))
                         lst))
                  cycles))))))

(define^ (_snow:get-req-packages all-packages reqs kind deep? cont)
  (_snow:get-req-packages-aux all-packages '() reqs kind deep? cont))

(define^ (_snow:package-dependencies package-info kind)
  (_snow:combine-requirements
   (case kind
     ((intf)
      (_snow:package-info-intf-reqs package-info))
     ((impl)
      (_snow:package-info-impl-reqs package-info))
     (else
      (append (_snow:package-info-intf-reqs package-info)
              (_snow:package-info-impl-reqs package-info))))))

(define^ (_snow:parse-define-var-or-macro*
          form-name
          kind
          allow-incomplete?
          verspack
          pattern
          body
          cont)
  (_snow:parse-pattern
   pattern
   kind
   (lambda (name)

     (define (return params-descr)
       (cont
        (case kind
          ((var)
           (_snow:make-var-def
            verspack
            name
            params-descr
            body))
          (else
           (_snow:make-macro-def
            verspack
            name
            params-descr
            body
            kind)))))

     (cond ((and (null? body)
                 (not allow-incomplete?))
            (_snow:syntax-err
             (string-append
              (symbol->string form-name)
              " definition must be complete")))
           ((pair? pattern)
            (if (eq? kind 'syntax)
                (_snow:syntax-err
                 "parameter list is not allowed in define-syntax")
                (_snow:parse-formals
                 (memq form-name '(define* define-macro*))
                 (cdr pattern)
                 return)))
           (else
            (return #f))))))

(define^ (_snow:parse-pattern pattern kind cont)
  (cond ((pair? pattern)
         (let ((name (car pattern)))
           (if (not (symbol? name))
               (_snow:syntax-err "name in definition must be a symbol")
               (cont name))))
        ((symbol? pattern)
         (cont pattern))
        (else
         (_snow:syntax-err "invalid name in definition"))))

(define^ (_snow:make-params-descr
          positional-before-named?
          positional-reqs
          positional-opts
          named
          rest)
  (vector positional-before-named?
          positional-reqs
          positional-opts
          named
          rest))

(define^ (_snow:params-descr-positional-before-named? params-descr)
  (vector-ref params-descr 0))

(define^ (_snow:params-descr-positional-reqs params-descr)
  (vector-ref params-descr 1))

(define^ (_snow:params-descr-positional-opts params-descr)
  (vector-ref params-descr 2))

(define^ (_snow:params-descr-named params-descr)
  (vector-ref params-descr 3))

(define^ (_snow:params-descr-rest params-descr)
  (vector-ref params-descr 4))

(define^ (_snow:parse-formals srfi-89-style? formals cont)

  (define (variable? x)
    (symbol? x))

  (define (required-positional? x)
    (variable? x))

  (define (optional-positional? x)
    (and srfi-89-style?
         (pair? x)
         (pair? (cdr x))
         (null? (cddr x))
         (variable? (car x))))

  (define (required-named? x)
    (and srfi-89-style?
         (pair? x)
         (pair? (cdr x))
         (null? (cddr x))
         (_snow:source-code-keyword? (car x))
         (variable? (cadr x))))

  (define (optional-named? x)
    (and srfi-89-style?
         (pair? x)
         (pair? (cdr x))
         (pair? (cddr x))
         (null? (cdddr x))
         (_snow:source-code-keyword? (car x))
         (variable? (cadr x))))

  (define (named? x)
    (or (required-named? x)
        (optional-named? x)))

  (define (duplicates? lst)
    (cond ((null? lst)
           #f)
          ((memq (car lst) (cdr lst))
           #t)
          (else
           (duplicates? (cdr lst)))))

  (define (parse-positional-section lst cont)
    (let loop1 ((lst lst) (rev-reqs '()))
      (if (and (pair? lst)
               (required-positional? (car lst)))
          (loop1 (cdr lst) (cons (car lst) rev-reqs))
          (let loop2 ((lst lst) (rev-opts '()))
            (if (and (pair? lst)
                     (optional-positional? (car lst)))
                (loop2 (cdr lst) (cons (car lst) rev-opts))
                (cont lst
                      (cons (reverse rev-reqs)
                            (reverse rev-opts))))))))

  (define (parse-named-section lst cont)
    (let loop ((lst lst) (rev-named '()))
      (if (and (pair? lst)
               (named? (car lst)))
          (loop (cdr lst)
                (cons (cons (_snow:source-code-keyword->keyword (caar lst))
                            (cdar lst))
                      rev-named))
          (cont lst
                (reverse rev-named)))))

  (define (parse-rest lst
                      positional-before-named?
                      positional-reqs/opts
                      named
                      cont)
    (if (null? lst)
        (parse-end positional-before-named?
                   positional-reqs/opts
                   named
                   #f
                   cont)
        (if (variable? lst)
            (parse-end positional-before-named?
                       positional-reqs/opts
                       named
                       lst
                       cont)
            (_snow:syntax-err "syntax error in formal parameter list"))))

  (define (parse-end positional-before-named?
                     positional-reqs/opts
                     named
                     rest
                     cont)
    (let ((positional-reqs (car positional-reqs/opts))
          (positional-opts (cdr positional-reqs/opts)))
      (let ((vars
             (append positional-reqs
                     (map car positional-opts)
                     (map cadr named)
                     (if rest (list rest) '())))
            (keys
             (map car named)))
        (cond ((duplicates? vars)
               (_snow:syntax-err
                "duplicate variable in formal parameter list"))
              ((duplicates? keys)
               (_snow:syntax-err
                "duplicate keyword in formal parameter list"))
              (else
               (cont
                (_snow:make-params-descr
                 positional-before-named?
                 positional-reqs
                 positional-opts
                 named
                 rest)))))))

  (define (parse lst cont)
    (if (and (pair? lst)
             (named? (car lst)))
        (parse-named-section
         lst
         (lambda (lst named)
           (parse-positional-section
            lst
            (lambda (lst positional-reqs/opts)
              (parse-rest lst
                          #f
                          positional-reqs/opts
                          named
                          cont)))))
        (parse-positional-section
         lst
         (lambda (lst positional-reqs/opts)
           (parse-named-section
            lst
            (lambda (lst named)
              (parse-rest lst
                          #t
                          positional-reqs/opts
                          named
                          cont)))))))

  (parse formals cont))

(define^ (_snow:expand-lambda params-descr body)
  (let ((positional-before-named?
         (_snow:params-descr-positional-before-named? params-descr))
        (positional-reqs
         (_snow:params-descr-positional-reqs params-descr))
        (positional-opts
         (_snow:params-descr-positional-opts params-descr))
        (named
         (_snow:params-descr-named params-descr))
        (rest
         (_snow:params-descr-rest params-descr)))
    (if (and (null? positional-opts) (null? named)) ;; direct R5RS equivalent

        `(lambda ,(append positional-reqs (or rest '())) ,@body)

        (let* ((utility-fns
                `(,@(if (or positional-before-named?
                            (null? positional-reqs))
                        `()
                        `(
                          (_snow:req
                           (lambda ()
                             (if (pair? _snow:args)
                                 (let ((arg (car _snow:args)))
                                   (set! _snow:args (cdr _snow:args))
                                   arg)
                                 (snow-error
                                  "too few actual parameters"))))
                         ))
                  ,@(if (null? positional-opts)
                        `()
                        `(
                          (_snow:opt
                           (lambda (default)
                             (if (pair? _snow:args)
                                 (let ((arg (car _snow:args)))
                                   (set! _snow:args (cdr _snow:args))
                                   arg)
                                 (default))))
                         ))))
               (positional-bindings
                `(,@(if positional-before-named?
                        `()
                        (map (lambda (x)
                               `(,x (_snow:req)))
                             positional-reqs))
                  ,@(map (lambda (x)
                           `(,(car x) (_snow:opt (lambda () ,(cadr x)))))
                         positional-opts)))
               (named-bindings
                (if (null? named)
                    `()
                    `(
                      (_snow:key-values
                       (vector ,@(map (lambda (x)
                                        `(snow-get-undefined))
                                      named)))
                      (_snow:args
                       (snow-process-keys
                        _snow:args
                        ',(_snow:make-perfect-hash-table
                           (map (lambda (x i)
                                  (cons (car x) i))
                                named
                                (_snow:range 0 (length named))))
                        _snow:key-values))
                      ,@(map (lambda (x i)
                               `(,(cadr x)
                                 ,(if (null? (cddr x))
                                      `(snow-req-key
                                        _snow:key-values
                                        ,i)
                                      `(snow-opt-key
                                        _snow:key-values
                                        ,i
                                        (lambda ()
                                          ,(caddr x))))))
                             named
                             (_snow:range 0 (length named)))
                     )))
               (rest-binding
                (if (not rest)
                    `(
                      (_snow:args
                       (or (null? _snow:args)
                           (snow-error
                            "too many actual parameters")))
                     )
                    `(
                      (,rest _snow:args)
                     )))
               (bindings
                (append (if positional-before-named?
                            (append utility-fns
                                    positional-bindings
                                    named-bindings)
                            (append named-bindings
                                    utility-fns
                                    positional-bindings))
                        rest-binding)))
          `(lambda ,(append (if positional-before-named?
                                positional-reqs
                                '())
                            '_snow:args)
             (let* ,bindings
               ,@body))))))

(define^ (_snow:make-perfect-hash-table alist)

  ;; "alist" is a list of pairs of the form "(keyword . value)"

  ;; The result is a perfect hash-table represented as a vector of
  ;; length 2*N, where N is the hash modulus.  If the keyword K is in
  ;; the hash-table it is at index
  ;;
  ;;   X = (* 2 (_snow:hash-keyword K N))
  ;;
  ;; and the associated value is at index X+1.

  (let loop1 ((n (length alist)))
    (let ((v (make-vector (* 2 n) #f)))
      (let loop2 ((lst alist))
        (if (pair? lst)
            (let* ((key-val (car lst))
                   (key (car key-val)))
              (let ((x (* 2 (_snow:hash-keyword key n))))
                (if (vector-ref v x)
                    (loop1 (+ n 1))
                    (begin
                      (vector-set! v x key)
                      (vector-set! v (+ x 1) (cdr key-val))
                      (loop2 (cdr lst))))))
            v)))))

(define^ (_snow:hash-keyword key n)
  (let ((str (keyword->string key)))
    (let loop ((h 0) (i 0))
      (if (< i (string-length str))
          (loop (modulo (+ (* h 65536) (char->integer (string-ref str i)))
                        n)
                (+ i 1))
          h))))

(define^ (_snow:parse-define-record*
          verspack
          name
          options-and-fields
          cont)

  (define (symbol-or-false? obj)
    (or (symbol? obj) (eq? obj #f)))

  (let ((allowed-options
         (list (cons 'parent: symbol?)
               (cons 'uid: symbol?)
               (cons 'sealed: boolean?)
               (cons 'constructor: symbol-or-false?)
               (cons 'predicate: symbol-or-false?)
               (cons 'rtd: symbol?)
               (cons 'prefix: string?)
               (cons 'type-checks: boolean?))))

    (define (get-parent options)
      (let ((x (assq 'parent: options)))
        (if x (cdr x) #f)))

    (define (get-sealed options)
      (let ((x (assq 'sealed: options)))
        (if x (cdr x) #t)))

    (define (get-constructor options)
      (let ((x (assq 'constructor: options)))
        (if x (cdr x) (_snow:sym "make-" name))))

    (define (get-predicate options)
      (let ((x (assq 'predicate: options)))
        (if x (cdr x) (_snow:sym name "?"))))

    (define (get-rtd options)
      (let ((x (assq 'rtd: options)))
        (if x (cdr x) (_snow:sym name "*"))))

    (define (get-uid options)
      (let ((x (assq 'uid: options)))
        (if x (cdr x) #f)))

    (define (get-prefix options)
      (let ((x (assq 'prefix: options)))
        (if x (cdr x) "")))

    (define (get-type-checks options)
      (let ((x (assq 'type-checks: options)))
        (if x (cdr x) #t)))

    (define (parse-options-and-fields lst rev-options rev-fields all cont)

      (define (add-field rest name getter setter)
        (cond ((not (symbol? name))
               (_snow:syntax-err "illegal field name in define-record*"))
              ((not (symbol? getter))
               (_snow:syntax-err "illegal field getter in define-record*"))
              ((not (symbol? setter))
               (_snow:syntax-err "illegal field setter in define-record*"))
              (else
               (add-getter/setter rest name getter setter))))

      (define (add-getter/setter rest name getter setter)
        (cond ((assq name rev-fields)
               (_snow:syntax-err "duplicate field name in define-record*"))
              ((or (memq getter all)
                   (memq setter (cons getter all)))
               (_snow:syntax-err "name clash in define-record*"))
              (else
               (parse-options-and-fields
                rest
                rev-options
                (cons (list name
                            getter
                            setter)
                      rev-fields)
                (cons setter (cons getter all))
                cont))))

      (cond ((pair? lst)
             (let* ((head (car lst))
                    (rest (cdr lst))
                    (x (assq head allowed-options)))
               (cond ((not x)
                      (cond ((list? head)
                             (if (not (= 3 (length head)))
                                 (_snow:syntax-err
                                  "illegal field in define-record*")
                                 (let ((name (car head))
                                       (getter (cadr head))
                                       (setter (caddr head)))
                                   (add-field rest name getter setter))))
                            ((not (symbol? head))
                             (_snow:syntax-err
                              "illegal field name in define-record*"))
                            (else
                             (add-getter/setter
                              rest
                              head
                              (_snow:sym name "-" head)
                              (_snow:sym name "-" head "-set!")))))
                     ((assq head rev-options)
                      (_snow:syntax-err
                       "duplicate option in define-record*"))
                     ((not (pair? rest))
                      (_snow:syntax-err
                       "absent option value in define-record*"))
                     (else
                      (let ((val (car rest)))
                        (if (not ((cdr x) val))
                            (_snow:syntax-err
                             "illegal option value in define-record*")
                            (parse-options-and-fields
                             (cdr rest)
                             (cons (cons head val) rev-options)
                             rev-fields
                             all
                             cont)))))))
            ((null? lst)
             (cont (reverse rev-options)
                   (reverse rev-fields)
                   all))
            (else
             (_snow:syntax-err "improperly terminated define-record*"))))

    (if (not (symbol? name))
        (_snow:syntax-err "illegal record name in define-record*")
        (parse-options-and-fields
         options-and-fields
         '()
         '()
         '()
         (lambda (options fields all)
           (let ((constructor (get-constructor options))
                 (predicate (get-predicate options))
                 (rtd (get-rtd options)))
             (if (or (memq rtd all)
                     (and constructor
                          (memq constructor (cons rtd all)))
                     (and predicate
                          (memq predicate (cons constructor (cons rtd all)))))
                 (_snow:syntax-err "name clash in define-record*")
                 (cont
                  (_snow:make-record-def
                   verspack
                   name
                   (get-parent options)
                   constructor
                   predicate
                   rtd
                   fields
                   (get-sealed options)
                   (get-uid options)
                   (get-prefix options)
                   (get-type-checks options))))))))))

(define^ (_snow:def-kind def)
  (vector-ref def 0))

(define^ (_snow:def-verspack def)
  (vector-ref def 1))

(define^ (_snow:def-name def)
  (vector-ref def 2))

(define^ (_snow:make-var-def
          verspack
          name
          params-descr
          body)
  (vector 'var
          verspack
          name
          params-descr
          body))

(define^ (_snow:make-macro-def
          verspack
          name
          params-descr
          body
          kind)
  (vector 'macro
          verspack
          name
          params-descr
          body
          kind))

(define^ (_snow:var-or-macro-def-params-descr def)
  (vector-ref def 3))

(define^ (_snow:var-or-macro-def-body def)
  (vector-ref def 4))

(define^ (_snow:macro-def-kind def)
  (vector-ref def 5))

(define^ (_snow:make-record-def
          verspack
          name
          parent
          constructor
          predicate
          rtd
          fields
          sealed
          uid
          prefix
          type-checks)
  (vector 'record
          verspack
          name
          parent
          constructor
          predicate
          rtd
          fields
          sealed
          uid
          prefix
          type-checks))

(define^ (_snow:record-def-parent def)
  (vector-ref def 3))

(define^ (_snow:record-def-constructor def)
  (vector-ref def 4))

(define^ (_snow:record-def-predicate def)
  (vector-ref def 5))

(define^ (_snow:record-def-rtd def)
  (vector-ref def 6))

(define^ (_snow:record-def-fields def)
  (vector-ref def 7))

(define^ (_snow:record-def-sealed def)
  (vector-ref def 8))

(define^ (_snow:record-def-uid def)
  (vector-ref def 9))

(define^ (_snow:record-def-prefix def)
  (vector-ref def 10))

(define^ (_snow:record-def-type-checks def)
  (vector-ref def 11))

(define^ (_snow:generate-var-or-macro-definition def)
  (let* ((kind (_snow:def-kind def))
         (name (_snow:def-name def))
         (verspack (_snow:def-verspack def))
         (params-descr (_snow:var-or-macro-def-params-descr def))
         (body (_snow:var-or-macro-def-body def)))
    (append
     (if params-descr
         (_snow:generate-keyword-defs
          (map car (_snow:params-descr-named params-descr)))
         '())
     (if (and (eq? kind 'var)
              (>= _snow:debug 1)
              (pair? body))
         `(
           (_snow:add-definition ',(string->symbol
                                    (_snow:verspack->string verspack))
                                 ',name)
          )
         '())
     (if (pair? body)

         (let ((expr
                (if params-descr
                    (_snow:expand-lambda params-descr body)
                    (car body))))
           `(
             (,(cond ((eq? kind 'var)
                      'define)
                     ((eq? (_snow:macro-def-kind def) 'syntax)
                      'define-syntax)
                     (else
                      'define-macro))
              ,name
              ,expr)
            ))

         '()))))

(define^ (_snow:warn-duplicates lst)

  (define (warn-dups lst seen)
    (if (pair? lst)
        (let ((x (car lst)))
          (cond ((assq (car x) seen)
                 =>
                 (lambda (other)
                   (display "*** SNOW WARNING -- ")
                   (display (_snow:verspack->string (cdr x)))
                   (display " and ")
                   (display (_snow:verspack->string (cdr other)))
                   (display " both provide ")
                   (write (car x))
                   (newline)
                   (warn-dups (cdr lst) seen)))
                (else
                 (warn-dups (cdr lst) (cons x seen)))))))

  (warn-dups lst '()))

;;;----------------------------------------------------------------------------

;; Expansion of (package* ...).

(define^ (_snow:get-exported-ids pi)

  (define (maybe name) (if name (list name) '()))

  (_snow:apply-append
   (map (lambda (def)
          (let ((kind (_snow:def-kind def))
                (name (_snow:def-name def)))
            (case kind
              ((var)
               (list name))
              ((macro)
               (list name))
              ((record)
               (map (lambda (n)
                      (_snow:sym (_snow:record-def-prefix def) n))
                    (append
                     (maybe name)
                     (maybe (_snow:record-def-constructor def))
                     (maybe (_snow:record-def-predicate def))
                     (maybe (_snow:record-def-rtd def))
                     (_snow:apply-append
                      (map cdr
                           (_snow:record-def-fields def))))))
              (else
               '()))))
        (_snow:package-info-intf-defs pi))))

(define^ (_snow:get-import-alist pi impl? all-req-packages)
  (let* ((verspack
          (_snow:package-info-verspack pi))
         (reqs
          (cons verspack
                (if impl?
                    (_snow:package-info-impl-reqs pi)
                    (_snow:package-info-intf-reqs pi))))
         (import-table
          (map (lambda (vp)
                 (let ((pi (_snow:lookup-package vp all-req-packages)))
                   (cons (_snow:package-info-verspack pi)
                         (_snow:get-exported-ids pi))))
               (_snow:combine-requirements reqs)))
         (import-alist
          (_snow:apply-append
           (map (lambda (p)
                  (let ((pn (car p)))
                    (map (lambda (n) (cons n pn))
                         (cdr p))))
                import-table))))

    (_snow:warn-duplicates import-alist)

    import-alist))

(define^ (_snow:generate-keyword-defs keywords)
  (_snow:apply-append
   (map (lambda (k)
          (let ((sck (_snow:keyword->source-code-keyword k)))
            (if (or (not (symbol? sck))
                    (memq sck _snow:current-defined-keywords))
                '()
                (begin
                  (set! _snow:current-defined-keywords
                        (cons sck _snow:current-defined-keywords))
                  `(
                    (define ,sck ',k)
                   )))))
        keywords)))

(define^ (_snow:generate-renamings pi)

  (define generic _snow:generate-renamings-generic)

  ((case _snow:target-host
     ((bigloo)   generic)
     ((chez)     generic)
     ((chicken)  _snow:generate-renamings-chicken)
     ((gambit)   _snow:generate-renamings-gambit)
     ((gauche)   generic)
     ((guile)    generic)
     ((kawa)     generic)
     ((larceny)  generic)
     ((mit)      generic)
     ((mzscheme) generic)
     ((rscheme)  generic)
     ((scheme48) generic)
     ((scm)      generic)
     ((scsh)     generic)
     ((sisc)     generic)
     ((stalin)   generic)
     ((stklos)   generic)
     (else       generic))
   pi))

(define^ (_snow:generate-package-import pi main-pi all-req-packages)
  (let* ((verspack
          (_snow:package-info-verspack pi))
         (intf-defs
          (_snow:package-info-intf-defs pi))
         (imported-for-main-impl?
          (or (eq? pi main-pi)
              (_snow:verspack-member
               verspack
               (_snow:package-info-impl-reqs main-pi)))))
    (append
     (_snow:generate-renamings pi)
     (_snow:apply-append
      (map (lambda (def)
             (let ((kind (_snow:def-kind def))
                   (name (_snow:def-name def)))
               (case kind
                 ((var macro)
                  (if imported-for-main-impl?
                      (_snow:generate-var-or-macro-definition def)
                      '()))
                 ((record)
                  (if (or imported-for-main-impl?
                          (not (_snow:record-def-sealed def)))
                      (_snow:generate-record-definition def (eq? pi main-pi))
                      '()))
                 (else
                  '()))))
           intf-defs)))))

(define^ (_snow:generate-all-package-imports package-info all-req-packages)
  (_snow:apply-append
   (map (lambda (pi)
          (_snow:generate-package-import pi package-info all-req-packages))
        all-req-packages)))

;;;----------------------------------------------------------------------------

;; Target system dependent functionality.

(define^ _snow:target-host ;; what are we generating code for?
  _snow:host
  ;; 'generic ;; assume a generic R5RS Scheme system
)

(define^ (_snow:syntax-err msg . args)
  (apply _snow:syntax-err-generic (cons msg args)))

(define^ (_snow:generate-include filename)
  ((case _snow:target-host
     ((bigloo)   _snow:generate-include-bigloo)
     ((chez)     _snow:generate-include-chez)
     ((chicken)  _snow:generate-include-chicken)
     ((gambit)   _snow:generate-include-gambit)
     ((gauche)   _snow:generate-include-gauche)
     ((guile)    _snow:generate-include-guile)
     ((kawa)     _snow:generate-include-kawa)
     ((larceny)  _snow:generate-include-larceny)
     ((mit)      _snow:generate-include-mit)
     ((mzscheme) _snow:generate-include-mzscheme)
     ((rscheme)  _snow:generate-include-rscheme)
     ((scheme48) _snow:generate-include-scheme48)
     ((scm)      _snow:generate-include-scm)
     ((scsh)     _snow:generate-include-scsh)
     ((sisc)     _snow:generate-include-sisc)
     ((stalin)   _snow:generate-include-stalin)
     ((stklos)   _snow:generate-include-stklos)
     (else       _snow:generate-include-generic))
   filename))

(define^ (_snow:generate-snow-special-form-macros)

  ;; Macro definitions for Snow special forms.

  (append
   '(
     (define-macro (define* pattern . body)
       `(_snow:eval-in-macro-environment
         (_snow:expand-macro-define*
          ',pattern
          ',body)))

     (define-macro (define-macro* pattern . body)
       `(_snow:eval-in-macro-environment
         (_snow:expand-macro-define-macro*
          ',pattern
          ',body)))

     (define-macro (define-record* name . options-and-fields)
       `(_snow:eval-in-macro-environment
         (_snow:expand-macro-define-record*
          ',name
          ',options-and-fields)))

     (define-macro (test* . body)
       `(_snow:eval-in-macro-environment
         (_snow:expand-macro-test* ',body)))

     (define-macro (expect* expr)
       `(_snow:eval-in-macro-environment
         (_snow:expand-macro-expect* ',expr)))
    )

   `(
     (define-macro (snow-site-root)
       ,_snow:site-root)

     (define-macro (snow-site-dir)
       ,_snow:site-dir)

     (define-macro (snow-user-root)
       ,_snow:user-root)

     (define-macro (snow-user-dir)
       ,_snow:user-dir)
    )))

(define^ (_snow:generate-record-macros)
  ((case _snow:target-host
     ((bigloo)   _snow:generate-record-macros-bigloo)
     ((chez)     _snow:generate-record-macros-chez)
     ((chicken)  _snow:generate-record-macros-chicken)
     ((gambit)   _snow:generate-record-macros-gambit)
     ((gauche)   _snow:generate-record-macros-gauche)
     ((guile)    _snow:generate-record-macros-guile)
     ((kawa)     _snow:generate-record-macros-kawa)
     ((larceny)  _snow:generate-record-macros-larceny)
     ((mit)      _snow:generate-record-macros-mit)
     ((mzscheme) _snow:generate-record-macros-mzscheme)
     ((rscheme)  _snow:generate-record-macros-rscheme)
     ((scheme48) _snow:generate-record-macros-scheme48)
     ((scm)      _snow:generate-record-macros-scm)
     ((scsh)     _snow:generate-record-macros-scsh)
     ((sisc)     _snow:generate-record-macros-sisc)
     ((stalin)   _snow:generate-record-macros-stalin)
     ((stklos)   _snow:generate-record-macros-stklos)
     (else       _snow:generate-record-macros-generic))))

(define^ (_snow:generate-record-definition def main?)
  ((case _snow:target-host
     ((bigloo)   _snow:generate-record-definition-bigloo)
     ((chez)     _snow:generate-record-definition-chez)
     ((chicken)  _snow:generate-record-definition-chicken)
     ((gambit)   _snow:generate-record-definition-gambit)
     ((gauche)   _snow:generate-record-definition-gauche)
     ((guile)    _snow:generate-record-definition-guile)
     ((kawa)     _snow:generate-record-definition-kawa)
     ((larceny)  _snow:generate-record-definition-larceny)
     ((mit)      _snow:generate-record-definition-mit)
     ((mzscheme) _snow:generate-record-definition-mzscheme)
     ((rscheme)  _snow:generate-record-definition-rscheme)
     ((scheme48) _snow:generate-record-definition-scheme48)
     ((scm)      _snow:generate-record-definition-scm)
     ((scsh)     _snow:generate-record-definition-scsh)
     ((sisc)     _snow:generate-record-definition-sisc)
     ((stalin)   _snow:generate-record-definition-stalin)
     ((stklos)   _snow:generate-record-definition-stklos)
     (else       _snow:generate-record-definition-generic))
   def
   main?))

(define^ (_snow:generate-snow-macros)
  (append (_snow:generate-snow-special-form-macros)
          (_snow:generate-record-macros)))

;;;----------------------------------------------------------------------------

;; Generic implementation of package* form.

(define^ (_snow:syntax-err-generic msg . args)

  (display "*** SNOW ERROR -- ")
  (display msg)
  (for-each (lambda (x) (display " ") (write x)) args)
  (newline)

  ;; This generates illegal code, which will hopefully cause the
  ;; compiler to signal a syntax error.

  (cons 'syntax-error (list->vector (cons msg args))))

(define^ _snow:include*-dir #f)

(define^ (_snow:generate-load filename)
  `(begin
     (set! _snow:include*-dir ,(_snow:filename-directory filename))
     (load ,filename)))

(define^ (_snow:expand-macro-include*-cd filename)
  (set! _snow:include*-dir (_snow:filename-directory filename))
  (_snow:make-begin '()))

(define^ (_snow:generate-include-generic rel-filename)
  (let* ((old-dir
          _snow:include*-dir)
         (filename
          (string-append old-dir rel-filename))
         (exprs
          (_snow:read-expressions filename))
         (new-dir
          (_snow:filename-directory filename)))
    (_snow:make-begin
     `(
       (include*-cd ,new-dir)
       ,@exprs
       (include*-cd ,old-dir)
      ))))

(define^ (_snow:read-expressions filename)
  (let ((port (open-input-file filename)))
    (let loop ((rev-result '()))
      (let ((x (read port)))
        (if (eof-object? x)
            (begin
              (close-input-port port)
              (reverse rev-result))
            (loop (cons x rev-result)))))))

(define^ (_snow:generate-package-header-generic package-info all-req-packages)
  `(,@(_snow:generate-snow-macros)
    ,@(_snow:generate-all-package-imports package-info all-req-packages)))

(define^ (_snow:generate-renamings-generic pi)
  '())

(define^ (_snow:generate-record-macros-generic)

  ;; Macro definitions for the generic implementation of the record
  ;; system.

  '(
    (define-macro (_snow:make-record-instance rtd . fields)
      `(vector ,rtd ,@fields))

    (define-macro (_snow:indirect-field-ref obj i rtd)
      `(if (snow-indirect-instance-of? ,obj ,rtd)
           (_snow:unchecked-field-ref ,obj ,i ,rtd)
           (snow-field-ref-error ,obj ,i ,rtd)))

    (define-macro (_snow:indirect-field-set! obj val i rtd)
      `(if (snow-indirect-instance-of? ,obj ,rtd)
           (_snow:unchecked-field-set! ,obj ,val ,i ,rtd)
           (snow-field-set!-error ,obj ,val ,i ,rtd)))

    (define-macro (_snow:direct-field-ref obj i rtd)
      `(if (snow-direct-instance-of? ,obj ,rtd)
           (_snow:unchecked-field-ref ,obj ,i ,rtd)
           (snow-field-ref-error ,obj ,i ,rtd)))

    (define-macro (_snow:direct-field-set! obj val i rtd)
      `(if (snow-direct-instance-of? ,obj ,rtd)
           (_snow:unchecked-field-set! ,obj ,val ,i ,rtd)
           (snow-field-set!-error ,obj ,val ,i ,rtd)))

    (define-macro (_snow:unchecked-field-ref obj i rtd)
      `(vector-ref ,obj ,i))

    (define-macro (_snow:unchecked-field-set! obj val i rtd)
      `(vector-set! ,obj ,i ,val))

    (define-macro (snow-record . args)
      `(_snow:eval-in-macro-environment
        (_snow:expand-record-definition
         '#(#f
            1
            #f
            record-f500f984-e01f-453f-a785-e40fa920d601)
         ''#(#f
             record-f500f984-e01f-453f-a785-e40fa920d601)
         ',args)))

    (define-macro (_snow:make-rtd parent-rtd uid)
      `(vector ,parent-rtd ,uid))

    (define-macro (_snow:make-rtd-generative parent-rtd uid)
      `(vector ,parent-rtd (list ,uid)))

    (define-macro (_snow:rtd-parent-rtd rtd)
      `(vector-ref ,rtd 0))

    (define-macro (_snow:rtd-uid rtd)
      `(vector-ref ,rtd 1))))

(define^ (_snow:expand-record-definition ct-rtd rt-rtd args)
  (_snow:trace-expansion
   (if (not (pair? args))
       rt-rtd
       (let* ((x (car args))
              (type-macro-only? (memq 'type-macro-only (cdr args)))
              (verspack (_snow:def-verspack x))
              (name (_snow:def-name x))
              (parent (_snow:record-def-parent x))
              (constructor (_snow:record-def-constructor x))
              (predicate (_snow:record-def-predicate x))
              (rtd (_snow:record-def-rtd x))
              (fields (_snow:record-def-fields x))
              (sealed (_snow:record-def-sealed x))
              (uid (_snow:record-def-uid x))
              (pfx (_snow:record-def-prefix x))
              (tc (_snow:record-def-type-checks x))
              (parent-size (vector-ref ct-rtd 1))
              (parent-sealed (vector-ref ct-rtd 2))
              (parent-uid (vector-ref ct-rtd 3)))
         (cond (parent-sealed
                (_snow:syntax-err
                 "sealed record cannot be extended"))
               ((and (not parent-uid)
                     uid)
                (_snow:syntax-err
                 "nongenerative record cannot extend a generative record"))
               (else
                (let* ((n
                        (length fields))
                       (size
                        (+ parent-size n))
                       (indices
                        (_snow:range parent-size size)))

                  (define (pfx-sym s)
                    (_snow:sym pfx s))

                  (let* ((defs1
                           `(
                             (define-macro (,(pfx-sym name) . args)
                               `(_snow:eval-in-macro-environment
                                 (_snow:expand-record-definition
                                  ',',(vector ct-rtd
                                              size
                                              sealed
                                              uid)
                                  ',',(pfx-sym rtd)
                                  ',args)))
                            ))
                         (defs2
                          (if type-macro-only?
                              '()
                              `(
                                (define ,(pfx-sym rtd)
                                  ,(if uid
                                       `(_snow:make-rtd
                                         ,rt-rtd
                                         ',uid)
                                       `(_snow:make-rtd-generative
                                         ,rt-rtd
                                         ',name)))

                                ,@(if constructor
                                      (let ((_fields
                                             (map (lambda (i)
                                                    (_snow:sym
                                                     "_"
                                                     (number->string i)))
                                                  (_snow:range 1 size))))
                                        `(
                                          (define (,(pfx-sym constructor)
                                                   ,@_fields)
                                            (_snow:make-record-instance
                                             ,(pfx-sym rtd)
                                             ,@_fields))
                                         ))
                                      '())

                                ,@(if predicate
                                      `(
                                        (define (,(pfx-sym predicate) _1)
                                          (,(if sealed
                                                `snow-direct-instance-of?
                                                `snow-indirect-instance-of?)
                                           _1
                                           ,(pfx-sym rtd)))
                                       )
                                      '())

                                ,@(map
                                   (lambda (field i)
                                     `(define (,(pfx-sym (cadr field)) _1)
                                        (,(cond ((not tc)
                                                 `_snow:unchecked-field-ref)
                                                (sealed
                                                 `_snow:direct-field-ref)
                                                (else
                                                 `_snow:indirect-field-ref))
                                         _1
                                         ,i
                                         ,(pfx-sym rtd))))
                                   fields
                                   indices)

                                ,@(map
                                   (lambda (field i)
                                     `(define (,(pfx-sym (caddr field)) _1 _2)
                                        (,(cond ((not tc)
                                                 `_snow:unchecked-field-set!)
                                                (sealed
                                                 `_snow:direct-field-set!)
                                                (else
                                                 `_snow:indirect-field-set!))
                                         _1
                                         _2
                                         ,i
                                         ,(pfx-sym rtd))))
                                   fields
                                   indices)
                               ))))
                    (_snow:make-begin
                     (append defs1
                             defs2))))))))))

(define^ (_snow:generate-record-definition-generic def main?)
  (let ((parent-name
         (let ((parent (_snow:record-def-parent def)))
           (if (not parent)
               'snow-record
               parent))))
    (list (append (list parent-name
                        def)
                  (if main? '() '(type-macro-only))))))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Bigloo.

(define^ (_snow:generate-include-bigloo filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-bigloo package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-bigloo)
  '())

(define^ (_snow:generate-record-definition-bigloo def main?)
  (cond ((not main?)
         '())
        ((_snow:record-def-parent def)
         (error "record extension is not supported"))
        (else
         `(
           (define-struct ,(_snow:def-name def)
             ,@(map car (_snow:record-def-fields def)))
           (define ,(_snow:record-def-constructor def)
             ,(_snow:def-name def))
          ))))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Chez.

(define^ (_snow:generate-include-chez filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-chez package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-chez)
  '())

(define^ (_snow:generate-record-definition-chez def main?)
  (cond ((not main?)
         '())
        ((_snow:record-def-parent def)
         (error "record extension is not supported"))
        (else
         `(
           (define-record ,(_snow:def-name def)
             ,(map car (_snow:record-def-fields def)))
           ,@(map (lambda (field)
                    `(define ,(caddr field)
                       ,(_snow:sym "set-" (cadr field) "!")))
                  (_snow:record-def-fields def))
          ))))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Chicken.

(define^ (_snow:generate-include-chicken filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-namespace-chicken prefix ids)
  `(,(string->symbol "##sys#namespace") (,prefix ,@ids)))

(define^ (_snow:namespace-prefix-chicken pi)
  (string-append
   (symbol->string (_snow:package-name-with-major-version pi))
   "#"))

(define^ (_snow:generate-contour-chicken pi)
  `(
    (define-macro (,(string->symbol "##sys#namespace") . args)
      (,(string->symbol "snow#namespace-expand") args))
    ,(_snow:generate-namespace-chicken
      (_snow:namespace-prefix-chicken pi)
      '())
   ))

(define^ (_snow:generate-imports-chicken pi ids)
  (if (null? ids)
      `()
      `(,(_snow:generate-namespace-chicken
          (_snow:namespace-prefix-chicken pi)
          ids))))

(define^ (_snow:generate-renamings-chicken pi)
  (_snow:generate-imports-chicken pi (_snow:get-exported-ids pi)))

(define^ (_snow:generate-package-header-chicken package-info all-req-packages)
  `(
    ,@(_snow:generate-contour-chicken package-info)
    ,(_snow:generate-namespace-chicken "" _snow:ids-in-null-namespace)
    ,@(_snow:generate-snow-macros)
    ,@(_snow:generate-all-package-imports package-info all-req-packages)
   ))

(define^ _snow:ids-in-null-namespace
  '(_snow:eval-in-macro-environment
    _snow:expand-macro-package*
    _snow:expand-macro-load-program*
    _snow:expand-macro-include-program*
    _snow:expand-macro-define*
    _snow:expand-macro-define-macro*
    _snow:expand-macro-define-record*
    _snow:expand-macro-include*
    _snow:expand-macro-test*
    _snow:expand-macro-expect*
    _snow:expand-record-definition
    _snow:program-filename
    _snow:add-definition
    _snow:cleanup
    _snow:cleanup-handler-push!
    _snow:include*-dir

    package*
    load-program*
    include-program*
    define*
    define-macro*
    define-record*
    include*
    include*-cd
    test*
    expect*
    snow-site-root
    snow-site-dir
    snow-user-root
    snow-user-dir

    _snow:make-record-instance
    _snow:indirect-field-ref
    _snow:indirect-field-set!
    _snow:direct-field-ref
    _snow:direct-field-set!
    _snow:unchecked-field-ref
    _snow:unchecked-field-set!
    snow-record
    _snow:make-rtd
    _snow:make-rtd-generative
    _snow:rtd-parent-rtd
    _snow:rtd-uid))

(define^ (_snow:generate-record-macros-chicken)
  '())

(define^ (_snow:generate-record-definition-chicken def main?)
  (cond ((not main?)
         '())
        ((_snow:record-def-parent def)
         (error "record extension is not supported"))
        (else
         `(
           (define-record ,(_snow:def-name def)
             ,@(map car (_snow:record-def-fields def)))
          ))))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Gambit.

(define^ (_snow:generate-include-gambit filename)
  `(,(string->symbol "##include") ,filename))

(define^ (_snow:generate-namespace-gambit prefix ids)
  `(,(string->symbol "##namespace") (,prefix ,@ids)))

(define^ (_snow:namespace-prefix-gambit pi)
  (string-append
   (symbol->string (_snow:package-name-with-major-version pi))
   "#"))

(define^ (_snow:generate-contour-gambit pi)
  `(,(_snow:generate-namespace-gambit
      (_snow:namespace-prefix-gambit pi)
      '())
    ,(_snow:generate-include-gambit "~~/lib/gambit#.scm")))

(define^ (_snow:generate-imports-gambit pi ids)
  (if (null? ids)
      `()
      `(,(_snow:generate-namespace-gambit
          (_snow:namespace-prefix-gambit pi)
          ids))))

(define^ (_snow:generate-renamings-gambit pi)
  (_snow:generate-imports-gambit pi (_snow:get-exported-ids pi)))

(define^ (_snow:generate-package-header-gambit package-info all-req-packages)
  `(
    ,@(_snow:generate-contour-gambit package-info)
    ,(_snow:generate-namespace-gambit "" _snow:ids-in-null-namespace)
    ,@(_snow:generate-snow-macros)
    ,@(_snow:generate-all-package-imports package-info all-req-packages)
   ))

(define^ (_snow:generate-record-macros-gambit)
  '())

(define^ (_snow:generate-record-definition-gambit def main?)
  (cond ((not main?)
         '())
        ((_snow:record-def-parent def)
         (error "record extension is not supported"))
        (else
         `(
           (define-type ,(_snow:def-name def)
             ,@(map car (_snow:record-def-fields def)))
          ))))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Gauche.

(define^ (_snow:generate-include-gauche filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-gauche package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-gauche)
  '())

(define^ (_snow:generate-record-definition-gauche def main?)
  (cond ((not main?)
         '())
        ((_snow:record-def-parent def)
         (error "record extension is not supported"))
        (else
         (let ((_fields
                (map (lambda (i)
                       (_snow:sym "_" (number->string i)))
                     (_snow:range 0 (length (_snow:record-def-fields def))))))
           `(
             (define-class ,(_snow:def-name def) ()
               ,(map (lambda (field)
                       `(,(car field)
                         :init-keyword ,(make-keyword (symbol->string (car field)))
                         :getter ,(cadr field)
                         :setter ,(caddr field)))
                     (_snow:record-def-fields def)))
             (define (,(_snow:record-def-constructor def) ,@_fields)
               (make ,(_snow:def-name def)
                 ,@(_snow:apply-append
                    (map (lambda (field _field)
                           (list (make-keyword (symbol->string (car field)))
                                 _field))
                         (_snow:record-def-fields def)
                         _fields))))
             (define (,(_snow:record-def-predicate def) _1)
               (is-a? _1 ,(_snow:def-name def)))
            )))))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Guile.

(define^ (_snow:generate-include-guile filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-guile package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-guile)
  '())

(define^ (_snow:generate-record-definition-guile def main?)
  (cons '(use-modules (srfi srfi-9))
        (_snow:srfi-9-generate-record-definition def main?)))

(define^ (_snow:srfi-9-generate-record-definition def main?)
  (cond ((not main?)
         '())
        ((_snow:record-def-parent def)
         (error "record extension is not supported"))
        (else
         `(
           (define-record-type ,(_snow:def-name def)
             (,(_snow:record-def-constructor def)
              ,@(map car (_snow:record-def-fields def)))
             ,(_snow:record-def-predicate def)
             ,@(_snow:record-def-fields def))
          ))))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Kawa.

(define^ (_snow:generate-include-kawa filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-kawa package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-kawa)
  '())

(define^ (_snow:generate-record-definition-kawa def main?)
  (_snow:srfi-9-generate-record-definition def main?))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Larceny.

(define^ (_snow:generate-include-larceny filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-larceny package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-larceny)
  (_snow:generate-record-macros-generic))

(define^ (_snow:generate-record-definition-larceny def main?)
  (_snow:generate-record-definition-generic def main?))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for MIT-Scheme.

(define^ (_snow:generate-include-mit filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-mit package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-mit)
  '())

(define^ (_snow:generate-record-definition-mit def main?)
  (cond ((not main?)
         '())
        ((_snow:record-def-parent def)
         (error "record extension is not supported"))
        (else
         `(
           (define-structure (,(_snow:def-name def) safe-accessors)
             ,@(map car (_snow:record-def-fields def)))
           ,@(map (lambda (field)
                    `(define ,(caddr field)
                       ,(_snow:sym "set-" (cadr field) "!")))
                  (_snow:record-def-fields def))
          ))))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for MzScheme.

(define^ (_snow:generate-include-mzscheme filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-mzscheme package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-mzscheme)
  '())

(define^ (_snow:generate-record-definition-mzscheme def main?)
  (cond ((not main?)
         '())
        ((_snow:record-def-parent def)
         (error "record extension is not supported"))
        (else
         `(
           (define-struct ,(_snow:def-name def)
             ,(map car (_snow:record-def-fields def)))
           ,@(map (lambda (field)
                    `(define ,(caddr field)
                       ,(_snow:sym "set-" (cadr field) "!")))
                  (_snow:record-def-fields def))
          ))))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for RScheme.

(define^ (_snow:generate-include-rscheme filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-rscheme package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-rscheme)
  '())

(define^ (_snow:generate-record-definition-rscheme def main?)
  (_snow:srfi-9-generate-record-definition def main?))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Scheme48.

(define^ (_snow:generate-include-scheme48 filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-scheme48 package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-scheme48)
  '())

(define^ (_snow:generate-record-definition-scheme48 def main?)
  (_snow:srfi-9-generate-record-definition def main?))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for SCM.

(define^ (_snow:generate-include-scm filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-scm package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

;; Because SCM lacks an exception handling hook, it fails the self test
;; if we use the SRFI 9 record facility.
;;
;;(define^ (_snow:generate-record-macros-scm)
;;  '())
;;
;;(define^ (_snow:generate-record-definition-scm def main?)
;;  (cons '(require 'srfi-9)
;;        (_snow:srfi-9-generate-record-definition def main?)))

(define^ (_snow:generate-record-macros-scm)
  (_snow:generate-record-macros-generic))

(define^ (_snow:generate-record-definition-scm def main?)
  (_snow:generate-record-definition-generic def main?))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Scsh.

(define^ (_snow:generate-include-scsh filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-scsh package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-scsh)
  '())

(define^ (_snow:generate-record-definition-scsh def main?)
  (_snow:srfi-9-generate-record-definition def main?))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Sisc.

(define^ (_snow:generate-include-sisc filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-sisc package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-sisc)
  '())

(define^ (_snow:generate-record-definition-sisc def main?)
  (cons '(import srfi-9)
        (_snow:srfi-9-generate-record-definition def main?)))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for Stalin.

(define^ (_snow:generate-include-stalin filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-stalin package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-stalin)
  (_snow:generate-record-macros-generic))

(define^ (_snow:generate-record-definition-stalin def main?)
  (_snow:generate-record-definition-generic def main?))

;;;----------------------------------------------------------------------------

;; Implementation of package* form for STklos.

(define^ (_snow:generate-include-stklos filename)
  (_snow:generate-include-generic filename))

(define^ (_snow:generate-package-header-stklos package-info all-req-packages)
  ;; TODO: better integration with module system
  (_snow:generate-package-header-generic package-info all-req-packages))

(define^ (_snow:generate-record-macros-stklos)
  '())

(define^ (_snow:generate-record-definition-stklos def main?)
  (cond ((not main?)
         '())
        ((_snow:record-def-parent def)
         (error "record extension is not supported"))
        (else
         `(
           (define-struct ,(_snow:def-name def)
             ,@(map car (_snow:record-def-fields def)))
           ,@(map (lambda (field)
                    `(define (,(caddr field) _1 _2)
                       (set! (,(cadr field) _1) _2)))
                  (_snow:record-def-fields def))
          ))))

;;;----------------------------------------------------------------------------

(define^ _snow:current-verspack (_snow:make-verspack 'dummy '(0 0 0)))
(define^ _snow:current-defined-keywords '())

(define^ (_snow:generate-package-header package-info all-req-packages)

  (let ((verspack (_snow:package-info-verspack package-info)))
    (set! _snow:current-verspack verspack)
    (set! _snow:current-defined-keywords '())
    (set! _snow:tests-enabled?
          (memq (_snow:verspack-name verspack) _snow:test-packages)))

  ((case _snow:target-host
     ((bigloo)   _snow:generate-package-header-bigloo)
     ((chez)     _snow:generate-package-header-chez)
     ((chicken)  _snow:generate-package-header-chicken)
     ((gambit)   _snow:generate-package-header-gambit)
     ((gauche)   _snow:generate-package-header-gauche)
     ((guile)    _snow:generate-package-header-guile)
     ((kawa)     _snow:generate-package-header-kawa)
     ((larceny)  _snow:generate-package-header-larceny)
     ((mit)      _snow:generate-package-header-mit)
     ((mzscheme) _snow:generate-package-header-mzscheme)
     ((rscheme)  _snow:generate-package-header-rscheme)
     ((scheme48) _snow:generate-package-header-scheme48)
     ((scm)      _snow:generate-package-header-scm)
     ((scsh)     _snow:generate-package-header-scsh)
     ((sisc)     _snow:generate-package-header-sisc)
     ((stalin)   _snow:generate-package-header-stalin)
     ((stklos)   _snow:generate-package-header-stklos)
     (else       _snow:generate-package-header-generic))
   package-info
   all-req-packages))

;;;----------------------------------------------------------------------------

(define^ (_snow:analyze-package* package-name-with-version body fail cont)
  (_snow:extract-verspack
   package-name-with-version
   #t
   fail
   (lambda (verspack)
     (_snow:parse-package-header
      (_snow:verspack-name verspack)
      #f
      #f ;; TODO: get the current filename
      (cons 'package*
            (cons package-name-with-version
                  body))
      (lambda (package-info)
        (_snow:get-req-packages
         (list package-info) ;; keep track of packages that have been read
         (list (_snow:package-info-verspack package-info))
         'all  ;; interface and implementation requirements
         #t    ;; deep requirements
         (lambda (all-packages all-req-packages)
           (cont package-info
                 all-req-packages))))))))

(define^ (_snow:expand-macro-package* package-name-with-version body)
  (_snow:trace-expansion
   (_snow:analyze-package*
    package-name-with-version
    body
    (lambda (msg)
      (_snow:syntax-err
       msg
       (list 'package-name-with-version= package-name-with-version)))
    (lambda (package-info all-req-packages)
      (_snow:make-begin
       (_snow:generate-package-header package-info all-req-packages))))))

;;;----------------------------------------------------------------------------

;; Expansion of (define* ...), (define-syntax ...) and (define-macro* ...)
;; when in package-file-body.

(define^ (_snow:expand-macro-define-var-or-macro*
          form-name
          kind
          allow-incomplete?
          pattern
          body)
  (_snow:trace-expansion
   (_snow:parse-define-var-or-macro*
    form-name
    kind
    allow-incomplete?
    _snow:current-verspack
    pattern
    body
    (lambda (def)
      (_snow:make-begin
       (_snow:generate-var-or-macro-definition def))))))

(define^ (_snow:expand-macro-define* pattern body)
  (_snow:macro-expand
   (_snow:expand-macro-define-var-or-macro*
    'define*
    'var
    #t
    pattern
    body)))

(define^ (_snow:expand-macro-define-macro* pattern body)
  (_snow:macro-expand
   (_snow:expand-macro-define-var-or-macro*
    'define-macro*
    'macro
    #f
    pattern
    body)))

;;;----------------------------------------------------------------------------

;; Expansion of (define-record* ...).

(define^ (_snow:expand-macro-define-record* name options-and-fields)
  (_snow:trace-expansion
   (_snow:parse-define-record*
    _snow:current-verspack
    name
    options-and-fields
    (lambda (def)
      (_snow:make-begin
       (_snow:generate-record-definition def #t))))))

;;;----------------------------------------------------------------------------

(define^ (_snow:simplify-path path)
  (let loop ((path path) (stack '()))
    (if (pair? path)
        (let ((x (car path)))
          (loop (cdr path)
                (cond ((string=? x ".")
                       stack)
                      ;; ((and (string=? x "..")
                      ;;       (pair? stack)
                      ;;       (not (string=? (car stack) x)))
                      ;;  (cdr stack))
                      (else
                       (cons x stack)))))
        (reverse stack))))

(define^ (_snow:path->relative-path-string path separator)
  (if (pair? path)
      (apply string-append
             (cons (car path)
                   (map (lambda (s) (string-append separator s))
                        (cdr path))))
      ""))

(define^ (_snow:path->unix-relative-path-string path)
  (_snow:path->relative-path-string path "/"))

(define^ (_snow:path->windows-relative-path-string path)
  (_snow:path->relative-path-string path "\\"))

(define^ (_snow:path->string path)
  (_snow:path->unix-relative-path-string path))

(define^ (_snow:parse-unix-relative-path-string str)
  (if (or (= 0 (string-length str))
          (char=? #\/ (string-ref str 0))
          (char=? #\/ (string-ref str (- (string-length str) 1))))
      #f
      (let loop1 ((i 0) (rev-path '()))
        (let loop2 ((j i))

          (define (add-part)
            (if (= i j)
                rev-path
                (cons (substring str i j) rev-path)))

          (if (< j (string-length str))
              (let ((c (string-ref str j)))
                (cond ((or (and (char>=? c #\a) (char<=? c #\z))
                           (and (char>=? c #\0) (char<=? c #\9))
                           (char=? c #\.)
                           (char=? c #\_)
                           (char=? c #\-)
                           (char=? c #\space))
                       (loop2 (+ j 1)))
                      ((char=? c #\/)
                       (loop1 (+ j 1) (add-part)))
                      (else
                       #f)))
              (_snow:path->string
               (_snow:simplify-path (reverse (add-part)))))))))

(define^ (_snow:expand-macro-load* filename)
  (_snow:generate-load filename))

(define^ (_snow:expand-macro-include* filename)
  (_snow:generate-include
   (_snow:parse-unix-relative-path-string filename)))

(define^ (_snow:expand-macro-test* body)
  (_snow:make-begin
   (if _snow:tests-enabled?
       body
       '())))

(define^ (_snow:expand-macro-expect* expr)
  `(snow-expect ',expr (lambda () ,expr)))

;;;----------------------------------------------------------------------------

;; Expansion of (include-program* ...) and (load-program* ...) when in
;; package body.

(define^ (_snow:expand-macro-include-program* package-name-or-filename options)
  (_snow:expand-macro-load-or-include-program*
   package-name-or-filename
   options
   #t))

(define^ (_snow:expand-macro-load-program* package-name-or-filename options)
  (_snow:expand-macro-load-or-include-program*
   package-name-or-filename
   options
   #f))

(define^ (_snow:expand-macro-load-or-include-program*
          package-name-or-filename
          options
          include?)

  (define (gen verspack-or-filename)
    (_snow:get-req-packages
     '()
     (list verspack-or-filename)
     'all ;; interface and implementation requirements
     #t ;; deep requirements
     (lambda (all-packages impl-req-packages)
       (let ((program-filename
              (if (string? verspack-or-filename)
                  verspack-or-filename
                  (_snow:package-info-filename
                   (_snow:lookup-package verspack-or-filename all-packages)))))
         (_snow:make-begin
          (append
           (_snow:generate-basic-runtime program-filename)
           (map (lambda (pi)
                  (let ((filename (_snow:package-info-filename pi)))
                    (if include?
                        `(include* ,filename)
                        `(load* ,(if (memq _snow:host
                                           _snow:loads-source-when-no-ext)
                                     (_snow:filename-strip-extension filename)
                                     filename)))))
                impl-req-packages)
           (list `(_snow:cleanup))))))))

  (_snow:trace-expansion-noisy
   (if (string? package-name-or-filename)
       (gen package-name-or-filename)
       (_snow:extract-verspack
        package-name-or-filename
        #f
        (lambda (msg)
          (_snow:syntax-err msg '(in main file)))
        (lambda (verspack)
          (gen verspack))))))

(define^ (_snow:generate-basic-runtime program-filename)
  (append
   `(
     (define _snow:program-filename ,program-filename)
       
     (define _snow:cleanup-handlers '())

     (define (_snow:cleanup-handler-push! thunk)
       (set! _snow:cleanup-handlers
             (cons thunk _snow:cleanup-handlers)))

     (define (_snow:cleanup)
       (let ((handlers _snow:cleanup-handlers))
         (set! _snow:cleanup-handlers '())
         (for-each (lambda (h) (h)) handlers)))
    )
   (if (>= _snow:debug 1)

       '(
         (define _snow:defined-names '())

         (define (_snow:add-definition package-name name)
           (let* ((names _snow:defined-names)
                  (x (assq name names)))
             (if x
                 (begin
                   (display
                    (string-append "*** SNOW WARNING -- "
                                   (symbol->string package-name)
                                   " and "
                                   (symbol->string (cdr x))
                                   " both define "
                                   (symbol->string name)))
                   (newline)))
             (set! _snow:defined-names (cons (cons name package-name) names))))
        )

       '())))

;;;----------------------------------------------------------------------------

;; Define user visible macros of the Scheme Now! framework.

(define-macro (package* package-name-with-version . body)
  `(_snow:eval-in-macro-environment
    (_snow:expand-macro-package*
     ',package-name-with-version
     ',body)))

(define-macro (load-program* package-name-or-filename . options)
  `(_snow:eval-in-macro-environment
    (_snow:expand-macro-load-program*
     ',package-name-or-filename
     ',options)))

(define-macro (include-program* package-name-or-filename . options)
  `(_snow:eval-in-macro-environment
    (_snow:expand-macro-include-program*
     ',package-name-or-filename
     ',options)))

(define-macro (load* filename)
  `(_snow:eval-in-macro-environment
    (_snow:expand-macro-load* ',filename)))

(define-macro (include* filename)
  `(_snow:eval-in-macro-environment
    (_snow:expand-macro-include* ',filename)))

(define-macro (include*-cd filename)
  `(_snow:eval-in-macro-environment
    (_snow:expand-macro-include*-cd ',filename)))

;;;============================================================================
