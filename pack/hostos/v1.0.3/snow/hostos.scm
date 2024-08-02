;;;============================================================================

;;; File: "hostos.scm", Time-stamp: <2007-09-03 12:13:33 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; Provides procedures to access the host operating-system.

(package* hostos/v1.0.3
 (provide:

  (define* (snow-exit (status _)))
  (define (snow-command-line))
  (define (snow-getenv var . opts))
  (define (snow-shell-command command))
  (define (snow-read-shell-command command))

  (define (make-unbound-hostos-env-var-condition var))
  (define (unbound-hostos-env-var-condition? obj))
  (define (unbound-hostos-env-var-condition-var cnd)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "Host operating system access.")

 (keywords: os snow)

 (license: lgpl/v2.1)

 (require: filesys/v1)
 (require: homovector/v1)
 (require: genport/v1))

;;;============================================================================

;;; System dependencies.

(cond-expand

 (else

  (define (make-unbound-hostos-env-var-condition var)
    (make-snow-cond '(unbound-hostos-env-var-condition)
                    (vector var)))

  (define (unbound-hostos-env-var-condition? obj)
    (and (snow-cond? obj)
         (memq 'unbound-hostos-env-var-condition (snow-cond-type obj))))

  (define (unbound-hostos-env-var-condition-var cnd)
    (vector-ref (snow-cond-fields cnd) 0))))

(cond-expand

 (bigloo

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (let ((x (command-line)))
      (if (and (>= (length x) 4)
               (equal? (cadr x) "-s")
               (equal? (caddr x) "-eval"))
          (cons _snow:program-filename (cddddr x))
          x)))

  (define (snow-getenv var . opts)
    (or (getenv var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (system command)))

 (chez

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (cons _snow:program-filename
          (cdr (command-line))))

  (define (snow-getenv var . opts)
    (or (getenv var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (system command)))

 (chicken

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (let* ((x (argv))
           (n (length x)))
      (if (and (>= n 6)
               (equal? (list-ref x 1) "-R")
               (equal? (list-ref x 2) "syntax-case")
               (equal? (list-ref x 3) "-e")
               (equal? (list-ref x 5) "--"))
          (cons _snow:program-filename (cddddr (cddr x)))
          (if (and (>= n 4)
                   (equal? (list-ref x 1) "-e")
                   (equal? (list-ref x 3) "--"))
              (cons _snow:program-filename (cddddr x))
              x))))

  (define (snow-getenv var . opts)
    (or (getenv var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (system command)))

 (gambit

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (let* ((x (command-line))
           (n (length x)))
      (if (and (>= n 3)
               (equal? (list-ref x 1) "-e"))
          (if (and (>= n 5)
                   (equal? (list-ref x 3) "-e"))
              (cons _snow:program-filename (cdddr (cddr x)))
              (cons _snow:program-filename (cdddr x)))
          x)))

  (define (snow-getenv var . opts)
    (or (getenv var #f)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (shell-command command)))

 (gauche

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (cons _snow:program-filename *argv*))

  (define (snow-getenv var . opts)
    (or (sys-getenv var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (sys-system command)))

 (guile

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (cons _snow:program-filename (cdr (command-line))))

  (define (snow-getenv var . opts)
    (or (getenv var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (system command)))

 (kawa

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (cons _snow:program-filename (vector->list command-line-arguments)))

  (define (snow-getenv var . opts)
    (let ((val (invoke-static <java.lang.System> 'getenv var)))
      (if (string? val)
          val
          (if (null? opts)
              (snow-raise (make-unbound-hostos-env-var-condition var))
              (car opts)))))

  (define (snow-shell-command command)
    (system (list "/bin/sh" "-c" command))))

 (larceny

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (cons _snow:program-filename
          ;; Conspires w/ snow.in to pass arguments via Larceny command line.
          (cdr (member "-snow-args" (vector->list (command-line-arguments))))))

  (define (snow-getenv var . opts)
    (or (getenv var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (system command)))

 (mit

  (load-option 'synchronous-subprocess)

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (%exit status))

  (define (snow-command-line)
    (let loop ((i 1)
               (rev-args '()))
      (let* ((var (string-append "SNOW_SCRIPT_ARG" (number->string i)))
             (arg (snow-getenv var #f)))
        (if arg
            (loop (+ i 1)
                  (cons arg rev-args))
            (cons _snow:program-filename
                  (reverse rev-args))))))

  (define (snow-getenv var . opts)
    (or (get-environment-variable var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (run-shell-command command)))

 (mzscheme

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (cons _snow:program-filename (vector->list argv)))

  (define (snow-getenv var . opts)
    (or (getenv var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (system/exit-code command)))

 (scheme48

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (let loop ((i 1)
               (rev-args '()))
      (let* ((var (string-append "SNOW_SCRIPT_ARG" (number->string i)))
             (arg (snow-getenv var #f)))
        (if arg
            (loop (+ i 1)
                  (cons arg rev-args))
            (cons _snow:program-filename
                  (reverse rev-args))))))

  (define (snow-getenv var . opts)
    (or (lookup-environment-variable var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (system command)))

 (scm

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (let ((x *argv*))
      (if (and (>= (length x) 4)
               (equal? (cadr x) "-q")
               (equal? (caddr x) "-c"))
          (cons _snow:program-filename (cdr (cddddr x)))
          x)))

  (define (snow-getenv var . opts)
    (or (getenv var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (system command)))

 (scsh

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (let ((x (command-line)))
      (cons _snow:program-filename (cdr x))))

  (define (snow-getenv var . opts)
    (or (getenv var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    ;; The "unquote" in the following "run" form causes problems
    ;; (run (sh "-c" ,command))
    ;; so we use this instead:
    (wait (fork (lambda () (exec-path "sh" "-c" command))))))

 (sisc

  (import os)

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (_snow:exit-continuation status))

  (define (snow-command-line)
    (cons _snow:program-filename (cdr _snow:command-line-arguments)))

  (define _snow:environ
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

  (define (snow-getenv var . opts)
    (let ((x (assoc var _snow:environ)))
      (if x
          (cdr x)
          (if (null? opts)
              (snow-raise (make-unbound-hostos-env-var-condition var))
              (car opts)))))

  (define (snow-shell-command command)

    (define (read-char port)
      (let ((b (read-byte port)))
        (if (eof-object? b)
            b
            (integer->char b))))

    (with-failure-continuation
     (lambda (error-record error-k)
       127)
     (lambda ()
       (let* ((process
               (spawn-process "/bin/sh" (list "-c" command)))
              (port
               (get-process-stdout process)))
         (let loop ()
           (let ((c (read-char port)))
             (if (not (eof-object? c))
                 (begin
                   (write-char c)
                   (loop)))))
         (close-input-port port)
         (wait-for-process process))))))

 (stklos

  (define* (snow-exit (status 0))
    (snow-cleanup)
    (exit status))

  (define (snow-command-line)
    (cons _snow:program-filename (argv)))

  (define (snow-getenv var . opts)
    (or (getenv var)
        (if (null? opts)
            (snow-raise (make-unbound-hostos-env-var-condition var))
            (car opts))))

  (define (snow-shell-command command)
    (system command))))

;;;----------------------------------------------------------------------------

(define (snow-read-shell-command command)
  (let* ((filename
          (snow-make-temp-filename))
         (status
          (snow-shell-command (string-append command " > " filename))))
    (if (= status 0)
        (let ((out (genport-read-file filename)))
          (snow-delete-file filename)
          (snow-u8vector->ISO-8859-1-string out))
        (begin
          (snow-delete-file filename)
          (snow-error "shell command terminated abnormally")))))

;;;============================================================================
