;;;============================================================================

;;; File: "compat-kawa.scm", Time-stamp: <2007-04-04 17:23:54 feeley>

;;; Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(define (directory-files (dir :: filepath))
  (let ((files ((java.io.File (dir:toFile)):list)))
    (if (eq? files (gnu.expr.QuoteExp:nullExp:getValue))
        (error "directory does not exist or is unreadable:" dir)
        (gnu.lists.LList:makeList files 0))))

;;;============================================================================
