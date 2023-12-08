;;;; SPDX-FileCopyrightText: 2015 Taylan Kammer <taylan.kammer@gmail.com>
;;;;
;;;; SPDX-License-Identifier: MIT

;;; In some systems, a macro use like (source-info ...), that resides in a
;;; syntax-rules macro body, first gets inserted into the place where the
;;; syntax-rules macro was used, and then the transformer of 'source-info' is
;;; called with a syntax object that has the source location information of that
;;; position.  That works fine when the user calls e.g. (test-assert ...), whose
;;; body contains (source-info ...); the user gets the source location of the
;;; (test-assert ...) call as intended, and not the source location of the real
;;; (source-info ...) call.

;;; In other systems, *first* the (source-info ...) is processed to get its real
;;; position, which is within the body of a syntax-rules macro like test-assert,
;;; so no matter where the user calls (test-assert ...), they get source
;;; location information of where we defined test-assert with the call to
;;; (source-info ...) in its body.  That's arguably more correct behavior,
;;; although in this case it makes our job a bit harder; we need to get the
;;; source location from an argument to 'source-info' instead.

(define (canonical-syntax form arg)
  (cond-expand
   (kawa arg)
   (guile-2 form)
   (else #f)))

(cond-expand
 ((or kawa guile-2)
  (define-syntax source-info
    (lambda (stx)
      (syntax-case stx ()
        ((_ <x>)
         (let* ((stx (canonical-syntax stx (syntax <x>)))
                (file (syntax-source-file stx))
                (line (syntax-source-line stx)))
           (quasisyntax
            (cons (unsyntax file) (unsyntax line)))))))))
 (else
  (define-syntax source-info
    (syntax-rules ()
      ((_ <x>)
       #f)))))

(define (syntax-source-file stx)
  (cond-expand
   (kawa
    (syntax-source stx))
   (guile-2
    (let ((source (syntax-source stx)))
      (and source (assq-ref source 'filename))))
   (else
    #f)))

(define (syntax-source-line stx)
  (cond-expand
   (kawa
    (syntax-line stx))
   (guile-2
    (let ((source (syntax-source stx)))
      (and source (assq-ref source 'line))))
   (else
    #f)))

(define (set-source-info! runner source-info)
  (when source-info
    (test-result-set! runner 'source-file (car source-info))
    (test-result-set! runner 'source-line (cdr source-info))))

;;; source-info.body.scm ends here
