;;;; SPDX-FileCopyrightText: 2005 - 2013 Per Bothner <per@bothner.com>
;;;; SPDX-FileCopyrightText: 2005 Alex Shinn
;;;; SPDX-FileCopyrightText: 2012 Álvaro Castro-Castilla
;;;; SPDX-FileCopyrightText: 2014 Mark H Weaver <mhw@netris.org>
;;;; SPDX-FileCopyrightText: 2014 - 2015 Taylan Kammer <taylan.kammer@gmail.com>
;;;;
;;;; SPDX-License-Identifier: MIT


;;; The data type

(define-record-type <test-runner>
  (make-test-runner) test-runner?

  (result-alist test-result-alist test-result-alist!)

  (pass-count test-runner-pass-count test-runner-pass-count!)
  (fail-count test-runner-fail-count test-runner-fail-count!)
  (xpass-count test-runner-xpass-count test-runner-xpass-count!)
  (xfail-count test-runner-xfail-count test-runner-xfail-count!)
  (skip-count test-runner-skip-count test-runner-skip-count!)
  (total-count %test-runner-total-count %test-runner-total-count!)

  ;; Stack (list) of (count-at-start . expected-count):
  (count-list %test-runner-count-list %test-runner-count-list!)

  ;; Normally #f, except when in a test-apply.
  (run-list %test-runner-run-list %test-runner-run-list!)

  (skip-list %test-runner-skip-list %test-runner-skip-list!)
  (fail-list %test-runner-fail-list %test-runner-fail-list!)

  (skip-save %test-runner-skip-save %test-runner-skip-save!)
  (fail-save %test-runner-fail-save %test-runner-fail-save!)

  (group-stack test-runner-group-stack test-runner-group-stack!)

  ;; Note: on-test-begin and on-test-end are unrelated to the test-begin and
  ;; test-end forms in the execution library.  They're called at the
  ;; beginning/end of each individual test, whereas the test-begin and test-end
  ;; forms demarcate test groups.

  (on-group-begin test-runner-on-group-begin test-runner-on-group-begin!)
  (on-test-begin test-runner-on-test-begin test-runner-on-test-begin!)
  (on-test-end test-runner-on-test-end test-runner-on-test-end!)
  (on-group-end test-runner-on-group-end test-runner-on-group-end!)
  (on-final test-runner-on-final test-runner-on-final!)
  (on-bad-count test-runner-on-bad-count test-runner-on-bad-count!)
  (on-bad-end-name test-runner-on-bad-end-name test-runner-on-bad-end-name!)

  (on-bad-error-type %test-runner-on-bad-error-type
                     %test-runner-on-bad-error-type!)

  (aux-value test-runner-aux-value test-runner-aux-value!)

  (log-file %test-runner-log-file %test-runner-log-file!)
  (log-port %test-runner-log-port %test-runner-log-port!))

(define (test-runner-group-path runner)
  (reverse (test-runner-group-stack runner)))

(define (test-runner-reset runner)
  (test-result-alist! runner '())
  (test-runner-pass-count! runner 0)
  (test-runner-fail-count! runner 0)
  (test-runner-xpass-count! runner 0)
  (test-runner-xfail-count! runner 0)
  (test-runner-skip-count! runner 0)
  (%test-runner-total-count! runner 0)
  (%test-runner-count-list! runner '())
  (%test-runner-run-list! runner #f)
  (%test-runner-skip-list! runner '())
  (%test-runner-fail-list! runner '())
  (%test-runner-skip-save! runner '())
  (%test-runner-fail-save! runner '())
  (test-runner-group-stack! runner '()))

(define (test-runner-null)
  (define (test-null-callback . args) #f)
  (let ((runner (make-test-runner)))
    (test-runner-reset runner)
    (test-runner-on-group-begin! runner test-null-callback)
    (test-runner-on-group-end! runner test-null-callback)
    (test-runner-on-final! runner test-null-callback)
    (test-runner-on-test-begin! runner test-null-callback)
    (test-runner-on-test-end! runner test-null-callback)
    (test-runner-on-bad-count! runner test-null-callback)
    (test-runner-on-bad-end-name! runner test-null-callback)
    (%test-runner-on-bad-error-type! runner test-null-callback)
    (%test-runner-log-file! runner #f)
    (%test-runner-log-port! runner #f)
    runner))


;;; State

(define test-result-ref
  (case-lambda
    ((runner key)
     (test-result-ref runner key #f))
    ((runner key default)
     (let ((entry (assq key (test-result-alist runner))))
       (if entry (cdr entry) default)))))

(define (test-result-set! runner key value)
  (let* ((alist (test-result-alist runner))
         (entry (assq key alist)))
    (if entry
        (set-cdr! entry value)
        (test-result-alist! runner (cons (cons key value) alist)))))

(define (test-result-remove runner key)
  (test-result-alist! runner (remove (lambda (entry)
                                       (eq? key (car entry)))
                                     (test-result-alist runner))))

(define (test-result-clear runner)
  (test-result-alist! runner '()))

(define (test-runner-test-name runner)
  (or (test-result-ref runner 'name) ""))

(define test-result-kind
  (case-lambda
    (() (test-result-kind (test-runner-get)))
    ((runner) (test-result-ref runner 'result-kind))))

(define test-passed?
  (case-lambda
    (() (test-passed? (test-runner-get)))
    ((runner) (memq (test-result-kind runner) '(pass xpass)))))


;;; Factory and current instance

(define test-runner-factory (make-parameter #f))

(define (test-runner-create) ((test-runner-factory)))

(define test-runner-current (make-parameter #f))

(define (test-runner-get)
  (or (test-runner-current)
      (error "test-runner not initialized - test-begin missing?")))

;;; test-runner.scm ends here
