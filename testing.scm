;; Copyright (c) 2005, Per Bothner
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(cond-expand
 (kawa
  (define-syntax source-file
    (lambda (x)
      (syntax-case x ()
		   ((_ form)
		    (let ((form (syntax-object->datum (syntax (form)))))
		      (if (instance? form <gnu.lists.PairWithPosition>)
			  (list (quote quote)
				(datum->syntax-object form (gnu.lists.PairWithPosition:getFile form)))
			  #f))))))
  (define-syntax source-line
    (lambda (x)
      (syntax-case x ()
		   ((_ form)
		    (let ((form (syntax-object->datum (syntax (form)))))
		      (if (instance? form <gnu.lists.PairWithPosition>)
			  (list (quote quote)
				(datum->syntax-object form (gnu.lists.PairWithPosition:getLine form)))
			  #f))))))
  (define-syntax test-source-location-cons%
    (syntax-rules ()
      ((test-source-location-cons% form-to-use cdr)
       (cons (cons 'source-file (source-file  form-to-use))
	     (cons (cons 'source-line (source-line  form-to-use))
		   cdr))))))
 (else
  (define-syntax test-source-location-cons%
    (syntax-rules ()
      ((test-source-location-cons% form-to-use cdr)
       cdr)))))

(cond-expand
 (srfi-9
  (define-record-type test-runner
    (test-runner-alloc)
    test-runner?
    (pass-count test-runner-pass-count test-runner-pass-count!)
    (fail-count test-runner-fail-count test-runner-fail-count!)
    (xpass-count test-runner-xpass-count test-runner-xpass-count!)
    (xfail-count test-runner-xfail-count test-runner-xfail-count!)
    (skip-count test-runner-skip-count test-runner-skip-count!)
    (skip-list test-runner-skip-list test-runner-skip-list!)
    (fail-list test-runner-fail-list test-runner-fail-list!)
    (run-list test-runner-run-list test-runner-run-list!)
    (skip-save test-runner-skip-save test-runner-skip-save!)
    (fail-save test-runner-fail-save test-runner-fail-save!)
    (group-path test-runner-group-path test-runner-group-path!)
    (on-test test-runner-on-test test-runner-on-test!)
    (on-final test-runner-on-final test-runner-on-final!)
    (aux-value test-runner-aux-value test-runner-aux-value!)
    (test-name test-runner-test-name test-runner-test-name!)
))
 (else
  (define test-runner-cookie% (list "test-runner"))
  (define (test-runner? obj)
    (and (vector? obj)
	 (> (vector-length obj) 1)
	 (eq (vector-ref obj 0) test-runner-cookie%)))
  (define (test-runner-alloc)
    (let ((runner (make-vector 16)))
      (vector-set! runner 0 test-runner-cookie%)
      runner))
  (define-syntax test-%def-fld
    (syntax-rules ()
      ((test-%def-fld getter setter index)
       (begin
	 (define (getter runner) (vector-ref runner index))
	 (define (setter runner value) (vector-set! runner index value))))))
  (test-%def-fld test-runner-pass-count test-runner-pass-count! 1)
  (test-%def-fld test-runner-fail-count test-runner-fail-count! 2)
  (test-%def-fld test-runner-xpass-count test-runner-xpass-count! 3)
  (test-%def-fld test-runner-xfail-count test-runner-xfail-count! 4)
  (test-%def-fld test-runner-skip-count test-runner-skip-count! 5)
  (test-%def-fld test-runner-skip-list test-runner-skip-list! 6)
  (test-%def-fld test-runner-fail-list test-runner-fail-list! 7)
  (test-%def-fld test-runner-run-list test-runner-run-list! 8)
  (test-%def-fld test-runner-skip-save test-runner-skip-save! 9)
  (test-%def-fld test-runner-fail-save test-runner-fail-save! 10)
  (test-%def-fld test-runner-group-path test-runner-group-path! 11)
  (test-%def-fld test-runner-on-test test-runner-on-test! 12)
  (test-%def-fld test-runner-on-final test-runner-on-final! 13)
  (test-%def-fld test-runner-aux-value test-runner-aux-value! 14)
  (test-%def-fld test-runner-test-name test-runner-test-name! 15)
 ))

(define (test-runner-reset runner)
    (test-runner-pass-count! runner 0)
    (test-runner-fail-count! runner 0)
    (test-runner-xpass-count! runner 0)
    (test-runner-xfail-count! runner 0)
    (test-runner-skip-count! runner 0)
    (test-runner-run-list! runner #t)
    (test-runner-skip-list! runner '())
    (test-runner-fail-list! runner '())
    (test-runner-skip-save! runner '())
    (test-runner-fail-save! runner '())
    (test-runner-group-path! runner '())
    (test-runner-test-name! runner ""))

(define (test-runner-null)
  (let ((runner (test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-final! runner (lambda (runner) #f))
    (test-runner-on-test! runner (lambda (runner alist) #f))
    runner))

(define (test-runner-simple)
  (let ((runner (test-runner-alloc)))
    (test-runner-reset runner)
    (test-runner-on-final! runner test-on-final-simple)
    (test-runner-on-test! runner test-on-test-simple)
    runner))

(cond-expand
 (srfi-39
  (define test-runner-current (make-parameter #f))
  (define test-runner-factory (make-parameter test-runner-simple)))
 (else
  (define test-runner-current% #f)
  (define-syntax test-runner-current
    (syntax-rules ()
      ((test-runner-current)
       test-runner-current%)
      ((test-runner-current runner)
       (set! test-runner-current% runner))))
  (define test-runner-factory% test-runner-simple)
  (define-syntax test-runner-factory
    (syntax-rules ()
      ((test-runner-factory)
       test-runner-factory%)
      ((test-runner-factory runner)
       (set! test-runner-factory% runner))))))

(define (test-%specificier-matches spec runner)
  (spec runner))

(define (test-runner-create)
  ((test-runner-factory)))

(define (test-%any-specifier-matches list runner)
  (let loop ((l list))
    (cond ((null? l) #f)
	  ((test-%specificier-matches (car l) runner) #t)
	  (else (loop (cdr l))))))

;; Returns #f, #t, or 'expected-fail.
(define (test-%should-execute-test name runner)
  (test-runner-test-name! runner name)
  (let ((run (test-runner-run-list runner)))
    (and
     (or (eqv? run #t)
	 (test-%any-specifier-matches run runner))
     (cond ((test-%any-specifier-matches
	     (test-runner-skip-list runner)
	     runner)
	    #f)
	   ((test-%any-specifier-matches
	     (test-runner-fail-list runner)
	     runner)
	    'expected-fail)
	   (else #t)))))

(define (test-%begin suite-name)
  (if (not (test-runner-current))
      (test-runner-current (test-runner-create)))
  (let ((runner (test-runner-current)))
    (test-runner-skip-save! runner
			       (cons (test-runner-skip-list runner)
				     (test-runner-skip-save runner)))
    (test-runner-fail-save! runner
			       (cons (test-runner-fail-list runner)
				     (test-runner-fail-save runner)))
    (test-runner-group-path! runner (cons suite-name
					(test-runner-group-path runner)))))

(define-syntax test-begin
  (syntax-rules ()
    ((test-begin suite-name)
     (test-%begin suite-name))))

(define (test-%report-display value)
  (display value))

(define (test-%report-newline)
  (newline))

(define-syntax test-%report1
  (syntax-rules ()
    ((test-%report1 getter prefix)
     (let ((value (getter (test-runner-current))))
       (if (> value 0)
	   (begin
	     (test-%report-display prefix)
	     (test-%report-display value)
	     (test-%report-newline)))))))

(define (test-on-final-simple runner)
  (test-%report1 test-runner-pass-count	 "# of expected passes      ")
  (test-%report1 test-runner-xfail-count "# of expected failures    ")
  (test-%report1 test-runner-xpass-count "# of unexpected successes ")
  (test-%report1 test-runner-fail-count  "# of unexpected failures  ")
  (test-%report1 test-runner-skip-count  "# of skipped tests        ")
  (display "Done!")
  (newline))

;; FIXME doesn't check count
;; FIXME doesn't check that suite-name matches
;; nor does try to recover from a mismatch (by extra pops).
(define (test-end% suite-name count)
  (let ((r (test-runner-current)))
    (test-runner-group-path! r (cdr (test-runner-group-path r)))
    (test-runner-skip-list! r (car (test-runner-skip-save r)))
    (test-runner-skip-save! r (cdr (test-runner-skip-save r)))
    (test-runner-fail-list! r (car (test-runner-fail-save r)))
    (test-runner-fail-save! r (cdr (test-runner-fail-save r)))
    (if (null? (test-runner-group-path r))
	((test-runner-on-final r) r))))

(define-syntax test-end
  (syntax-rules ()
    ((test-end)
     (test-end% #f #f))
    ((test-end suite-name)
     (test-end% suite-name #f))
    ((test-end suite-name count)
     (test-end% suite-name count))))

(define-syntax test-group
  (syntax-rules ()
    ((test-group suite-name . body)
     (if (test-%should-execute-test suite-name (test-runner-current))
	 (dynamic-wind
	     (lambda () (test-begin suite-name))
	     (lambda () . body)
	     (lambda () (test-end  suite-name)))))))

(define-syntax test-group-with-cleanup
  (syntax-rules ()
    ((test-group-with-cleanup suite-name form cleanup-form)
     (test-group suite-name
		    (dynamic-wind
			(lambda () #f)
			(lambda () form)
			(lambda () cleanup-form))))
    ((test-group-with-cleanup suite-name cleanup-form)
     (test-group-with-cleanup suite-name #f cleanup-form))
    ((test-group-with-cleanup suite-name form1 form2 form3 . rest)
     (test-group-with-cleanup suite-name (begin form1 form2) form3 . rest))))

(define (test-on-test-simple runner alist)
  (display "test ")
  (display alist)
  (newline))

(define (test-%report-result alist)
  (let ((result-kind (cdr (assq 'result-kind alist)))
	(r (test-runner-current)))
    (case result-kind
      ((pass)
       (test-runner-pass-count! r (+ 1 (test-runner-pass-count r))))
      ((fail)
       (test-runner-fail-count!	r (+ 1 (test-runner-fail-count r))))
      ((xpass)
       (test-runner-xpass-count! r (+ 1 (test-runner-xpass-count r))))
      ((xfail)
       (test-runner-xfail-count! r (+ 1 (test-runner-xfail-count r))))
      (else
       (test-runner-skip-count! r (+ 1 (test-runner-skip-count r)))))
    ((test-runner-on-test r) r alist)))

(cond-expand
 (kawa
  (define-syntax test-evaluate-with-catch%
    (syntax-rules ()
      ((test-evaluate-with-catch test-expression)
       (try-catch test-expression
		  (ex <java.lang.Throwable>
		      #f))))))
 (srfi-34
  (define-syntax test-evaluate-with-catch%
    (syntax-rules ()
      ((test-evaluate-with-catch test-expression)
       (guard (cond (else #f)) test-expression)))))
 (else
  (define-syntax test-evaluate-with-catch%
    (syntax-rules ()
      ((test-evaluate-with-catch test-expression)
       test-expression)))))

(define-syntax test-raw-assert
  (syntax-rules ()
    ((test-raw-assert test-name alist test-expression)
     (let* ((should (test-%should-execute-test test-name (test-runner-current)))
	      (result
	       (if should
		   (let ((result (test-evaluate-with-catch% test-expression)))
		     (if (eq? should 'expected-fail)
			 (if result 'xpass 'xfail)
			 (if result 'pass 'fail)))
		   'skip)))
       (test-%report-result (cons (cons 'result-kind result) alist))))))

(define-syntax test-assert
  (syntax-rules ()
    ((test-assert tname test-expression)
     (test-raw-assert tname
		      (test-source-location-cons% tname
						  '((test-name . tname)))
		      test-expression))
    ((test-assert test-expression)
     (test-raw-assert "" '() test-expression))))

(define-syntax test-equal
  (syntax-rules ()
    ((test-equal name expr expected)
     (test-assert name (equal? expr expected)))
    ((test-equal expr expected)
     (test-assert (equal? expr expected)))))

(define-syntax test-eqv
  (syntax-rules ()
    ((test-eql name expr expected)
     (test-assert name (eqv? expr expected)))
    ((test-eql expr expected)
     (test-assert (eqv? expr expected)))))

(define-syntax test-eq
  (syntax-rules ()
    ((test-eq name expr expected)
     (test-assert name (eq? expr expected)))
    ((test-eq expr expected)
     (test-assert (eq? expr expected)))))

(cond-expand
 (srfi-35
  (define-syntax test-error%
    (syntax-rules ()
      ((test-error% #t expr)
       (guard (else #t))
	      (begin expr #f))
      ((test-error% etype expr)
       (guard (cond ((condition-has-type? cond etype) #t) (else #f))
	      (begin expr #f))))))
 (srfi-34
  (define-syntax test-error%
    (syntax-rules ()
      ((test-error% etype expr)
       (guard (else #t))
	      (begin expr #f)))))
 (kawa
  (define-syntax test-error%
    (syntax-rules ()
      ((test-error% etype expr)
       (try-catch (begin expr #f)
		  (ex <java.lang.Throwable> #t))))))
 (else
  (define-syntax test-error%
    (syntax-rules ()
      ((test-error% etype expr)
       #t)))))

(define-syntax test-error
  (syntax-rules ()
    ((test-error name etype expr)
     (test-assert name (test-error% etype expr)))
    ((test-error etype expr)
     (test-assert (test-error% etype expr)))
    ((test-error expr)
     (test-assert (test-error% #t expr)))))

(define-syntax test-with-runner
  (syntax-rules ()
    ((test-with-runner runner form ...)
     (let ((saved-runner (test-runner-current)))
       (dynamic-wind
           (lambda () (test-runner-current runner))
           (lambda () form ...)
           (lambda () (test-runner-current saved-runner)))))))

;;; Predicates

;; Coerce a form to a prediacte function:
(define-syntax test-make-predicate%
  (syntax-rules ()
    ((test-make-predicate% form) form))) ;; FIXME

(define (test-match-nth% n count)
  (let ((i 0))
    (lambda (runner)
      (set! i (+ i 1))
      (and (>= i n) (< i (+ n count))))))

(define-syntax test-match-nth
  (syntax-rules ()
    ((test-match-nth n)
     (test-match-nth n 1))
    ((test-match-nth n count)
     (test-match-nth% n count))))

(define (test-match-all% predlist)
  (lambda (runner)
    (let loop ((l pred-list))
      (if (null? l) #t
	  (and ((car l) runner)
	       (loop (cdr l)))))))
  
(define-syntax test-match-all
  (syntax-rules ()
    ((test-match-all pred)
     (test-make-predicate% pred))
    ((test-match-all pred ...)
     (test-match-all% (list (test-make-predicate% pred) ...)))))

(define (test-match-any% predlist)
  (lambda (runner)
    (let loop ((l pred-list))
      (if (null? l) #f
	  (any ((car l) runner)
	      (loop (cdr l)))))))
  
(define-syntax test-match-any
  (syntax-rules ()
    ((test-match-any pred ...)
     (test-match-any% (list (test-make-predicate% pred) ...)))))

(define-syntax test-skip
  (syntax-rules ()
    ((test-skip . pred)
     (let ((runner (test-runner-current)))
       (test-runner-skip-list! runner
				  (cons (test-match-all . pred)
					(test-runner-skip-list runner)))))))

(define-syntax test-expect-fail
  (syntax-rules ()
    ((test-expect-fail . pred)
     (let ((runner (test-runner-current)))
       (test-runner-fail-list! runner
				  (cons (test-match-all . pred)
					(test-runner-fail-list runner)))))))

(define (test-match-named name)
  (lambda (runner)
    (equal? name (test-runnner-test-name runner))))
