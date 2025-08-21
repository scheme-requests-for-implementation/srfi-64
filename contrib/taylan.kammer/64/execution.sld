;;; SPDX-FileCopyrightText: 2015 Taylan Kammer <taylan.kammer@gmail.com>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 64 execution)
  (import
   (scheme base)
   (scheme case-lambda)
   (scheme complex)
   (scheme eval)
   (scheme process-context)
   (scheme read)
   (srfi 1)
   (srfi 35)
   (srfi 48)
   (srfi 64 source-info)
   (srfi 64 test-runner)
   (srfi 64 test-runner-simple))
  (cond-expand
   (guile
    (import (only (guile) current-module))))
  (include-library-declarations "execution.exports.sld")
  (include "execution.body.scm"))
