;;; SPDX-FileCopyrightText: 2015 Taylan Kammer <taylan.kammer@gmail.com>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 64 test-runner)
  (import
   (scheme base)
   (scheme case-lambda)
   (srfi 1))
  (include-library-declarations "test-runner.exports.sld")
  (include "test-runner.body.scm"))
