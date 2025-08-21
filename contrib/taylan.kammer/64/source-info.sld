;;; SPDX-FileCopyrightText: 2015 Taylan Kammer <taylan.kammer@gmail.com>
;;;
;;; SPDX-License-Identifier: MIT

(define-library (srfi 64 source-info)
  (import
   (rnrs syntax-case (6))
   (scheme base)
   (srfi 64 test-runner))
  (cond-expand
   (guile
    (import (only (guile) assq-ref syntax-source))))
  (export source-info set-source-info!)
  (include "source-info.body.scm"))
