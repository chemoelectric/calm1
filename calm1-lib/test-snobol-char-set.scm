;;; Copyright (c) 2026 Barry Schwartz
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(import (scheme base)
        (scheme write)
        (scheme process-context)
        (calm1-lib utf32-string)
        (calm1-lib snobol-match)
        (calm1-lib snobol-char-set))
(cond-expand
    ((library (scheme charset)) (import (scheme charset)))
    ((library (srfi 14)) (import (srfi 14)))
    (loko (import (srfi :14 char-sets)))
    (else (import (srfi srfi-14))))

(define (->utf32 s)
  (cond ((string? s) (string->utf32-string s))
        (else s)))

(define (assert-equal? label expected actual)
  (if (not (equal? expected actual))
      (begin
        (display "FAIL: ") (display label) (newline)
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write actual) (newline)
        (exit 1))))

(define (run-charset-tests)
  ;; Test 1: p:span-char-set continuous digit scanning
  (let ((span-digit-pat (p:seq (p:span-char-set char-set:digit) (p:lit "kg"))))
    (assert-equal? "p:span-char-set matches continuous integer blocks natively"
                   6
                   (car (snobol-match span-digit-pat "1024kg"))))

  ;; Test 2: p:break-char-set tracking up to a whitespace boundary
  (let ((break-space-pat (p:seq (p:assign-local (p:break-char-set char-set:whitespace) 'word)
                                (p:span-char-set char-set:whitespace))))
    (assert-equal? "p:break-char-set identifies text blocks up to a charset delimiter"
                   (->utf32 "symbol-name")
                   (let ((res (snobol-match break-space-pat "symbol-name \t")))
                     (if res (cdr (assoc 'word (cdr res))) #f))))

  (begin #t))

(run-charset-tests)
