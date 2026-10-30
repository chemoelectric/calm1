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

;;;---------------------------------------------------------------------

(define-library (calm1-lib preprocessor-text)

  (import (scheme base)
          (scheme write))
  (import (calm1-lib utf32-string))

  (export preprocessor-text
          set-preprocessor-text!
          shorten!
          reinsert!
          preprocessor-serialize
          preprocessor-stringize)

  (begin

    (define (utf32-string? obj)
      ;;
      ;; FIXME: This will suffice unless we put a proper first-class
      ;; wrapper on utf32-string.
      ;;
      (bytevector? obj))

    (define (preprocessor-serialize obj)
      (let ((port (open-output-string)))
        (write obj port)
        (string->utf32-string (get-output-string port))))

    (define (preprocessor-stringize x)
      (cond ((utf32-string? x) x)
            ((string? x) (string->utf32-string x))
            ((symbol? x) (symbol->utf32-string x))
            ((number? x) (number->utf32-string x))
            ((char? x) (utf32-string (char->integer x)))
            (else (preprocessor-serialize x))))

    (define *preprocessor-text*
      (make-parameter (list '())))

    (define (set-preprocessor-text! u32)
      (unless (utf32-string? u32)
        (error "expected a utf32-string" u32))
      (set-car! (*preprocessor-text*) u32))

    (define (preprocessor-text)
      (car (*preprocessor-text*)))

    (define (shorten! n)
      (set-preprocessor-text!
       (utf32-string-copy (preprocessor-text) n)))

    (define (reinsert! str)
      (set-preprocessor-text!
       (utf32-string-append (preprocessor-stringize str)
                            (preprocessor-text))))

    ))

;;;---------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; coding: utf-8
;;; end:
