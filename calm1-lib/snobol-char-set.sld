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

(define-library (calm1-lib snobol-char-set)

  (import (scheme base)
          (calm1-lib utf32-string))
  (cond-expand
    ((library (scheme charset)) (import (scheme charset)))
    ((library (srfi 14)) (import (srfi 14)))
    (loko (import (srfi :14 char-sets)))
    (else (import (srfi srfi-14))))

  (export p:span-char-set
          p:break-char-set
          p:any-char-set
          p:notany-char-set)

  (begin

    (define (->utf32 s)
      (cond ((string? s) (string->utf32-string s))
            (else s)))

    ;; Optimized tail-recursive forward index loops querying the
    ;; native char-set directly.
    (define (span-cs-forward str idx cs len)
      (let ((str (->utf32 str)))
        (if (and (< idx len)
                 (char-set-contains?
                  cs (utf32-string-ref str idx)))
          (span-cs-forward str (+ idx 1) cs len)
          idx)))

    (define (break-cs-forward str idx cs len)
      (let ((str (->utf32 str)))
        (if (and (< idx len)
                 (not (char-set-contains?
                       cs (utf32-string-ref str idx))))
          (break-cs-forward str (+ idx 1) cs len)
          idx)))

    ;;------------------------------------------------------------------
    ;;
    ;; SPITBOL atomic compliant character-set combinators.
    ;;

    ;; SNOBOL SPAN: Consumes the longest contiguous block matching the
    ;; char-set object.
    (define (p:span-char-set cs)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (let* ((len (utf32-string-length str))
                 (max-end (span-cs-forward str idx cs len)))
            (if (< idx max-end)
              (succeed max-end env fail)
              (fail))))))

    ;; SNOBOL BREAK: Consumes text up to, but excluding, the char-set
    ;; boundary.
    (define (p:break-char-set cs)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (let* ((len (utf32-string-length str))
                 (max-end (break-cs-forward str idx cs len)))
            (if (< idx max-end)
              (succeed max-end env fail)
              (fail))))))

    ;; SNOBOL ANY: Matches a single character contained within the
    ;; target char-set.
    (define (p:any-char-set cs)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (if (and (< idx (utf32-string-length str))
                   (char-set-contains?
                    cs (utf32-string-ref str idx)))
            (succeed (+ idx 1) env fail)
            (fail)))))

    ;; SNOBOL NOTANY: Matches a character completely absent from the
    ;; target char-set.
    (define (p:notany-char-set cs)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (if (and (< idx (utf32-string-length str))
                   (not (char-set-contains?
                         cs (utf32-string-ref str idx))))
            (succeed (+ idx 1) env fail)
            (fail)))))

    ))

;;;---------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; coding: utf-8
;;; end:
