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

(define-library (calm1-lib preprocessor-define-lookup)

  (export define-lookup)

  (import (scheme base))
  (cond-expand
    ((library (srfi 1)) (import (srfi 1)))
    ((library (scheme list)) (import (scheme list)))
    (else (import (srfi srfi-1))))

  (begin

    (define-syntax define-lookup
      (syntax-rules ()
        ((¶ *things*
            getter setter!
            pusher! popper!
            localizer)
         (begin
           (define *things* (make-parameter (list (list '()))))
           (define (getter name)
             (let ((p (*things*)))
               (let ((association (assoc name (caar p))))
                 (if association
                   (cdr association)
                   #f))))
           (define (setter! name value)
             (popper! name)
             (pusher! name value))
           (define (pusher! name value)
             (let ((p (*things*)))
               ;;
               ;; This is not a “true” association list, but rather a
               ;; stack.
               ;;
               (let ((lst (caar p))
                     (association (cons name value)))
                 (set-car! (car p) (cons association lst)))))
           (define (popper! name)
             ;;
             ;; Pop the first instance of name.
             ;;
             (let-values (((a b) (break! (lambda (pair)
                                           (equal? name (car pair)))
                                         (caar (*things*)))))
               (let ((b (if (pair? b) (cdr b) b)))
                 (set-car! (car (*things*)) (append a b)))))
           (define-syntax localizer
             (syntax-rules --- ()
               ((ß body ---)
                (parameterize
                    ((*things*
                      (list (map (lambda (p) (cons (car p) (cdr p)))
                                 (caar (*things*))))))
                  (begin
                    body ---
                    (unspecified-value)
                    )))))))))

    ))

;;;---------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; coding: utf-8
;;; end:
