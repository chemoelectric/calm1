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

(define-library (calm1-lib define-record-factory)

  (export define-record-factory)

  (import (scheme base))

  (begin

    (define-syntax define-record-factory
      (syntax-rules ()
        ((_ designation rule ...)
         (begin
           (define-record-type designation
             (original-constructor% fields)
             predicate
             (fields access))
           (begin
             (define-syntax record-rule
               (syntax-rules (constructor>
                              predicate>
                              getter> setter>)
                 ((_ constructor predicate access
                     (constructor> name proc))
                  (define name (proc constructor)))
                 ((_ constructor predicate access
                     (constructor> name))
                  (define name constructor))
                 ((_ constructor predicate access
                     (predicate> name proc))
                  (define name (proc predicate)))
                 ((_ constructor predicate access
                     (predicate> name))
                  (define name predicate))
                 ((_ constructor predicate access
                     (getter> i name proc))
                  (define name
                    (proc
                     (lambda (obj)
                       (vector-ref (access obj) (- i 1))))))
                 ((_ constructor predicate access
                     (getter> i name))
                  (define name
                    (lambda (obj)
                      (vector-ref (access obj) (- i 1)))))
                 ((_ constructor predicate access
                     (setter> i name proc))
                  (define name
                    (proc
                     (lambda (obj value)
                       (vector-set! (access obj) (- i 1) value)))))
                 ((_ constructor predicate access
                     (setter> i name))
                  (define name
                    (lambda (obj value)
                      (vector-set! (access obj) (- i 1) value))))))
             (record-rule
              (lambda fields
                (original-constructor% (list->vector fields)))
              predicate access rule)
             ...)))))

    ))

;;;---------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; coding: utf-8
;;; end:
