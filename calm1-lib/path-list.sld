;;; Copyright © 2026 Barry Schwartz
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

(define-library (calm1-lib path-list)

  (import (scheme base)
          (scheme file)
          (scheme process-context))
  (cond-expand
    ((library (srfi 1)) (import (srfi 1)))
    ((library (scheme list)) (import (scheme list)))
    (else (import (srfi srfi-1))))

  (export split-path-list
          search-path-list
          search-calm1-path-list)

  (begin

    (define (split-path-list str)
      ;;
      ;;   (split-path-list "::abc:bcd:.::cfg:")
      ;;
      ;; will return
      ;;
      ;;   ("." "." "abc" "bcd" "." "." "cfg" ".")
      ;;
      (let* ((lst (string->list str))
             (lst (if (and (pair? lst) (char=? (car lst) #\:))
                    (cons #\. lst)
                    lst)))
        (let loop ((inputs (reverse! lst))
                   (outputs '())
                   (start-new? #t))
          (if (null? inputs)
            (map! list->string outputs)
            (let ((ch (car inputs)))
              (if start-new?
                (if (char=? ch #\:)
                  (loop (cdr inputs) (cons (list #\.) outputs) #t)
                  (loop (cdr inputs) (cons (list ch) outputs) #f))
                (if (char=? ch #\:)
                  (loop (cdr inputs) outputs #t)
                  (let-values (((hd tl) (car+cdr outputs)))
                    (loop (cdr inputs) (cons (cons ch hd) tl)
                          #f)))))))))

    (define (filename-is-absolute? filename)
      (and (not (zero? (string-length filename)))
           (char=? (string-ref filename 0) #\/)))

    (define (search-path-list path-list filename)
      (if (filename-is-absolute? filename)
        (and (file-exists? filename) filename)
        (let ((path-list
               (cond
                 ((string? path-list)
                  (split-path-list path-list))
                 ((proper-list? path-list)
                  path-list)
                 (else
                  (error "expected a path list" path-list)))))
          (let loop ((p path-list))
            (and (pair? p)
                 (let* ((entry (car p))
                        (entry (if (string=? entry "") "." entry))
                        (entry (string-append entry "/" filename)))
                   (if (file-exists? entry)
                     entry
                     (loop (cdr p)))))))))

    (define (search-calm1-path-list filename)
      (let* ((path-list (get-environment-variable "CALM1_PATH"))
             (path-list (if path-list path-list ".")))
        (search-path-list path-list filename)))

    ))
