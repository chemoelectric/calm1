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

(define-library (calm1-lib utf32-string)

  (export current-utf32-endianness
          utf32-string-length
          utf32-string-ref
          utf32-string-set!
          make-utf32-string
          utf32-string-copy
          utf32-string-copy!
          utf32-string=?
          utf32-string-index
          utf32-string-index-right
          utf32-string-map
          utf32-string-fold
          utf32-string-fold-right
          utf32-string-append
          utf32-string-concatenate
          list->utf32-string
          utf32-string->list
          utf32-string
          utf32-string->string
          string->utf32-string
          utf32-string->symbol
          symbol->utf32-string
          utf32-string->number
          number->utf32-string)

  (import (scheme base))
  (import (scheme case-lambda))
  (cond-expand
    ((library (srfi 1))
     (import (srfi 1)))
    ((library (scheme list))
     (import (scheme list)))
    (else (import (srfi srfi-1))))
  (cond-expand
    ((library (scheme bytevector))
     (import (except (scheme bytevector)
                     bytevector-copy!)))
    ((library (rnrs bytevectors))
     (import (except (rnrs bytevectors)
                     bytevector-copy!)))
    (chicken (import (r6rs bytevectors))))

  (begin

    ;; Public overridable parameter. Defaults to 'big.
    (define current-utf32-endianness (make-parameter 'big))

    ;; ----------------------------------------------------
    ;; Core Auxiliary Routines (Parameter-Free Internal Loops)
    ;; ----------------------------------------------------

    (define (utf32-string-ref-aux bv k endian)
      (integer->char (bytevector-u32-ref bv (* k 4) endian)))

    (define (utf32-string-set-aux! bv k char endian)
      (bytevector-u32-set! bv (* k 4) (char->integer char) endian))

    (define (utf32-string=?-aux bv1 bv2 endian)
      (let ((len (bytevector-length bv1)))
        (let loop ((byte-offset 0))
          (cond ((= byte-offset len) #t)
                ((= (bytevector-u32-ref bv1 byte-offset endian)
                    (bytevector-u32-ref bv2 byte-offset endian))
                 (loop (+ byte-offset 4)))
                (else #f)))))

    (define (utf32-string-index-aux bv pred len endian)
      (let loop ((i 0))
        (cond ((= i len) #f)
              ((pred (utf32-string-ref-aux bv i endian)) i)
              (else (loop (+ i 1))))))

    (define (utf32-string-index-right-aux bv pred len endian)
      (let loop ((i (- len 1)))
        (cond ((< i 0) #f)
              ((pred (utf32-string-ref-aux bv i endian)) i)
              (else (loop (- i 1))))))

    (define (utf32-string-map-aux proc len out-bv in-bv endian)
      (let loop ((i 0))
        (if (= i len)
          out-bv
          (begin
            (utf32-string-set-aux!
             out-bv i 
             (proc (utf32-string-ref-aux in-bv i endian)) 
             endian)
            (loop (+ i 1))))))

    (define (utf32-string-fold-aux proc seed bv len endian)
      (let loop ((i 0) (acc seed))
        (if (= i len)
          acc
          (loop (+ i 1)
                (proc acc (utf32-string-ref-aux bv i endian))))))

    (define (utf32-string-fold-right-aux proc seed bv len endian)
      (let loop ((i (- len 1)) (acc seed))
        (if (< i 0)
          acc
          (loop (- i 1)
                (proc acc (utf32-string-ref-aux bv i endian))))))

    (define (make-utf32-string-aux len char endian)
      (let ((bv (make-bytevector (* len 4) 0)))
        (let loop ((i 0))
          (if (= i len)
            bv
            (begin
              (utf32-string-set-aux! bv i char endian)
              (loop (+ i 1)))))))

    ;; ----------------------------------------------------
    ;; Public Interface
    ;; ----------------------------------------------------

    ;; Returns the character length by dividing the byte length by 4.
    (define (utf32-string-length bv)
      (quotient (bytevector-length bv) 4))

    (define (utf32-string-ref bv k)
      (utf32-string-ref-aux bv k (current-utf32-endianness)))

    (define (utf32-string-set! bv k char)
      (utf32-string-set-aux! bv k char (current-utf32-endianness)))

    ;; Allocates a UTF-32 bytevector string.
    (define make-utf32-string
      (case-lambda
        ((len) (make-bytevector (* len 4) 0))
        ((len char) (make-utf32-string-aux
                     len char (current-utf32-endianness)))))

    ;; Substring copying via fast flat memory slices.
    (define utf32-string-copy
      (case-lambda
        ((bv) (bytevector-copy bv))
        ((bv start) (bytevector-copy bv (* start 4)))
        ((bv start end) (bytevector-copy bv (* start 4) (* end 4)))))

    ;; In-place block bytevector fragment translation.
    (define (utf32-string-copy! to at from start end)
      (bytevector-copy! to (* at 4) from (* start 4) (* end 4)))

    (define (utf32-string=? bv1 bv2)
      (let ((len1 (bytevector-length bv1))
            (len2 (bytevector-length bv2)))
        (if (= len1 len2)
          (utf32-string=?-aux bv1 bv2 (current-utf32-endianness))
          #f)))

    (define (utf32-string-index bv pred)
      (utf32-string-index-aux bv pred (utf32-string-length bv)
                              (current-utf32-endianness)))

    (define (utf32-string-index-right bv pred)
      (utf32-string-index-right-aux bv pred (utf32-string-length bv)
                                    (current-utf32-endianness)))

    (define (utf32-string-map proc bv)
      (let* ((len (utf32-string-length bv))
             (out (make-bytevector (* len 4) 0)))
        (utf32-string-map-aux proc len out bv
                              (current-utf32-endianness))))

    (define (utf32-string-fold proc seed bv)
      (utf32-string-fold-aux proc seed bv (utf32-string-length bv)
                             (current-utf32-endianness)))

    (define (utf32-string-fold-right proc seed bv)
      (utf32-string-fold-right-aux proc seed bv (utf32-string-length bv)
                                   (current-utf32-endianness)))

    ;; Variadic appending. Bypasses element processing entirely.
    (define (utf32-string-append . bvs)
      (utf32-string-concatenate bvs))

    ;; List concatenation. Pre-allocates exact byte layout and
    ;; blast-copies chunks.
    (define (utf32-string-concatenate bvs)
      (let* ((total-bytes
              (let loop ((lst bvs)
                         (sum 0))
                (if (not-pair? lst)
                  sum
                  (loop (cdr lst)
                        (+ sum (bytevector-length (car lst)))))))
             (out-bv (make-bytevector total-bytes 0)))
        (let loop ((lst bvs) (offset 0))
          (if (not-pair? lst)
            out-bv
            (let* ((curr (car lst))
                   (len (bytevector-length curr)))
              (bytevector-copy! out-bv offset curr 0 len)
              (loop (cdr lst) (+ offset len)))))))

    ;;
    ;; List conversions.
    ;;

    (define (list->utf32-string lst)
      (let* ((n (length lst))
             (bv (make-utf32-string n)))
        (do ((p lst (cdr p))
             (i 0 (+ i 1)))
            ((not-pair? lst))
          (utf32-string-set! bv i (car p)))
        bv))

    (define (utf32-string->list bv)
      (let ((n (utf32-string-length bv)))
        (let loop ((lst '())
                   (i 0))
          (if (= i n)
            (reverse! lst)
            (loop (cons (utf32-string-ref bv i) lst) (+ i 1))))))

    (define (utf32-string . char*)
      (list->utf32-string char*))

    ;;
    ;; Mapping between strings and utf32-strings.
    ;;

    (define (utf32-string->string bv)
      (utf32->string bv (current-utf32-endianness)))

    (define (string->utf32-string str)
      (string->utf32 str (current-utf32-endianness)))

    ;;
    ;; Some more mappings.
    ;;

    (define (symbol->utf32-string symbol)
      (string->utf32-string (symbol->string symbol)))

    (define (utf32-string->symbol bv)
      (string->symbol (utf32-string->string bv)))

    (define number->utf32-string
      (case-lambda
        ((z)
         (string->utf32-string (number->string z)))
        ((z radix)
         (string->utf32-string (number->string z radix)))))

    (define utf32-string->number
      (case-lambda
        ((bv)
         (string->number (utf32-string->string bv)))
        ((bv radix)
         (string->number (utf32-string->string bv) radix))))

    ))

;;;---------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; coding: utf-8
;;; end:

