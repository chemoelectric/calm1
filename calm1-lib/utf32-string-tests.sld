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

(define-library (calm1-lib utf32-string-tests)

  (export run-utf32-string-tests)

  (import (scheme base)
          (scheme cxr)
          (scheme write)
          (calm1-lib utf32-string))

  (begin

    (define (check name expected actual)
      (if (equal? expected actual)
        (begin (display "PASS: ") (display name) (newline))
        (begin (display "FAIL: ") (display name) 
               (display " | Expected: ") (write expected)
               (display " Got: ") (write actual) (newline))))

    (define (run-utf32-string-tests)
      (display "Running UTF-32 String Engine Regression Tests...") (newline)
      
      ;; --------------------------------------------------
      ;; Encoding, Length, and References
      ;; --------------------------------------------------
      (let* ((str "A[λ]Z")
             (bv-big (string->utf32-string str))) ; default 'big
        
        (check "Length count" 5 (utf32-string-length bv-big))
        (check "Ref char index 0" #\A (utf32-string-ref bv-big 0))
        (check "Ref char index 1" #\[ (utf32-string-ref bv-big 1))
        (check "Ref char index 2" #\λ (utf32-string-ref bv-big 2))
        (check "Ref char index 4" #\Z (utf32-string-ref bv-big 4))

        (parameterize ((current-utf32-endianness 'little))
          (define bv-lit (string->utf32-string str))
          (check "Little-endian ref" #\λ (utf32-string-ref bv-lit 2))
          (check "Cross-endian comparison mismatch" #f (utf32-string=? bv-big bv-lit))))

      ;; --------------------------------------------------
      ;; Mutation and Allocation
      ;; --------------------------------------------------
      (let ((bv (make-utf32-string 3 #\space)))
        (utf32-string-set! bv 0 #\{)
        (utf32-string-set! bv 1 #\X)
        (utf32-string-set! bv 2 #\})
        (check "Mutation structural match" "{X}" (utf32-string->string bv)))

      ;; --------------------------------------------------
      ;; Scanning: Index and Index-Right
      ;; --------------------------------------------------
      (let ((bv (string->utf32-string "ab[c]d[e]f")))
        (define (bracket? c) (or (char=? c #\[) (char=? c #\])))
        
        (check "Leftmost bracket index" 2 (utf32-string-index bv bracket?))
        (check "Rightmost bracket index" 8 (utf32-string-index-right bv bracket?))
        (check "Unmatched scan left" #f (utf32-string-index bv (lambda (c) (char=? c #\Z))))
        (check "Unmatched scan right" #f (utf32-string-index-right bv (lambda (c) (char=? c #\Z)))))

      ;; --------------------------------------------------
      ;; Slicing and Moving
      ;; --------------------------------------------------
      (let* ((src (string->utf32-string "0123456"))
             (dest (string->utf32-string "abcdefg"))
             (sliced (utf32-string-copy src 2 5)))
        
        (check "Substring copy extraction" "234" (utf32-string->string sliced))
        
        (utf32-string-copy! dest 1 src 3 6)
        (check "In-place block injection" "a345efg" (utf32-string->string dest)))

      ;; --------------------------------------------------
      ;; Functional Operators: Map and Folds
      ;; --------------------------------------------------
      (let ((bv (string->utf32-string "123")))
        (define (shift-char c) (integer->char (+ (char->integer c) 1)))
        
        (check "Map transformation" "234" 
               (utf32-string->string (utf32-string-map shift-char bv)))
        
        (check "Fold-left string processing" '( #\3 #\2 #\1 . #\0)
               (utf32-string-fold (lambda (acc c) (cons c acc)) #\0 bv))
        
        (check "Fold-right string processing" '( #\1 #\2 #\3 . #\0)
               (utf32-string-fold-right (lambda (acc c) (cons c acc)) #\0 bv)))

      ;; --------------------------------------------------
      ;; Aggregation and Concatenation
      ;; --------------------------------------------------
      (let ((bvs (list (string->utf32-string "SNOBOL")
                       (string->utf32-string "-")
                       (string->utf32-string "4"))))
        
        (check "Variadic append block moves" "SNOBOL-4" 
               (utf32-string->string (utf32-string-append (car bvs) (cadr bvs) (caddr bvs))))
        
        (check "List concatenation block moves" "SNOBOL-4" 
               (utf32-string->string (utf32-string-concatenate bvs))))

      ;; --------------------------------------------------
      ;; Symbols
      ;; --------------------------------------------------
      (let ()

        (check "symbol->utf32-string" "symbol12345"
               (utf32-string->string (symbol->utf32-string 'symbol12345)))

        (check "utf32-string->symbol" 'symbol12345
               (utf32-string->symbol (string->utf32-string "symbol12345"))) )

      ;; --------------------------------------------------
      ;; Numbers
      ;; --------------------------------------------------
      (let ()

        (check "number->utf32-string" "12345"
               (utf32-string->string (number->utf32-string 12345)))

        (check "utf32-string->number" 12345
               (utf32-string->number (string->utf32-string "12345"))) )

      (display "Verification pipeline completed.") (newline))

    ))

;;;---------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; coding: utf-8
;;; end:
