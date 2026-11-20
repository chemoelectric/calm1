;; Copyright © 2026 Barry Schwartz
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions: The above
;; copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
;;;---------------------------------------------------------------------

(define-library (calm1-lib preprocessor-text)

  (import (scheme base)
          (scheme case-lambda)
          (scheme write))
  (cond-expand
    ((library (srfi 1)) (import (srfi 1)))
    ((library (scheme list)) (import (scheme list)))
    (else (import (srfi srfi-1))))
  (import (calm1-lib uc-string))

  (export preprocessor-text
          set-preprocessor-text!
          @in
          @ins
          @out
          @outs
          @raw
          @drop
          preprocessor-serialize
          preprocessor-stringize
          preprocessor-read-to-string
          preprocessor-display-as-string
          
          converting-scheme-comment-brackets
          set-converting-scheme-comment-brackets!
          scheme-comment-left
          scheme-comment-right)

  (begin

;;;---------------------------------------------------------------------

    (define *preprocessor-text*
      (make-parameter (list #f)))

    (define (set-preprocessor-text! u32)
      (unless (uc-string? u32)
        (error "expected a uc-string" u32))
      (set-car! (*preprocessor-text*) u32))

    (define (preprocessor-text)
      (car (*preprocessor-text*)))

    (define (@drop n)
      (let* ((text (preprocessor-text))
             (len (min n (uc-string-length text))))
        (uc-string-resize! text len)))

    (define (@raw n)
      (let* ((text (preprocessor-text))
             (len (min n (uc-string-length text))))
        (@out (uc-string-copy text 0 len))
        (@drop len)))

    (define (@in str)
      (set-preprocessor-text!
       (uc-string-append (preprocessor-stringize str)
                         (preprocessor-text))))

    (define (@ins obj)
      (let ((f (open-output-string)))
        (write obj f)
        (set-preprocessor-text!
         (uc-string-append (preprocessor-stringize
                            (get-output-string f))
                           (preprocessor-text)))
        (close-port f)))

;;;---------------------------------------------------------------------

    (define (preprocessor-serialize obj)
      (let ((port (open-output-string)))
        (write obj port)
        (string->uc-string (get-output-string port))))

    (define (preprocessor-stringize x)
      (cond ((uc-string? x) x)
            ((string? x) (string->uc-string x))
            ((symbol? x) (symbol->uc-string x))
            ((number? x) (number->uc-string x))
            ((char? x) (uc-string (char->integer x)))
            (else (preprocessor-serialize x))))

;;;---------------------------------------------------------------------

    (define preprocessor-read-to-string
      (let ((n 4096))
        (case-lambda
          (()
           (preprocessor-read-to-string (current-input-port)))
          ((port)
           (comment-brackets->chars
            (let loop ((lst '()))
              (let ((s (read-string n port)))
                (if (eof-object? s)
                  (uc-string-concatenate
                   (map! string->uc-string (reverse! lst)))
                  (loop (cons s lst))))))))))

    (define preprocessor-display-as-string
      (case-lambda
        ((obj)
         (preprocessor-display-as-string obj (current-output-port)))
        ((obj port)
         (display
          (uc-string->string
           (chars->comment-brackets (preprocessor-stringize obj)))
          port))))

    (define @out preprocessor-display-as-string)

    (define @outs
      (case-lambda
        ((obj)
         (@outs obj (current-output-port)))
        ((obj port)
         (let ((f (open-output-string)))
           (write obj f)
           (@out (get-output-string f) port)
           (close-port f)))))

;;;---------------------------------------------------------------------

    (define *converting-scheme-comment-brackets*
      (make-parameter (list #f)))

    (define (converting-scheme-comment-brackets)
      (car (*converting-scheme-comment-brackets*)))

    (define (set-converting-scheme-comment-brackets! bool)
      (unless (boolean? bool)
        (error "expected a boolean" bool))
      (set-car! (*converting-scheme-comment-brackets*) bool))

    ;; "#|" and "|#" get converted to these code points, for handling
    ;; as if they were bracket characters. These code points are
    ;; chosen haphazardly from Supplemental Private Use Area-B.
    (define scheme-comment-left #\x10A631)
    (define scheme-comment-right #\x10A632)

    (define (comment-brackets->chars str)
      (if (converting-scheme-comment-brackets)
        (comment-brackets->chars%%aux str)
        (uc-string-copy str)))

    (define (chars->comment-brackets str)
      (if (converting-scheme-comment-brackets)
        (chars->comment-brackets%%aux str)
        (uc-string-copy str)))

    (define (comment-brackets->chars%%aux str)
      ;;
      ;; Convert "#|" and "|#" to single code points in an
      ;; out-of-the-way range.
      ;;
      (let* ((n (uc-string-length str))
             (t (make-uc-string n)))
        (let loop1 ((i 0)
                    (k 0))
          (if (= i n)
            (uc-string-copy t 0 k)
            (let loop2 ((j i))
              (cond ((= j n)
                     (uc-string-copy! t k str i j)
                     (loop1 j (+ k (- j i))))
                    ((and (not (= (+ j 1) n))
                          (char=? (uc-string-ref str j) #\#)
                          (char=? (uc-string-ref str (+ j 1)) #\|))
                     (uc-string-copy! t k str i j)
                     (uc-string-set! t (+ k (- j i))
                                     scheme-comment-left)
                     (loop1 (+ j 2) (+ k (- j i) 1)))
                    ((and (not (= (+ j 1) n))
                          (char=? (uc-string-ref str j) #\|)
                          (char=? (uc-string-ref str (+ j 1)) #\#))
                     (uc-string-copy! t k str i j)
                     (uc-string-set! t (+ k (- j i))
                                     scheme-comment-right)
                     (loop1 (+ j 2) (+ k (- j i) 1)))
                    (else
                     (loop2 (+ j 1)))))))))

    (define (chars->comment-brackets%%aux str)
      ;;
      ;; Convert certain code points to "#|" and "|#".
      ;;
      (let* ((n (uc-string-length str))
             (t (make-uc-string (+ n n))))
        (let loop1 ((i 0)
                    (k 0))
          (if (= i n)
            (uc-string-copy t 0 k)
            (let loop2 ((j i))
              (cond ((= j n)
                     (uc-string-copy! t k str i j)
                     (loop1 j (+ k (- j i))))
                    ((char=? (uc-string-ref str j)
                             scheme-comment-left)
                     (uc-string-copy! t k str i j)
                     (uc-string-set! t (+ k (- j i)) #\#)
                     (uc-string-set! t (+ k (- j i) 1) #\|)
                     (loop1 (+ j 1) (+ k (- j i) 2)))
                    ((char=? (uc-string-ref str j)
                             scheme-comment-right)
                     (uc-string-copy! t k str i j)
                     (uc-string-set! t (+ k (- j i)) #\|)
                     (uc-string-set! t (+ k (- j i) 1) #\#)
                     (loop1 (+ j 1) (+ k (- j i) 2)))
                    (else
                     (loop2 (+ j 1)))))))))

    ))

;;;---------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; coding: utf-8
;;; end:
