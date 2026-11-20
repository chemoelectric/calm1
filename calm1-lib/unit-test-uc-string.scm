#!/usr/bin/env scheme-r7rs

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
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import (scheme base)
        (scheme case-lambda)
        (scheme char)
        (scheme file)
        (scheme process-context)
        (scheme write)
        (calm1-lib uc-string))

;;;
;;; Run with argument “pretty” to pretty-print the test expressions.
;;;
(define *pretty*
  (make-parameter
   (and (<= 2 (length (command-line)))
        (string=? "pretty" (cadr (command-line))))))

(define pass-count 0)
(define fail-count 0)

(define-syntax check
  (syntax-rules ()
    ((check expr)
     (begin
       (write 'expr)
       (if expr
         (begin
           (set! pass-count (+ pass-count 1))
           (display " \x21D2; pass\n"))
         (begin
           (set! fail-count (+ fail-count 1))
           (display " \x21D2; FAIL\n")))))))

(define (complement pred)
  (case-lambda
    ((arg)
     (not (pred arg)))
    (arg*
     (not (apply pred arg*)))))

(define *str->uc-str*
  (make-parameter string->uc-string))

(define (a-bunch-of-checks)
  (check (not (uc-string? 3)))
  (check (uc-string? (make-uc-string 3)))
  (check (uc-string-null? (make-uc-string 0)))
  (check (not (uc-string-null? (make-uc-string 3))))
  (check (zero? (uc-string-length (make-uc-string 0))))
  (check (= 3 (uc-string-length (make-uc-string 3))))
  (check (uc-string-null? (uc-string)))
  (check (= 3 (uc-string-length (uc-string #\a #\b #\c))))
  (check (= 3 (uc-string-length (list->uc-string '(#\a #\b #\c)))))
  (check (equal? '(#\a #\b #\c)
                 (uc-string->list (uc-string #\a #\b #\c))))
  (check (equal? '(#\b #\c)
                 (uc-string->list (uc-string #\a #\b #\c) 1)))
  (check (equal? '(#\b #\c)
                 (uc-string->list (uc-string #\a #\b #\c #\d)
                                  1 3)))
  (check (equal? '() (uc-string->list (uc-string #\a #\b #\c #\d)
                                      1 1)))
  (check (equal? '() (uc-string->list (uc-string))))
  (check (equal? '#(#\a #\b #\c)
                 (uc-string->vector (uc-string #\a #\b #\c))))
  (check (equal? '#(#\b #\c)
                 (uc-string->vector (uc-string #\a #\b #\c) 1)))
  (check (equal? '#(#\b #\c)
                 (uc-string->vector (uc-string #\a #\b #\c #\d)
                                    1 3)))
  (check (equal? '#() (uc-string->vector (uc-string #\a #\b #\c #\d)
                                         1 1)))
  (check (equal? '#() (uc-string->vector (uc-string))))
  (check (string=? "fred flintstone"
                   (uc-string->string
                    ((*str->uc-str*) "fred flintstone"))))
  (check (string=? "🔠🔡🉐🈹🟫⬛"
                   (uc-string->string
                    ((*str->uc-str*) "🔠🔡🉐🈹🟫⬛"))))
  (check (= 6 (uc-string-length
               ((*str->uc-str*) "🔠🔡🉐🈹🟫⬛"))))
  (check (string=? "srfi-152"
                   (uc-string->string
                    (symbol->uc-string 'srfi-152))))
  (check (eq? 'srfi-152
              (uc-string->symbol (symbol->uc-string 'srfi-152))))
  (check (string=? "12345" (uc-string->string
                            (number->uc-string 12345))))
  (check (string=? "30071" (uc-string->string
                            (number->uc-string 12345 8))))
  (check (= 12345 (uc-string->number ((*str->uc-str*) "12345"))))
  (check (= 12345 (uc-string->number
                   ((*str->uc-str*) "30071") 8)))
  (check (uc-string=? ((*str->uc-str*) "🔠🔡🉐🈹🟫⬛")
                      ((*str->uc-str*) "🔠🔡🉐🈹🟫⬛")))
  (check (not (uc-string=? ((*str->uc-str*) "🔠🔡🉐🈹🟫⬛")
                           ((*str->uc-str*) "fred flintstone"))))
  (let ((str ((*str->uc-str*) "🔠🔡🉐🈹🟫⬛")))
    (check (char=? #\🔠 (uc-string-ref str 0)))
    (check (char=? #\🔡 (uc-string-ref str 1)))
    (check (char=? #\🉐 (uc-string-ref str 2)))
    (check (char=? #\🈹 (uc-string-ref str 3)))
    (check (char=? #\🟫 (uc-string-ref str 4)))
    (check (char=? #\⬛ (uc-string-ref str 5)))
    (uc-string-set! str 0 #\a)
    (uc-string-set! str 1 #\b)
    (uc-string-set! str 2 #\c)
    (uc-string-set! str 3 #\d)
    (uc-string-set! str 4 #\e)
    (uc-string-set! str 5 #\f)
    (check (char=? #\a (uc-string-ref str 0)))
    (check (char=? #\b (uc-string-ref str 1)))
    (check (char=? #\c (uc-string-ref str 2)))
    (check (char=? #\d (uc-string-ref str 3)))
    (check (char=? #\e (uc-string-ref str 4)))
    (check (char=? #\f (uc-string-ref str 5))))
  (check (uc-string=? ((*str->uc-str*) "red")
                      (uc-substring
                       ((*str->uc-str*) "fred flintstone")
                       1 4)))
  (check (uc-string=? ((*str->uc-str*) "red")
                      (uc-string-copy
                       ((*str->uc-str*) "fred flintstone")
                       1 4)))
  (check (uc-string=? ((*str->uc-str*) "flintstone")
                      (uc-string-copy
                       ((*str->uc-str*) "fred flintstone") 5)))
  (check (uc-string=? ((*str->uc-str*) "fred flintstone")
                      (uc-string-copy
                       ((*str->uc-str*) "fred flintstone"))))
  (let ((str (make-uc-string 10 #\.)))
    (uc-string-copy! str 9 ((*str->uc-str*) "🔠🔡🉐🈹🟫⬛") 4 5)
    (check (uc-string=? str ((*str->uc-str*) ".........🟫"))))
  (let ((str (make-uc-string 10 #\.)))
    (uc-string-copy! str 4 ((*str->uc-str*) "🔠🔡🉐🈹🟫⬛") 3)
    (check (uc-string=? str ((*str->uc-str*) "....🈹🟫⬛..."))))
  (let ((str (make-uc-string 10 #\.)))
    (uc-string-copy! str 2 ((*str->uc-str*) "🔠🔡🉐🈹🟫⬛"))
    (check (uc-string=? str ((*str->uc-str*) "..🔠🔡🉐🈹🟫⬛.."))))
  (check (uc-string=? ((*str->uc-str*) "23456")
                      (uc-string-map (lambda (ch)
                                       (integer->char
                                        (+ 1 (char->integer ch))))
                                     ((*str->uc-str*) "12345"))))
  (check (uc-string=? ((*str->uc-str*) "1122334455")
                      (uc-string-map (lambda (ch) (uc-string ch ch))
                                     ((*str->uc-str*) "12345"))))
  (check (uc-string=? ((*str->uc-str*) "55555")
                      (uc-string-map (lambda (ch1 ch2)
                                       (integer->char
                                        (+ (char->integer ch1)
                                           (char->integer ch2)
                                           -48)))
                                     ((*str->uc-str*) "12345678")
                                     ((*str->uc-str*) "43210"))))
  (check (uc-string=? ((*str->uc-str*) "1a2b3c4d5e")
                      (uc-string-map (lambda (ch1 ch2)
                                       (uc-string ch1 ch2))
                                     ((*str->uc-str*) "12345678")
                                     ((*str->uc-str*) "abcde"))))
  (check (uc-string=? ((*str->uc-str*) "1a2b3c4d5e")
                      (uc-string-map (lambda (ch1 ch2)
                                       (string ch1 ch2))
                                     ((*str->uc-str*) "12345678")
                                     ((*str->uc-str*) "abcde"))))
  (let ((vec (make-vector 5))
        (i 0))
    (uc-string-for-each (lambda (ch)
                          (vector-set! vec i ch)
                          (set! i (+ i 1)))
                        ((*str->uc-str*) "abcde"))
    (check (equal? vec '#(#\a #\b #\c #\d #\e))))
  (let ((vec (make-vector 5))
        (i 0))
    (uc-string-for-each (lambda (ch1 ch2 ch3)
                          (vector-set! vec i (string ch1 ch2 ch3))
                          (set! i (+ i 1)))
                        ((*str->uc-str*) "abcde")
                        ((*str->uc-str*) "12345")
                        ((*str->uc-str*) ".!@#%"))
    (check (equal? vec '#("a1." "b2!" "c3@" "d4#" "e5%"))))
  (check (equal? '(#\a #\b #\c #\d #\e)
                 (uc-string-fold cons '()
                                 ((*str->uc-str*) "edcba"))))
  (check (equal? '(#\a #\b #\c)
                 (uc-string-fold cons '()
                                 ((*str->uc-str*) "edcba") 2)))
  (check (equal? '(#\b #\c #\d)
                 (uc-string-fold cons '()
                                 ((*str->uc-str*) "edcba") 1 4)))
  (check (equal? '(#\a #\b #\c #\d #\e)
                 (uc-string-fold-right cons '()
                                       ((*str->uc-str*) "abcde"))))
  (check (equal? '(#\c #\d #\e)
                 (uc-string-fold-right cons '()
                                       ((*str->uc-str*) "abcde") 2)))
  (check (equal? '(#\b #\c #\d)
                 (uc-string-fold-right cons '()
                                       ((*str->uc-str*) "abcde") 1 4)))
  (check (uc-string=? ((*str->uc-str*) "")
                      (uc-string-concatenate '())))
  (check (uc-string=? ((*str->uc-str*) "fred and wilma")
                      (uc-string-append ((*str->uc-str*) "fred")
                                        ((*str->uc-str*) " ")
                                        ((*str->uc-str*) "and")
                                        ((*str->uc-str*) " ")
                                        ((*str->uc-str*) "wilma"))))
  (check (not (uc-string-index ((*str->uc-str*) "fred flintstone")
                               (lambda (ch) (char=? ch #\q)))))
  (check (= 3 (uc-string-index ((*str->uc-str*) "fred flintstone")
                               (lambda (ch) (char=? ch #\d)))))
  (check (= 8 (uc-string-index ((*str->uc-str*) "fred flintstone")
                               (lambda (ch) (char=? ch #\n)) 5)))
  (check (not (uc-string-index ((*str->uc-str*) "fred flintstone")
                               (lambda (ch) (char=? ch #\n)) 5 8)))
  (check (= 8 (uc-string-index ((*str->uc-str*) "fred flintstone")
                               (lambda (ch) (char=? ch #\n)) 5 9)))
  (check (not (uc-string-skip ((*str->uc-str*) "fred flintstone")
                              (lambda (ch)
                                (or (char-alphabetic? ch)
                                    (char-whitespace? ch))))))
  (check (= 4 (uc-string-skip ((*str->uc-str*) "fred flintstone")
                              (lambda (ch) (char-alphabetic? ch)))))
  (check (= 4 (uc-string-skip ((*str->uc-str*) "fred flintstone")
                              (lambda (ch) (char-alphabetic? ch)) 4)))
  (check (not (uc-string-skip ((*str->uc-str*) "fred flintstone")
                              (lambda (ch) (char-alphabetic? ch)) 5)))
  (check (= 4 (uc-string-skip ((*str->uc-str*) "fred flintstone")
                              (lambda (ch) (char-alphabetic? ch)) 3 5)))
  (check (not (uc-string-skip ((*str->uc-str*) "fred flintstone")
                              (lambda (ch) (char-alphabetic? ch)) 3 4)))
  (check (not (uc-string-index-right
               ((*str->uc-str*) "fred flintstone")
               (lambda (ch) (char=? ch #\q)))))
  (check (= 13 (uc-string-index-right
                ((*str->uc-str*) "fred flintstone")
                (lambda (ch) (char=? ch #\n)))))
  (check (= 13 (uc-string-index-right
                ((*str->uc-str*) "fred flintstone")
                (lambda (ch) (char=? ch #\n)) 13)))
  (check (not (uc-string-index-right
               ((*str->uc-str*) "fred flintstone")
               (lambda (ch) (char=? ch #\n)) 14)))
  (check (= 8 (uc-string-index-right
               ((*str->uc-str*) "fred flintstone")
               (lambda (ch) (char=? ch #\n)) 8 13)))
  (check (not (uc-string-skip-right
               ((*str->uc-str*) "fred flintstone")
               (lambda (ch)
                 (or (char-alphabetic? ch)
                     (char-whitespace? ch))))))
  (check (= 4 (uc-string-skip-right
               ((*str->uc-str*) "fred flintstone")
               (lambda (ch) (char-alphabetic? ch)))))
  (check (= 4 (uc-string-skip-right
               ((*str->uc-str*) "fred flintstone")
               (lambda (ch) (char-alphabetic? ch)) 4)))
  (check (not (uc-string-skip-right
               ((*str->uc-str*) "fred flintstone")
               (lambda (ch) (char-alphabetic? ch)) 5)))
  (check (= 4 (uc-string-skip-right
               ((*str->uc-str*) "fred flintstone")
               (lambda (ch) (char-alphabetic? ch)) 4 5)))
  (check (not (uc-string-skip-right
               ((*str->uc-str*) "fred flintstone")
               (lambda (ch) (char-alphabetic? ch)) 4 4)))
  (check (uc-string=? ((*str->uc-str*) "fred and wilma")
                      (uc-string-join
                       `(,((*str->uc-str*) "fred")
                         ,((*str->uc-str*) "and")
                         ,((*str->uc-str*) "wilma")))))
  (check (uc-string=? ((*str->uc-str*) "fred, wilma, barney, betty")
                      (uc-string-join
                       `(,((*str->uc-str*) "fred")
                         ,((*str->uc-str*) "wilma")
                         ,((*str->uc-str*) "barney")
                         ,((*str->uc-str*) "betty"))
                       ((*str->uc-str*) ", "))))
  (check (uc-string=? ((*str->uc-str*) "fred, wilma, barney, betty")
                      (uc-string-join
                       `(,((*str->uc-str*) "fred")
                         ,((*str->uc-str*) "wilma")
                         ,((*str->uc-str*) "barney")
                         ,((*str->uc-str*) "betty"))
                       ((*str->uc-str*) ", ") 'infix)))
  (check (uc-string-null? (uc-string-join '())))
  (check (uc-string-null? (uc-string-join
                           '() ((*str->uc-str*) ", ") 'infix)))
  (check (guard (condition (#t #t))
           (begin
             (uc-string-join '() ((*str->uc-str*) ", ") 'strict-infix)
             #f)))
  (check (uc-string=? ((*str->uc-str*) "fred, wilma, barney, betty")
                      (uc-string-join
                       `(,((*str->uc-str*) "fred")
                         ,((*str->uc-str*) "wilma")
                         ,((*str->uc-str*) "barney")
                         ,((*str->uc-str*) "betty"))
                       ((*str->uc-str*) ", ") 'strict-infix)))
  (check (uc-string=? ((*str->uc-str*) "fred. wilma. barney. betty. ")
                      (uc-string-join
                       `(,((*str->uc-str*) "fred")
                         ,((*str->uc-str*) "wilma")
                         ,((*str->uc-str*) "barney")
                         ,((*str->uc-str*) "betty"))
                       ((*str->uc-str*) ". ") 'suffix)))
  (check (uc-string=? ((*str->uc-str*) "=fred=wilma=barney=betty")
                      (uc-string-join
                       `(,((*str->uc-str*) "fred")
                         ,((*str->uc-str*) "wilma")
                         ,((*str->uc-str*) "barney")
                         ,((*str->uc-str*) "betty"))
                       ((*str->uc-str*) "=") 'prefix)))
  (check (uc-string=? ((*str->uc-str*) "cba")
                      (reverse-list->uc-string '(#\a #\b #\c))))
  (check (equal? '(#\a #\b #\c)
                 (uc-string->reverse-list ((*str->uc-str*) "cba"))))
  (let-values (((s t)
                (uc-string-span ((*str->uc-str*) "fred flintstone")
                                char-alphabetic?)))
    (check (uc-string=? s ((*str->uc-str*) "fred")))
    (check (uc-string=? t ((*str->uc-str*) " flintstone"))))
  (let-values (((s t)
                (uc-string-span ((*str->uc-str*) "...fred flintstone")
                                char-alphabetic? 3)))
    (check (uc-string=? s ((*str->uc-str*) "fred")))
    (check (uc-string=? t ((*str->uc-str*) " flintstone"))))
  (let-values (((s t)
                (uc-string-span ((*str->uc-str*)
                                 "...fred flintstone...")
                                char-alphabetic? 3 18)))
    (check (uc-string=? s ((*str->uc-str*) "fred")))
    (check (uc-string=? t ((*str->uc-str*) " flintstone"))))
  (let-values (((s t)
                (uc-string-break ((*str->uc-str*) "fred flintstone")
                                 (complement char-alphabetic?))))
    (check (uc-string=? s ((*str->uc-str*) "fred")))
    (check (uc-string=? t ((*str->uc-str*) " flintstone"))))
  (let-values (((s t)
                (uc-string-break ((*str->uc-str*) "...fred flintstone")
                                 (complement char-alphabetic?) 3)))
    (check (uc-string=? s ((*str->uc-str*) "fred")))
    (check (uc-string=? t ((*str->uc-str*) " flintstone"))))
  (let-values (((s t)
                (uc-string-break ((*str->uc-str*)
                                  "...fred flintstone...")
                                 (complement char-alphabetic?) 3 18)))
    (check (uc-string=? s ((*str->uc-str*) "fred")))
    (check (uc-string=? t ((*str->uc-str*) " flintstone"))))
  (let-values (((s t)
                (uc-string-span ((*str->uc-str*) "fred flintstone")
                                char-numeric?)))
    (check (uc-string=? s ((*str->uc-str*) "")))
    (check (uc-string=? t ((*str->uc-str*) "fred flintstone"))))
  (let-values (((s t)
                (uc-string-span ((*str->uc-str*) "fred flintstone")
                                (complement char-numeric?))))
    (check (uc-string=? s ((*str->uc-str*) "fred flintstone")))
    (check (uc-string=? t ((*str->uc-str*) ""))))
  (let-values (((s t)
                (uc-string-break ((*str->uc-str*) "fred flintstone")
                                 (complement char-numeric?))))
    (check (uc-string=? s ((*str->uc-str*) "")))
    (check (uc-string=? t ((*str->uc-str*) "fred flintstone"))))
  (let-values (((s t)
                (uc-string-break ((*str->uc-str*) "fred flintstone")
                                 char-numeric?)))
    (check (uc-string=? s ((*str->uc-str*) "fred flintstone")))
    (check (uc-string=? t ((*str->uc-str*) ""))))
  (let-values (((s t)
                (uc-string-span ((*str->uc-str*) "...fred flintstone")
                                char-numeric? 3)))
    (check (uc-string=? s ((*str->uc-str*) "")))
    (check (uc-string=? t ((*str->uc-str*) "fred flintstone"))))
  (let-values (((s t)
                (uc-string-span ((*str->uc-str*) "...fred flintstone")
                                (complement char-numeric?) 3)))
    (check (uc-string=? s ((*str->uc-str*) "fred flintstone")))
    (check (uc-string=? t ((*str->uc-str*) ""))))
  (let-values (((s t)
                (uc-string-break ((*str->uc-str*) "...fred flintstone")
                                 (complement char-numeric?) 3)))
    (check (uc-string=? s ((*str->uc-str*) "")))
    (check (uc-string=? t ((*str->uc-str*) "fred flintstone"))))
  (let-values (((s t)
                (uc-string-break ((*str->uc-str*) "...fred flintstone")
                                 char-numeric? 3)))
    (check (uc-string=? s ((*str->uc-str*) "fred flintstone")))
    (check (uc-string=? t ((*str->uc-str*) ""))))
  (let-values (((s t)
                (uc-string-span ((*str->uc-str*)
                                 "...fred flintstone...")
                                char-numeric? 3 18)))
    (check (uc-string=? s ((*str->uc-str*) "")))
    (check (uc-string=? t ((*str->uc-str*) "fred flintstone"))))
  (let-values (((s t)
                (uc-string-span ((*str->uc-str*)
                                 "...fred flintstone...")
                                (complement char-numeric?) 3 18)))
    (check (uc-string=? s ((*str->uc-str*) "fred flintstone")))
    (check (uc-string=? t ((*str->uc-str*) ""))))
  (let-values (((s t)
                (uc-string-break ((*str->uc-str*)
                                  "...fred flintstone...")
                                 (complement char-numeric?) 3 18)))
    (check (uc-string=? s ((*str->uc-str*) "")))
    (check (uc-string=? t ((*str->uc-str*) "fred flintstone"))))
  (let-values (((s t)
                (uc-string-break ((*str->uc-str*)
                                  "...fred flintstone...")
                                 char-numeric? 3 18)))
    (check (uc-string=? s ((*str->uc-str*) "fred flintstone")))
    (check (uc-string=? t ((*str->uc-str*) ""))))
  (check (uc-string=? ((*str->uc-str*) "fred")
                      (uc-string-take-while
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic?)))
  (check (uc-string=? ((*str->uc-str*) "red")
                      (uc-string-take-while
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 1)))
  (check (uc-string=? ((*str->uc-str*) "re")
                      (uc-string-take-while
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 1 3)))
  (check (uc-string=? ((*str->uc-str*) "stone")
                      (uc-string-take-while
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 10)))
  (check (uc-string=? ((*str->uc-str*) "ston")
                      (uc-string-take-while
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 10 14)))
  (check (uc-string=? ((*str->uc-str*) "stone")
                      (uc-string-take-while
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 10 15)))
  (check (uc-string=? ((*str->uc-str*) "")
                      (uc-string-take-while
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 15 15)))
  (check (uc-string=? ((*str->uc-str*) " flintstone")
                      (uc-string-drop-while
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic?)))
  (check (uc-string=? ((*str->uc-str*) " flintstone")
                      (uc-string-drop-while
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 4)))
  (check (uc-string=? ((*str->uc-str*) "")
                      (uc-string-drop-while
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 5)))
  (check (uc-string=? ((*str->uc-str*) " flintston")
                      (uc-string-drop-while
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 0 14)))
  (check (uc-string=? ((*str->uc-str*) " flintstone")
                      (uc-string-trim
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic?)))
  (check (uc-string=? ((*str->uc-str*) " flintstone")
                      (uc-string-trim
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 4)))
  (check (uc-string=? ((*str->uc-str*) "")
                      (uc-string-trim
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 5)))
  (check (uc-string=? ((*str->uc-str*) " flintston")
                      (uc-string-trim
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 0 14)))
  (check (uc-string=? ((*str->uc-str*) " flintston")
                      (uc-string-trim
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 0 14)))
  (check (uc-string=? ((*str->uc-str*) "flintstone")
                      (uc-string-trim
                       ((*str->uc-str*) "\n\n    \tflintstone"))))
  (check (uc-string=? ((*str->uc-str*) "flintstone")
                      (uc-string-take-while-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic?)))
  (check (uc-string=? ((*str->uc-str*) "stone")
                      (uc-string-take-while-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 10)))
  (check (uc-string=? ((*str->uc-str*) "st")
                      (uc-string-take-while-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 10 12)))
  (check (uc-string=? ((*str->uc-str*) "fred")
                      (uc-string-take-while-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 0 4)))
  (check (uc-string=? ((*str->uc-str*) "fred ")
                      (uc-string-drop-while-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic?)))
  (check (uc-string=? ((*str->uc-str*) "ed ")
                      (uc-string-drop-while-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 2)))
  (check (uc-string=? ((*str->uc-str*) "fred ")
                      (uc-string-drop-while-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 0 5)))
  (check (uc-string=? ((*str->uc-str*) "")
                      (uc-string-drop-while-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 0 4)))
  (check (uc-string=? ((*str->uc-str*) "fred ")
                      (uc-string-trim-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic?)))
  (check (uc-string=? ((*str->uc-str*) "ed ")
                      (uc-string-trim-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 2)))
  (check (uc-string=? ((*str->uc-str*) "fred ")
                      (uc-string-trim-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 0 5)))
  (check (uc-string=? ((*str->uc-str*) "")
                      (uc-string-trim-right
                       ((*str->uc-str*) "fred flintstone")
                       char-alphabetic? 0 4)))
  (check (uc-string=? ((*str->uc-str*) "fred")
                      (uc-string-trim-right
                       ((*str->uc-str*) "fred      \t\t\t\n"))))
  (check (uc-string=? ((*str->uc-str*) "fred")
                      (uc-string-trim-both
                       ((*str->uc-str*) "\r \r \rfred      \t\t\t\n"))))
  (check (uc-string=? ((*str->uc-str*) "fred")
                      (uc-string-trim-both
                       ((*str->uc-str*) "...1234fred1234...")
                       char-numeric? 3 15)))
  (check (uc-string=? ((*str->uc-str*) "fred")
                      (uc-string-trim-both
                       ((*str->uc-str*) "...1234fred1234567")
                       char-numeric? 3)))
  (let-values (((s t)
                (uc-string-span-right
                 ((*str->uc-str*) "fred flintstone")
                 char-alphabetic?)))
    (check (uc-string=? s ((*str->uc-str*) "fred ")))
    (check (uc-string=? t ((*str->uc-str*) "flintstone"))))
  (let-values (((s t)
                (uc-string-break-right
                 ((*str->uc-str*) "fred flintstone")
                 (complement char-alphabetic?))))
    (check (uc-string=? s ((*str->uc-str*) "fred ")))
    (check (uc-string=? t ((*str->uc-str*) "flintstone"))))
  (let-values (((s t)
                (uc-string-span-right
                 ((*str->uc-str*) "fred flintstone")
                 char-alphabetic? 1)))
    (check (uc-string=? s ((*str->uc-str*) "red ")))
    (check (uc-string=? t ((*str->uc-str*) "flintstone"))))
  (let-values (((s t)
                (uc-string-break-right
                 ((*str->uc-str*) "fred flintstone")
                 (complement char-alphabetic?) 1)))
    (check (uc-string=? s ((*str->uc-str*) "red ")))
    (check (uc-string=? t ((*str->uc-str*) "flintstone"))))
  (let-values (((s t)
                (uc-string-span-right
                 ((*str->uc-str*) "fred flintstone")
                 char-alphabetic? 1 10)))
    (check (uc-string=? s ((*str->uc-str*) "red ")))
    (check (uc-string=? t ((*str->uc-str*) "flint"))))
  (let-values (((s t)
                (uc-string-break-right
                 ((*str->uc-str*) "fred flintstone")
                 (complement char-alphabetic?) 1 10)))
    (check (uc-string=? s ((*str->uc-str*) "red ")))
    (check (uc-string=? t ((*str->uc-str*) "flint"))))
  (check (= 15 (uc-string-contains
                ((*str->uc-str*) "eek -- what a geek.")
                ((*str->uc-str*) "ee") 12 18)))
  (check (= 15 (uc-string-contains
                ((*str->uc-str*) "eek -- what a geek.")
                ((*str->uc-str*) "cheese") 12 18 2 4)))
  (check (= 0 (uc-string-contains
               ((*str->uc-str*) "eek -- what a geek.")
               ((*str->uc-str*) "eek -- what a geek."))))
  (check (not (uc-string-contains
               ((*str->uc-str*) "eek -- what a geek.")
               ((*str->uc-str*) "eek -- what a jeek."))))
  (check (not (uc-string-contains
               ((*str->uc-str*) "eek -- what a geek.")
               ((*str->uc-str*) "@"))))
  (check (= 0 (uc-string-contains
               ((*str->uc-str*) "eek -- what a geek.")
               ((*str->uc-str*) ""))))
  (check (= 4 (uc-string-contains
               ((*str->uc-str*) "eek -- what a geek.")
               ((*str->uc-str*) "-"))))
  (check (= 4 (uc-string-contains
               ((*str->uc-str*) "eek -- what a geek.")
               ((*str->uc-str*) "--"))))
  (check (= 15 (uc-string-contains-right
                ((*str->uc-str*) "eek -- what a geek.")
                ((*str->uc-str*) "ee"))))
  (check (= 0 (uc-string-contains-right
               ((*str->uc-str*) "eek -- what a geek.")
               ((*str->uc-str*) "ee") 0 16)))
  (check (= 19 (uc-string-contains-right
                ((*str->uc-str*) "eek -- what a geek.")
                ((*str->uc-str*) ""))))
  (check (= 0 (uc-string-contains-right
               ((*str->uc-str*) "eek -- what a geek.")
               ((*str->uc-str*) "eek -- what a geek."))))
  (check (= 4 (uc-string-contains-right
               ((*str->uc-str*) "eek -- what a geek.")
               ((*str->uc-str*) "eek -- what a geek.") 0 19 4 6)))
  (check (= 17 (uc-string-contains-right
                ((*str->uc-str*) "eek -- what a geek.")
                ((*str->uc-str*) "k"))))
  (check (= 2 (uc-string-contains-right
               ((*str->uc-str*) "eek -- what a geek.")
               ((*str->uc-str*) "k") 0 17)))
  (check (= 1 (uc-string-contains-right
               ((*str->uc-str*) "eek -- what a geek.")
               ((*str->uc-str*) "ek") 0 17)))
  (check (uc-string=? ((*str->uc-str*) "  325")
                      (uc-string-pad ((*str->uc-str*) "325") 5)))
  (check (uc-string=? ((*str->uc-str*) "71325")
                      (uc-string-pad ((*str->uc-str*) "71325") 5)))
  (check (uc-string=? ((*str->uc-str*) "71325")
                      (uc-string-pad ((*str->uc-str*) "8871325") 5)))
  (check (uc-string=? ((*str->uc-str*) "...25")
                      (uc-string-pad ((*str->uc-str*) "8871325")
                                     5 #\. 5)))
  (check (uc-string=? ((*str->uc-str*) "....2")
                      (uc-string-pad ((*str->uc-str*) "8871325")
                                     5 #\. 5 6)))
  (check (uc-string=? ((*str->uc-str*) ".....")
                      (uc-string-pad ((*str->uc-str*) "8871325")
                                     5 #\. 5 5)))
  (check (uc-string=? ((*str->uc-str*) "325  ")
                      (uc-string-pad-right
                       ((*str->uc-str*) "325") 5)))
  (check (uc-string=? ((*str->uc-str*) "71325")
                      (uc-string-pad-right
                       ((*str->uc-str*) "71325") 5)))
  (check (uc-string=? ((*str->uc-str*) "88713")
                      (uc-string-pad-right
                       ((*str->uc-str*) "8871325") 5)))
  (check (uc-string=? ((*str->uc-str*) "25...")
                      (uc-string-pad-right
                       ((*str->uc-str*) "8871325")
                       5 #\. 5)))
  (check (uc-string=? ((*str->uc-str*) "2....")
                      (uc-string-pad-right
                       ((*str->uc-str*) "8871325")
                       5 #\. 5 6)))
  (check (uc-string=? ((*str->uc-str*) ".....")
                      (uc-string-pad-right
                       ((*str->uc-str*) "8871325")
                       5 #\. 5 5)))
  (check (uc-string-every char-numeric?
                          ((*str->uc-str*) "")))
  (check (uc-string-every char-numeric?
                          ((*str->uc-str*) "") 0))
  (check (uc-string-every char-numeric?
                          ((*str->uc-str*) "") 0 0))
  (check (equal? #t (uc-string-every char-numeric?
                                     ((*str->uc-str*) "1234"))))
  (check (equal? #t (uc-string-every char-numeric?
                                     ((*str->uc-str*) "  1234") 2)))
  (check (equal? #t (uc-string-every char-numeric?
                                     ((*str->uc-str*) "  1234  ") 2 6)))
  (check (not (uc-string-every char-numeric?
                               ((*str->uc-str*) "12.4"))))
  (check (not (uc-string-every char-numeric?
                               ((*str->uc-str*) "  12.4") 2)))
  (check (not (uc-string-every char-numeric?
                               ((*str->uc-str*) "  12.4  ") 2 6)))
  (check (equal? #\4 (uc-string-every (lambda (x) x)
                                      ((*str->uc-str*) "1234"))))
  (check (not (uc-string-any (complement char-numeric?)
                             ((*str->uc-str*) ""))))
  (check (not (uc-string-any (complement char-numeric?)
                             ((*str->uc-str*) "") 0)))
  (check (not (uc-string-any (complement char-numeric?)
                             ((*str->uc-str*) "") 0 0)))
  (check (not (uc-string-any (complement char-numeric?)
                             ((*str->uc-str*) "1234"))))
  (check (not (uc-string-any (complement char-numeric?)
                             ((*str->uc-str*) "  1234") 2)))
  (check (not (uc-string-any (complement char-numeric?)
                             ((*str->uc-str*) "  1234  ") 2 6)))
  (check (equal? #t (uc-string-any (complement char-numeric?)
                                   ((*str->uc-str*) "12.4"))))
  (check (equal? #t (uc-string-any (complement char-numeric?)
                                   ((*str->uc-str*) "  12.4") 2)))
  (check (equal? #t (uc-string-any (complement char-numeric?)
                                   ((*str->uc-str*) "  12.4  ") 2 6)))
  (check (equal? #\1 (uc-string-any (lambda (x) x)
                                    ((*str->uc-str*) "1234"))))
  (check (uc-string-null? (uc-string-tabulate integer->char 0)))
  (check (uc-string=? (uc-string #\null #\x1 #\x2 #\x3 #\x4)
                      (uc-string-tabulate integer->char 5)))
  (let ((port->uc-string
         (lambda (port)
           (uc-string-unfold eof-object? values
                             (lambda (x) (read-char port))
                             (read-char port))))
        (f (open-input-string "fred flintstone")))
    (check (uc-string=? ((*str->uc-str*) "fred flintstone")
                        (port->uc-string f)))
    (close-input-port f))
  (let ((port->uc-string
         (lambda (port)
           (uc-string-unfold eof-object? values
                             (lambda (x) (read-char port))
                             (read-char port)
                             #\:)))
        (f (open-input-string "fred flintstone")))
    (check (uc-string=? ((*str->uc-str*) ":fred flintstone")
                        (port->uc-string f)))
    (close-input-port f))
  (let ((port->uc-string
         (lambda (port)
           (uc-string-unfold eof-object? values
                             (lambda (x) (read-char port))
                             (read-char port)
                             "wilma and "
                             (lambda (x)
                               (if (eof-object? x)
                                 ": eof"
                                 ": not eof")))))
        (f (open-input-string "fred flintstone")))
    (check (uc-string=? ((*str->uc-str*)
                         "wilma and fred flintstone: eof")
                        (port->uc-string f)))
    (close-input-port f))
  (check (uc-string=?
          ((*str->uc-str*) " The English alphabet: \
                            abcdefghijklmnopqrstuvwxyz ")
          (uc-string-unfold-right
           (lambda (n) (= (+ n 1) (char->integer #\A)))
           (lambda (n) (char-downcase (integer->char n)))
           (lambda (n) (- n 1))
           (char->integer #\Z)
           #\space
           (lambda (n) " The English alphabet: "))))
  (check (uc-string=?
          ((*str->uc-str*) "abcdefghijklmnopqrstuvwxyz ")
          (uc-string-unfold-right
           (lambda (n) (= (+ n 1) (char->integer #\A)))
           (lambda (n) (char-downcase (integer->char n)))
           (lambda (n) (- n 1))
           (char->integer #\Z)
           #\space)))
  (check (uc-string=?
          ((*str->uc-str*) "abcdefghijklmnopqrstuvwxyz")
          (uc-string-unfold-right
           (lambda (n) (= (+ n 1) (char->integer #\A)))
           (lambda (n) (char-downcase (integer->char n)))
           (lambda (n) (- n 1))
           (char->integer #\Z))))
  (check (uc-string=?
          ((*str->uc-str*) "0123456789")
          (uc-string-unfold-right
           (lambda (n) (zero? (+ n 1)))
           (lambda (n) (number->string n))
           (lambda (n) (- n 1))
           9)))
  (check (uc-string=?
          ((*str->uc-str*) "00112233445566778899")
          (uc-string-unfold-right
           (lambda (n) (zero? (+ n 1)))
           (lambda (n) (uc-string-append
                        (number->uc-string n)
                        (number->uc-string n)))
           (lambda (n) (- n 1))
           9)))
  (check (null? (uc-string-segment (make-uc-string 0 #\null) 1)))
  (check (equal?
          '("f" "r" "e" "d" " " "f" "l" "i" "n" "t" "s" "t" "o" "n" "e")
          (map uc-string->string
               (uc-string-segment
                ((*str->uc-str*) "fred flintstone") 1))))
  (check (equal?
          '("fr" "ed" " f" "li" "nt" "st" "on" "e")
          (map uc-string->string
               (uc-string-segment
                ((*str->uc-str*) "fred flintstone") 2))))
  (check (equal?
          '("fre" "d f" "lin" "tst" "one")
          (map uc-string->string
               (uc-string-segment
                ((*str->uc-str*) "fred flintstone") 3))))
  (check (uc-string=?
          ((*str->uc-str*) "cdefab")
          (uc-string-replicate ((*str->uc-str*) "abcdef") 2 8)))
  (check (uc-string=?
          ((*str->uc-str*) "efabcd")
          (uc-string-replicate ((*str->uc-str*) "abcdef") -2 4)))
  (check (uc-string=?
          ((*str->uc-str*) "abcabca")
          (uc-string-replicate ((*str->uc-str*) "abc") 0 7)))
  (check (uc-string=?
          ((*str->uc-str*) "abcabca")
          (uc-string-replicate ((*str->uc-str*) "xxxabc") 0 7 3)))
  (check (uc-string=?
          ((*str->uc-str*) "abcabca")
          (uc-string-replicate ((*str->uc-str*) "xxxabcxxx") 0 7 3 6)))
  (check (uc-string=?
          ((*str->uc-str*) "")
          (uc-string-replicate ((*str->uc-str*) "") 0 0)))
  (check (uc-string=?
          ((*str->uc-str*) "")
          (uc-string-replicate ((*str->uc-str*) "fred flintstone")
                               7 7 5 5)))
  (check (null?
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "")
                                ((*str->uc-str*) "")))))
  (check (null?
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "")
                                ((*str->uc-str*) "anything")))))
  (check (guard (condition (#t #t))
           (begin
             (uc-string-split ((*str->uc-str*) "")
                              ((*str->uc-str*) "anything")
                              'strict-infix)
             #f)))
  (check (equal?
          '("f" "r" "e" "d" " " "f" "l" "i" "n" "t" "s" "t" "o" "n" "e")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                ""))))
  (check (equal?
          '("f" "r" "e" "d" " " "flintstone")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                "" 'infix 5))))
  (check (equal?
          '("fr" "d flintston" "")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                "e"))))
  (check (equal?
          '("fr" "d flintston" "")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                #\e 'infix))))
  (check (equal?
          '("fr" "d flintston" "")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                #\e 'strict-infix))))
  (check (equal?
          '("fr" "d flintston" "")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                #\e 'prefix))))
  (check (equal?
          '("fr" "d flintston")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                #\e 'suffix))))
  (check (equal?
          '("" "red " "lintstone")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                #\f))))
  (check (equal?
          '("" "red " "lintstone")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                #\f 'infix))))
  (check (equal?
          '("" "red " "lintstone")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                #\f 'strict-infix))))
  (check (equal?
          '("red " "lintstone")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                #\f 'prefix))))
  (check (equal?
          '("" "red " "lintstone")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*) "fred flintstone")
                                #\f 'suffix))))
  (check (equal?
          '("wilma" "stone and fred" "stone")
          (map uc-string->string
               (uc-string-split ((*str->uc-str*)
                                 "wilma flintstone and fred flintstone")
                                ((*str->uc-str*) " flint")))))
  (check (equal?
          '("wilma" "stone and fred" "stone")
          (map uc-string->string
               (uc-string-split
                ((*str->uc-str*)
                 "..wilma flintstone and fred flintstone")
                ((*str->uc-str*) " flint")
                'infix #f 2))))
  (check (equal?
          '("wilma" "stone and")
          (map uc-string->string
               (uc-string-split
                ((*str->uc-str*)
                 "..wilma flintstone and fred flintstone")
                ((*str->uc-str*) " flint")
                'infix #f 2 22))))
  (check (equal?
          '("wilma" "stone and fred flintstone")
          (map uc-string->string
               (uc-string-split
                ((*str->uc-str*)
                 "..wilma flintstone and fred flintstone")
                ((*str->uc-str*) " flint")
                'infix 1 2))))
  (check (equal?
          '("pebbles" "stone and wilma" "stone and fred flintstone")
          (map uc-string->string
               (uc-string-split
                ((*str->uc-str*)
                 "pebbles flintstone and wilma \
                flintstone and fred flintstone")
                ((*str->uc-str*) " flint")
                'infix 2))))
  (let ((str ((*str->uc-str*) "")))
    (uc-string-fill! str #\.)
    (check (uc-string=? ((*str->uc-str*) "") str)))
  (let ((str ((*str->uc-str*) "fred flintstone")))
    (uc-string-fill! str #\.)
    (check (uc-string=? ((*str->uc-str*) "...............") str)))
  (let ((str ((*str->uc-str*) "fred flintstone")))
    (uc-string-fill! str #\. 5)
    (check (uc-string=? ((*str->uc-str*) "fred ..........") str)))
  (let ((str ((*str->uc-str*) "fred flintstone")))
    (uc-string-fill! str #\. 1 4)
    (check (uc-string=? ((*str->uc-str*) "f... flintstone") str)))
  (check (uc-string=? ((*str->uc-str*) "")
                      (vector->uc-string '#())))
  (check (uc-string=? ((*str->uc-str*) "fred")
                      (vector->uc-string '#(#\f #\r #\e #\d))))
  (check (uc-string=? ((*str->uc-str*) "fred")
                      (vector->uc-string '#(#\. #\. #\f #\r #\e #\d)
                                         2)))
  (check (uc-string=? ((*str->uc-str*) "fre")
                      (vector->uc-string '#(#\. #\. #\f #\r #\e #\d)
                                         2 5)))
  (check (uc-string=?
          ((*str->uc-str*)
           "The miserable perl programmer endured daily ridicule.")
          (uc-string-replace
           ((*str->uc-str*)
            "The TCL programmer endured daily ridicule.")
           ((*str->uc-str*)
            "another miserable perl drone") 4 7 8 22)))
  (check (uc-string=?
          ((*str->uc-str*) "It’s lots of fun to code it up in Scheme.")
          (uc-string-replace
           ((*str->uc-str*) "It’s easy to code it up in Scheme.")
           ((*str->uc-str*) "lots of fun") 5 9)))
  (check (uc-string=?
          ((*str->uc-str*) "It’s fun to code it up in Scheme.")
          (uc-string-replace
           ((*str->uc-str*) "It’s easy to code it up in Scheme.")
           ((*str->uc-str*) "lots of fun") 5 9 8)))
  (check (uc-string=?
          ((*str->uc-str*)
           "It’s easy and rewarding to code it up in Scheme.")
          (uc-string-replace
           ((*str->uc-str*) "It’s easy to code it up in Scheme.")
           ((*str->uc-str*) " and rewarding") 9 9)))
  (check (uc-string=?
          ((*str->uc-str*) "Hello, I must be going.")
          (uc-string-concatenate-reverse
           (list ((*str->uc-str*) " must be")
                 ((*str->uc-str*) "Hello, I"))
           ((*str->uc-str*) " going.XXXX") 7)))
  (check (uc-string=?
          ((*str->uc-str*) "Hello, I must be going.XXXX")
          (uc-string-concatenate-reverse
           (list ((*str->uc-str*) " must be")
                 ((*str->uc-str*) "Hello, I"))
           ((*str->uc-str*) " going.XXXX"))))
  (check (uc-string=?
          ((*str->uc-str*) "Hello, I must be going.")
          (uc-string-concatenate-reverse
           (list ((*str->uc-str*) " going.")
                 ((*str->uc-str*) " must be")
                 ((*str->uc-str*) "Hello, I")))))
  (check (uc-string-null? (uc-string-concatenate-reverse '())))
  (check (zero? (uc-string-count (make-uc-string 0) (lambda (ch) #t))))
  (check (zero? (uc-string-count
                 ((*str->uc-str*) "fred flintstone") char-numeric?)))
  (check (= 1 (uc-string-count
               ((*str->uc-str*) "fred flintstone") char-whitespace?)))
  (check (= 14 (uc-string-count
                ((*str->uc-str*) "fred flintstone") char-lower-case?)))
  (check (= 10 (uc-string-count
                ((*str->uc-str*) "fred flintstone")
                char-lower-case? 4)))
  (check (= 5 (uc-string-count
               ((*str->uc-str*) "fred flintstone")
               char-lower-case? 10)))
  (check (uc-string=? ((*str->uc-str*) "red linsone")
                      (uc-string-filter
                       (lambda (ch) (and (not (char=? ch #\f))
                                         (not (char=? ch #\t))))
                       ((*str->uc-str*) "fred flintstone"))))
  (check (uc-string=? ((*str->uc-str*) "flisoe")
                      (uc-string-filter
                       (lambda (ch) (and (not (char=? ch #\n))
                                         (not (char=? ch #\t))))
                       ((*str->uc-str*) "fred flintstone") 5)))
  (check (uc-string=? ((*str->uc-str*) "fli")
                      (uc-string-filter
                       (lambda (ch) (and (not (char=? ch #\n))
                                         (not (char=? ch #\t))))
                       ((*str->uc-str*) "fred flintstone") 5 10)))
  (check (uc-string=? ((*str->uc-str*) "")
                      (uc-string-filter
                       char-numeric?
                       ((*str->uc-str*) "fred flintstone"))))
  (check (uc-string=? ((*str->uc-str*) "fftt")
                      (uc-string-remove
                       (lambda (ch) (and (not (char=? ch #\f))
                                         (not (char=? ch #\t))))
                       ((*str->uc-str*) "fred flintstone"))))
  (check (uc-string=? ((*str->uc-str*) "ftt")
                      (uc-string-remove
                       (lambda (ch) (and (not (char=? ch #\f))
                                         (not (char=? ch #\t))))
                       ((*str->uc-str*) "fred flintstone") 5)))
  (check (uc-string=? ((*str->uc-str*) "ft")
                      (uc-string-remove
                       (lambda (ch) (and (not (char=? ch #\f))
                                         (not (char=? ch #\t))))
                       ((*str->uc-str*) "fred flintstone") 5 10)))
  (check (uc-string=? ((*str->uc-str*) "fred flintstone")
                      (uc-string-remove
                       char-numeric?
                       ((*str->uc-str*) "fred flintstone"))))
  (check (uc-string=? ((*str->uc-str*) "abc")
                      ((*str->uc-str*) "abc")
                      ((*str->uc-str*) "abc")
                      ((*str->uc-str*) "abc")))
  (check (not (uc-string=? ((*str->uc-str*) "abc")
                           ((*str->uc-str*) "abc")
                           ((*str->uc-str*) "abcd")
                           ((*str->uc-str*) "abc"))))
  (check (uc-string-ci=? ((*str->uc-str*) "abc")
                         ((*str->uc-str*) "ABC")))
  (check (uc-string-ci=? ((*str->uc-str*) "abc")
                         ((*str->uc-str*) "ABC")
                         ((*str->uc-str*) "AbC")))
  (check (not (uc-string-ci=? ((*str->uc-str*) "abc")
                              ((*str->uc-str*) "ABC")
                              ((*str->uc-str*) "ABCd"))))
  (check (uc-string-ci<=? ((*str->uc-str*) "abc")
                          ((*str->uc-str*) "ABC")
                          ((*str->uc-str*) "ABCd")))
  (check (not (uc-string-ci<? ((*str->uc-str*) "abc")
                              ((*str->uc-str*) "ABC")
                              ((*str->uc-str*) "ABCd"))))
  (check (uc-string-ci<? ((*str->uc-str*) "abc")
                         ((*str->uc-str*) "ABCd")))
  (check (uc-string<=? ((*str->uc-str*) "abc")
                       ((*str->uc-str*) "abc")
                       ((*str->uc-str*) "abcd")))
  (check (not (uc-string<? ((*str->uc-str*) "abc")
                           ((*str->uc-str*) "abc")
                           ((*str->uc-str*) "abcd"))))
  (check (uc-string<? ((*str->uc-str*) "abc")
                      ((*str->uc-str*) "abcd")))
  (check (uc-string-ci>=? ((*str->uc-str*) "abCd")
                          ((*str->uc-str*) "ABC")
                          ((*str->uc-str*) "aBC")))
  (check (not (uc-string-ci>? ((*str->uc-str*) "abD")
                              ((*str->uc-str*) "ABC")
                              ((*str->uc-str*) "AbC"))))
  (check (uc-string-ci>? ((*str->uc-str*) "abcD")
                         ((*str->uc-str*) "ABC")))
  (check (uc-string>=? ((*str->uc-str*) "abcd")
                       ((*str->uc-str*) "abc")
                       ((*str->uc-str*) "abc")))
  (check (not (uc-string>? ((*str->uc-str*) "abcd")
                           ((*str->uc-str*) "abc")
                           ((*str->uc-str*) "abc"))))
  (check (uc-string>? ((*str->uc-str*) "abcd")
                      ((*str->uc-str*) "abc")))
  (check (uc-string=? ((*str->uc-str*) "abcd")
                      (uc-string-downcase ((*str->uc-str*) "ABCD"))))
  (check (uc-string=? ((*str->uc-str*) "ABCD")
                      (uc-string-upcase ((*str->uc-str*) "abcd"))))
  (check (uc-string=? ((*str->uc-str*) "abss")
                      (uc-string-foldcase ((*str->uc-str*) "ABß"))))
  (check (zero? (uc-string-prefix-length
                 ((*str->uc-str*) "fred flintstone")
                 ((*str->uc-str*) "wilma flintstone"))))
  (check (= 5 (uc-string-prefix-length
               ((*str->uc-str*) "fred flintstone")
               ((*str->uc-str*) "fred FLINTSTONE"))))
  (check (= 5 (uc-string-prefix-length
               ((*str->uc-str*) "xxfred flintstone")
               ((*str->uc-str*) "fred FLINTSTONE")
               2)))
  (check (= 3 (uc-string-prefix-length
               ((*str->uc-str*) "xxfred flintstone")
               ((*str->uc-str*) "fred FLINTSTONE")
               2 5)))
  (check (= 2 (uc-string-prefix-length
               ((*str->uc-str*) "xxfred flintstone")
               ((*str->uc-str*) "xxxxfred FLINTSTONE")
               2 5 4 6)))
  (check (zero? (uc-string-prefix-length
                 ((*str->uc-str*) "wilma flintstone")
                 ((*str->uc-str*) "fred flintstone"))))
  (check (= 5 (uc-string-prefix-length
               ((*str->uc-str*) "fred FLINTSTONE")
               ((*str->uc-str*) "fred flintstone"))))
  (check (= 5 (uc-string-prefix-length
               ((*str->uc-str*) "fred FLINTSTONE")
               ((*str->uc-str*) "xxfred flintstone")
               0 15 2)))
  (check (= 3 (uc-string-prefix-length
               ((*str->uc-str*) "fred FLINTSTONE")
               ((*str->uc-str*) "xxfred flintstone")
               0 15 2 5)))
  (check (= 2 (uc-string-prefix-length
               ((*str->uc-str*) "xxxxfred FLINTSTONE")
               ((*str->uc-str*) "xxfred flintstone")
               4 6 2 5)))
  (let ((str ((*str->uc-str*) "fred FLINTSTONE")))
    (check (= 15 (uc-string-prefix-length str str))))
  (let ((str ((*str->uc-str*) "fred FLINTSTONE")))
    (check (= 10 (uc-string-prefix-length str str 5 15 5))))
  (let ((str ((*str->uc-str*) "fred FLINTSTONE")))
    (check (= 10 (uc-string-prefix-length str str 5 15 5 15))))
  (let ((str ((*str->uc-str*) "fred FLINTSTONE")))
    (check (zero? (uc-string-prefix-length str str 4 15 5 15))))
  (let ((str ((*str->uc-str*) "fred flintstone")))
    (check (= 1 (uc-string-prefix-length str str 0 1 5 6))))
  (check (= 11 (uc-string-suffix-length
                ((*str->uc-str*) "fred flintstone")
                ((*str->uc-str*) "wilma flintstone"))))
  (check (zero? (uc-string-suffix-length
                 ((*str->uc-str*) "fred flintstone")
                 ((*str->uc-str*) "wilma FLINTSTONE"))))
  (check (= 11 (uc-string-suffix-length
                ((*str->uc-str*) "fred flintstone")
                ((*str->uc-str*) "xxwilma flintstone")
                0 15 2)))
  (check (= 11 (uc-string-suffix-length
                ((*str->uc-str*) "xxfred flintstone")
                ((*str->uc-str*) "wilma flintstone")
                2)))
  (check (= 1 (uc-string-suffix-length
               ((*str->uc-str*) "xxfred flintstone")
               ((*str->uc-str*) "wilma flintstone")
               2 5)))
  (check (= 10 (uc-string-suffix-length
                ((*str->uc-str*) "fred flintstone")
                ((*str->uc-str*) "xxwilma flintstone")
                0 14 2 17)))
  (check (uc-string-prefix?
          ((*str->uc-str*) "fred")
          ((*str->uc-str*) "fred flintstone")))
  (check (not (uc-string-prefix?
               ((*str->uc-str*) "fred flintstone")
               ((*str->uc-str*) "fred"))))
  (check (uc-string-prefix?
          ((*str->uc-str*) "")
          ((*str->uc-str*) "fred flintstone")))
  (check (not (uc-string-prefix?
               ((*str->uc-str*) "wilma")
               ((*str->uc-str*) "fred flintstone"))))
  (check (uc-string-prefix?
          ((*str->uc-str*) "xxfred")
          ((*str->uc-str*) "fred flintstone")
          2))
  (check (uc-string-prefix?
          ((*str->uc-str*) "xxfredxx")
          ((*str->uc-str*) "fred flintstone")
          2 6))
  (check (uc-string-prefix?
          ((*str->uc-str*) "xxfredxx")
          ((*str->uc-str*) "xxxfred flintstone")
          2 6 3))
  (check (not (uc-string-prefix?
               ((*str->uc-str*) "xxfredxx")
               ((*str->uc-str*) "xxxfrexxxd flintstone")
               2 6 3)))
  (check (uc-string-prefix?
          ((*str->uc-str*) "xxfredxx")
          ((*str->uc-str*) "xxxfrexxxd flintstone")
          2 5 3))
  (check (not (uc-string-prefix?
               ((*str->uc-str*) "xxfredxx")
               ((*str->uc-str*) "xxxfrexxxd flintstone")
               2 5 3 5)))
  (check (uc-string-prefix?
          ((*str->uc-str*) "xxfredxx")
          ((*str->uc-str*) "xxxfrexxxd flintstone")
          2 4 3 5))
  (check (uc-string-suffix?
          ((*str->uc-str*) "flintstone")
          ((*str->uc-str*) "fred flintstone")))
  (check (not (uc-string-suffix?
               ((*str->uc-str*) "rubble")
               ((*str->uc-str*) "fred flintstone"))))
  (check (uc-string-suffix?
          ((*str->uc-str*) "")
          ((*str->uc-str*) "fred flintstone")))
  (check (uc-string-suffix?
          ((*str->uc-str*) "xxxflintstone")
          ((*str->uc-str*) "fred flintstone")
          3))
  (check (uc-string-suffix?
          ((*str->uc-str*) "xxxflintstonexxx")
          ((*str->uc-str*) "fred flintstone")
          3 13))
  (check (uc-string-suffix?
          ((*str->uc-str*) "xxxflintstonexxx")
          ((*str->uc-str*) ".....fred flintstone")
          3 13 5))
  (check (uc-string-suffix?
          ((*str->uc-str*) "xxxflintstonexxx")
          ((*str->uc-str*) ".....fred flintstone.....")
          3 13 5 20))
  (let ((test-file "unit-test-uc-string.test-file"))
    (when (file-exists? test-file)
      (delete-file test-file))
    (with-output-to-file test-file
      (lambda ()
        (write-uc-string ((*str->uc-str*) "fred flintstone\n"))))
    (with-input-from-file test-file
      (lambda ()
        (let ((str (read-uc-string 16)))
          (check (eof-object? (read-uc-string 1)))
          (check (uc-string=? str ((*str->uc-str*)
                                   "fred flintstone\n"))))))
    (when (file-exists? test-file)
      (delete-file test-file)))
  (check (uc-string=? ((*str->uc-str*) "flint")
                      ((*str->uc-str*) "fred flint" 5)
                      ((*str->uc-str*) "fred flintstone" 5 10)))
  (check (string=?
          (uc-string->string ((*str->uc-str*) "flint"))
          (uc-string->string ((*str->uc-str*) "fred flint") 5)
          (uc-string->string ((*str->uc-str*) "fred flintstone") 5 10)))
  (check (eq? (uc-string->symbol ((*str->uc-str*) "flint"))
              (uc-string->symbol ((*str->uc-str*) "fred flint") 5)))
  (check (eq? (uc-string->symbol ((*str->uc-str*) "flint"))
              (uc-string->symbol ((*str->uc-str*) "fred flintstone")
                                 5 10)))
  (check (uc-string=?
          ((*str->uc-str*) "E=mc² é é")
          (uc-string-normalize-nfc
           ((*str->uc-str*) "E=mc² é e\x301;"))))
  (check (uc-string=?
          ((*str->uc-str*) "E=mc2 é é")
          (uc-string-normalize-nfkc
           ((*str->uc-str*) "E=mc² é e\x301;"))))
  (check (uc-string=?
          ((*str->uc-str*) "E=mc² e\x301; e\x301;")
          (uc-string-normalize-nfd
           ((*str->uc-str*) "E=mc² é e\x301;"))))
  (check (uc-string=?
          ((*str->uc-str*) "E=mc2 e\x301; e\x301;")
          (uc-string-normalize-nfkd
           ((*str->uc-str*) "E=mc² é e\x301;"))))
  (check (uc-string=?
          ((*str->uc-str*) "flintstone")
          (let ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 5)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "flint")
          (let ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 5 10)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "stone")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 5)
            (uc-string-resize! str 5)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "flint")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 5)
            (uc-string-resize! str 0 5)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "lint")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 5)
            (uc-string-resize! str 1 5)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "lintstone     ")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 5)
            (uc-string-resize! str 1 15)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "fred      ")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 0 5)
            (uc-string-resize! str 0 10)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "fred                ")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 0 5)
            (uc-string-resize! str 0 20)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "     fred                ")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 0 5)
            (uc-string-resize! str -5 20)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "     fred                ")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 0 5)
            (uc-string-resize! str -5 20)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "   ")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 0 5)
            (uc-string-resize! str -5 -2)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "   ")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 0 5)
            (uc-string-resize! str 7 10)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "     ")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str -10 -5)
            str)))
  (check (uc-string=?
          ((*str->uc-str*) "     ")
          (let* ((str ((*str->uc-str*) "fred flintstone")))
            (uc-string-resize! str 100 105)
            str)))
  )

(a-bunch-of-checks)
(parameterize
    ((*str->uc-str*
      (let ((s->u
             (lambda (s)
               (let* ((t (string-append "xxxxxxxxxx" s "xxxxxxxxxx"))
                      (str (string->uc-string t)))
                 (uc-string-resize! str 10 (+ 10 (string-length s)))
                 str))))
        (case-lambda
          ((s) (s->u s))
          ((s start) (s->u (string-copy s start)))
          ((s start end) (s->u (string-copy s start end)))))))
  (a-bunch-of-checks))
(parameterize
    ((*str->uc-str*
      (let ((s->u
             (lambda (s)
               (let* ((t (string-append s
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"))
                      (str (string->uc-string t)))
                 (uc-string-resize! str 0 (+ 0 (string-length s)))
                 str))))
        (case-lambda
          ((s) (s->u s))
          ((s start) (s->u (string-copy s start)))
          ((s start end) (s->u (string-copy s start end)))))))
  (a-bunch-of-checks))
(parameterize
    ((*str->uc-str*
      (let ((s->u
             (lambda (s)
               (let* ((t (string-append "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        s
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"))
                      (str (string->uc-string t)))
                 (uc-string-resize! str 80 (+ 80 (string-length s)))
                 str))))
        (case-lambda
          ((s) (s->u s))
          ((s start) (s->u (string-copy s start)))
          ((s start end) (s->u (string-copy s start end)))))))
  (a-bunch-of-checks))
(parameterize
    ((*str->uc-str*
      (let ((s->u
             (lambda (s)
               (let* ((t (string-append "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        "xxxxxxxxxx"
                                        s))
                      (str (string->uc-string t)))
                 (uc-string-resize! str 80 (+ 80 (string-length s)))
                 str))))
        (case-lambda
          ((s) (s->u s))
          ((s start) (s->u (string-copy s start)))
          ((s start end) (s->u (string-copy s start end)))))))
  (a-bunch-of-checks))
(parameterize
    ((*str->uc-str*
      (let ((s->u
             (lambda (s)
               (let* ((t (string-append "x" s "x"))
                      (str (string->uc-string t)))
                 (uc-string-resize! str 1 (+ 1 (string-length s)))
                 str))))
        (case-lambda
          ((s) (s->u s))
          ((s start) (s->u (string-copy s start)))
          ((s start end) (s->u (string-copy s start end)))))))
  (a-bunch-of-checks))
(parameterize
    ((*str->uc-str*
      (let ((s->u
             (lambda (s)
               (let* ((t (string-append "x" s))
                      (str (string->uc-string t)))
                 (uc-string-resize! str 1 (+ 1 (string-length s)))
                 str))))
        (case-lambda
          ((s) (s->u s))
          ((s start) (s->u (string-copy s start)))
          ((s start end) (s->u (string-copy s start end)))))))
  (a-bunch-of-checks))
(parameterize
    ((*str->uc-str*
      (let ((s->u
             (lambda (s)
               (let* ((t (string-append s "x"))
                      (str (string->uc-string t)))
                 (uc-string-resize! str 0 (+ 0 (string-length s)))
                 str))))
        (case-lambda
          ((s) (s->u s))
          ((s start) (s->u (string-copy s start)))
          ((s start end) (s->u (string-copy s start end)))))))
  (a-bunch-of-checks))

(display "pass: ")
(display pass-count)
(newline)
(display (if (zero? fail-count) "fail: " "FAIL: "))
(display fail-count)
(newline)
(exit (zero? fail-count))
