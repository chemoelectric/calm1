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
;;;
;;; A 128-bit LCG random number generator with output mixing.
;;;
;;; Core generator: 128-bit linear congruential generator using
;;; the Steele-Vigna 64-bit multiplier technique and Stafford13 mixing,
;;; implemented using multi-limb fixnum arithmetic (via SRFI-143)
;;; so that it works independently of host fixnum width.
;;;
;;; make-random-128bits
;;;
;;;     Returns a generator of 128-bit bytevectors.
;;;
;;; make-random-fixnum
;;;
;;;     Returns a generator of non-negative fixnums.
;;;
;;; make-random-integer
;;;
;;;     Returns a generator of bounded random integers in [0, n-1] or
;;;     [i, i + n - 1], using pure fixnum arithmetic when n is a
;;;     fixnum.
;;;
;;;---------------------------------------------------------------------

(define-library (calm1-lib random-fixnum)

  (export make-random-128bits)
  (export make-random-fixnum)
  (export make-random-integer)

  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme file))
  (cond-expand
    ((library (scheme fixnum)) (import (scheme fixnum)))
    ((library (srfi 143)) (import (srfi 143)))
    (else (import (srfi srfi-143))))

  (begin

    ;;------------------------------------------------------------------
    ;; Obtain a 16-byte seed from /dev/urandom or fallback.
    ;;------------------------------------------------------------------

    (define fallback-seed-128
      (bytevector #x2d #x7f #x95 #x4c #x2d #xf4 #x51 #x58
                  #x4f #x81 #x67 #xf7 #x7e #x7b #x05 #x14))

    (define (urandom-bytevector-16)
      (guard (ex (else (bytevector-copy fallback-seed-128)))
        (call-with-port (open-binary-input-file "/dev/urandom")
          (lambda (port)
            (let ((bv (read-bytevector 16 port)))
              (if (and (bytevector? bv) (= (bytevector-length bv) 16))
                bv
                (bytevector-copy fallback-seed-128)))))))

    ;;------------------------------------------------------------------
    ;; 16-bit limb constants and operations for 128-bit LCG.
    ;;
    ;; Steele-Vigna 64-bit multiplier M_8:
    ;;   A = 0xf1357aea2e62a9c5
    ;; 16-bit limbs (little-endian):
    ;;------------------------------------------------------------------

    (define a-limb0 #xa9c5)
    (define a-limb1 #x2e62)
    (define a-limb2 #x7aea)
    (define a-limb3 #xf135)

    ;; 128-bit odd additive constant:
    ;;   C = 0x14057b7ef767814f14057b7ef767814f
    ;; 16-bit limbs (little-endian):
    (define c-limb0 #x814f)
    (define c-limb1 #xf767)
    (define c-limb2 #x7b7e)
    (define c-limb3 #x1405)
    (define c-limb4 #x814f)
    (define c-limb5 #xf767)
    (define c-limb6 #x7b7e)
    (define c-limb7 #x1405)

    ;; Stafford13 mixing multipliers for 64-bit high half:
    ;;   K1 = 0xbf58476d1ce4e5b9
    ;;   K2 = 0x94d049bb133111eb
    (define k1-limb0 #xe5b9)
    (define k1-limb1 #x1ce4)
    (define k1-limb2 #x476d)
    (define k1-limb3 #xbf58)

    (define k2-limb0 #x11eb)
    (define k2-limb1 #x1331)
    (define k2-limb2 #x49bb)
    (define k2-limb3 #x94d0)

    ;; Load 8 16-bit limbs from little-endian 16-byte bytevector.
    (define (bv->limbs bv vec)
      (let loop ((i 0))
        (when (fx<? i 8)
          (let* ((b0 (bytevector-u8-ref bv (fx* 2 i)))
                 (b1 (bytevector-u8-ref bv (fx+ (fx* 2 i) 1)))
                 (val (fxior b0 (fxarithmetic-shift-left b1 8))))
            (vector-set! vec i val)
            (loop (fx+ i 1))))))

    ;; Store 8 16-bit limbs into little-endian 16-byte bytevector.
    (define (limbs->bv! vec bv)
      (let loop ((i 0))
        (when (fx<? i 8)
          (let ((val (vector-ref vec i)))
            (bytevector-u8-set! bv (fx* 2 i) (fxand val #xff))
            (bytevector-u8-set! bv (fx+ (fx* 2 i) 1)
                                (fxarithmetic-shift-right val 8))
            (loop (fx+ i 1))))))

    ;; Advance 8-limb state in place: S = (S * A + C) mod 2^128. Uses
    ;; 16-bit chunks; each limb product is at most 16x16 = 32 bits,
    ;; fitting comfortably into any SRFI-143 fixnum.
    (define (step-lcg! s acc)
      ;; Initialize acc with additive constant C
      (vector-set! acc 0 c-limb0)
      (vector-set! acc 1 c-limb1)
      (vector-set! acc 2 c-limb2)
      (vector-set! acc 3 c-limb3)
      (vector-set! acc 4 c-limb4)
      (vector-set! acc 5 c-limb5)
      (vector-set! acc 6 c-limb6)
      (vector-set! acc 7 c-limb7)

      ;; Multiply s (8 limbs) by A (4 limbs) and accumulate.
      (let a-loop ((j 0) (aj a-limb0))
        (when (fx<? j 4)
          (let ((curr-a (case j
                          ((0) a-limb0)
                          ((1) a-limb1)
                          ((2) a-limb2)
                          ((3) a-limb3)
                          (else 0))))
            (let s-loop ((i 0) (carry 0))
              (if (fx<? i (fx- 8 j))
                (let* ((k (fx+ i j))
                       (prod (fx* (vector-ref s i) curr-a))
                       (sum1 (fx+ (vector-ref acc k) carry))
                       (total (fx+ sum1 prod))
                       (new-val (fxand total #xffff))
                       (new-carry (fxarithmetic-shift-right
                                   total 16)))
                  (vector-set! acc k new-val)
                  (s-loop (fx+ i 1) new-carry))
                ;; Copy acc back to s for the next iteration.
                #t)))
          (a-loop (fx+ j 1) (case (fx+ j 1)
                              ((1) a-limb1)
                              ((2) a-limb2)
                              ((3) a-limb3)
                              (else 0)))))
      ;; Copy acc into s
      (let copy-loop ((idx 0))
        (when (fx<? idx 8)
          (vector-set! s idx (vector-ref acc idx))
          (copy-loop (fx+ idx 1)))))

    ;; 4-limb (64-bit) multiplication mod 2^64.
    (define (mul64! z k0 k1 k2 k3 acc)
      (vector-set! acc 0 0)
      (vector-set! acc 1 0)
      (vector-set! acc 2 0)
      (vector-set! acc 3 0)
      (let j-loop ((j 0))
        (when (fx<? j 4)
          (let ((kj (case j
                      ((0) k0)
                      ((1) k1)
                      ((2) k2)
                      ((3) k3)
                      (else 0))))
            (let i-loop ((i 0) (carry 0))
              (if (fx<? i (fx- 4 j))
                (let* ((idx (fx+ i j))
                       (prod (fx* (vector-ref z i) kj))
                       (sum1 (fx+ (vector-ref acc idx) carry))
                       (total (fx+ sum1 prod))
                       (new-val (fxand total #xffff))
                       (new-carry (fxarithmetic-shift-right total 16)))
                  (vector-set! acc idx new-val)
                  (i-loop (fx+ i 1) new-carry))
                #t)))
          (j-loop (fx+ j 1))))
      (vector-set! z 0 (vector-ref acc 0))
      (vector-set! z 1 (vector-ref acc 1))
      (vector-set! z 2 (vector-ref acc 2))
      (vector-set! z 3 (vector-ref acc 3)))

    ;; 64-bit right-shift by 30 and XOR into z.
    (define (xor-shift-right-30! z)
      ;; z >> 30:
      ;; bit 30..47 is in limb 1 (bits 14..15) and limb 2 (bits 0..15)
      ;; bit 48..63 is in limb 3 (bits 0..15)
      (let* ((z1 (vector-ref z 1))
             (z2 (vector-ref z 2))
             (z3 (vector-ref z 3))
             ;; shift30 limb 0 gets (z1 >> 14) | ((z2 & #x3fff) << 2)
             (s0 (fxior (fxarithmetic-shift-right z1 14)
                        (fxarithmetic-shift-left (fxand z2 #x3fff) 2)))
             ;; shift30 limb 1 gets (z2 >> 14) | ((z3 & #x3fff) << 2)
             (s1 (fxior (fxarithmetic-shift-right z2 14)
                        (fxarithmetic-shift-left (fxand z3 #x3fff) 2)))
             ;; shift30 limb 2 gets (z3 >> 14)
             (s2 (fxarithmetic-shift-right z3 14))
             (s3 0))
        (vector-set! z 0 (fxxor (vector-ref z 0) s0))
        (vector-set! z 1 (fxxor (vector-ref z 1) s1))
        (vector-set! z 2 (fxxor (vector-ref z 2) s2))
        (vector-set! z 3 (fxxor (vector-ref z 3) s3))))

    ;; 64-bit right-shift by 27 and XOR into z.
    (define (xor-shift-right-27! z)
      ;; z >> 27 = (z >> 16) >> 11:
      (let* ((z1 (vector-ref z 1))
             (z2 (vector-ref z 2))
             (z3 (vector-ref z 3))
             (s0 (fxior (fxarithmetic-shift-right z1 11)
                        (fxarithmetic-shift-left (fxand z2 #x7ff) 5)))
             (s1 (fxior (fxarithmetic-shift-right z2 11)
                        (fxarithmetic-shift-left (fxand z3 #x7ff) 5)))
             (s2 (fxarithmetic-shift-right z3 11))
             (s3 0))
        (vector-set! z 0 (fxxor (vector-ref z 0) s0))
        (vector-set! z 1 (fxxor (vector-ref z 1) s1))
        (vector-set! z 2 (fxxor (vector-ref z 2) s2))
        (vector-set! z 3 (fxxor (vector-ref z 3) s3))))

    ;; 64-bit right-shift by 31 and XOR into z
    (define (xor-shift-right-31! z)
      (let* ((z1 (vector-ref z 1))
             (z2 (vector-ref z 2))
             (z3 (vector-ref z 3))
             (s0 (fxior (fxarithmetic-shift-right z1 15)
                        (fxarithmetic-shift-left (fxand z2 #x7fff) 1)))
             (s1 (fxior (fxarithmetic-shift-right z2 15)
                        (fxarithmetic-shift-left (fxand z3 #x7fff) 1)))
             (s2 (fxarithmetic-shift-right z3 15))
             (s3 0))
        (vector-set! z 0 (fxxor (vector-ref z 0) s0))
        (vector-set! z 1 (fxxor (vector-ref z 1) s1))
        (vector-set! z 2 (fxxor (vector-ref z 2) s2))
        (vector-set! z 3 (fxxor (vector-ref z 3) s3))))

    ;; Stafford13 mixer applied to 4 16-bit limbs (top 64 bits of LCG):
    (define (mix-stafford13! z acc)
      (xor-shift-right-30! z)
      (mul64! z k1-limb0 k1-limb1 k1-limb2 k1-limb3 acc)
      (xor-shift-right-27! z)
      (mul64! z k2-limb0 k2-limb1 k2-limb2 k2-limb3 acc)
      (xor-shift-right-31! z))

    ;;------------------------------------------------------------------
    ;; make-random-128bits: Core 128-bit bytevector generator. Takes
    ;; an optional 16-byte bytevector seed or reads /dev/urandom.
    ;; ------------------------------------------------------------------

    (define make-random-128bits
      (case-lambda
        ((initial-seed)
         (let* ((seed-bv (cond
                           ((and (bytevector? initial-seed)
                                 (= (bytevector-length initial-seed)
                                    16))
                            (bytevector-copy initial-seed))
                           ((bytevector? initial-seed)
                            (let ((new-bv (urandom-bytevector-16))
                                  (len (min 16 (bytevector-length
                                                initial-seed))))
                              (bytevector-copy! new-bv 0
                                                initial-seed 0 len)
                              new-bv))
                           (else (urandom-bytevector-16))))
                (s (make-vector 8 0))
                (acc (make-vector 8 0))
                (out-bv (make-bytevector 16 0)))
           (bv->limbs seed-bv s)
           ;; Ensure state is odd for full 2^128 period
           (vector-set! s 0 (fxior (vector-ref s 0) 1))
           (lambda ()
             (step-lcg! s acc)
             (limbs->bv! s out-bv)
             (bytevector-copy out-bv))))
        (()
         (make-random-128bits (urandom-bytevector-16)))))

    ;;------------------------------------------------------------------
    ;; make-random-fixnum: Returns a generator of uniform non-negative
    ;; fixnums in [0, fx-greatest]. Applies Stafford13 mixing to the
    ;; high 64 bits and extracts the top bits required to fill the
    ;; host's fixnum width.
    ;; ------------------------------------------------------------------

    (define make-random-fixnum
      (case-lambda
        ((initial-seed)
         (let* ((s (make-vector 8 0))
                (acc (make-vector 8 0))
                (z (make-vector 4 0))
                (z-acc (make-vector 4 0))
                (seed-bv (if (bytevector? initial-seed)
                           initial-seed
                           (urandom-bytevector-16))))
           (bv->limbs (cond
                        ((and (bytevector? seed-bv)
                              (= (bytevector-length seed-bv) 16))
                         seed-bv)
                        (else (urandom-bytevector-16)))
                      s)
           (vector-set! s 0 (fxior (vector-ref s 0) 1))
           (lambda ()
             (step-lcg! s acc)
             ;; Copy top 64 bits (limbs 4..7) into z.
             (vector-set! z 0 (vector-ref s 4))
             (vector-set! z 1 (vector-ref s 5))
             (vector-set! z 2 (vector-ref s 6))
             (vector-set! z 3 (vector-ref s 7))
             ;; Scramble with Stafford13.
             (mix-stafford13! z z-acc)
             ;; Combine 16-bit limbs into non-negative fixnum,
             ;; masked by fx-greatest.
             (let loop ((i 3) (res 0))
               (if (fx<? i 0)
                 (fxand res fx-greatest)
                 (let* ((limb (vector-ref z i))
                        ;; Shift res left by 16 safely or accumulate.
                        (shifted (fxarithmetic-shift-left
                                  (fxand res (fxarithmetic-shift-right
                                              fx-greatest 16))
                                  16))
                        (next-res (fxior shifted limb)))
                   (loop (fx- i 1) next-res)))))))
        (()
         (make-random-fixnum (urandom-bytevector-16)))))

    ;;------------------------------------------------------------------
    ;; make-random-integer: Efficient bounded random integer in [0,
    ;; n-1] or [i, i + n - 1]. Uses pure fixnum arithmetic when n is a
    ;; fixnum. Employs unbiased rejection sampling when n is a fixnum.
    ;; Falls back to standard scaling if n exceeds fixnum range.
    ;; ------------------------------------------------------------------

    (define (make-random-integer random-fixnum)
      (let* ((randnum
              (lambda (n)
                (unless (and (integer? n) (positive? n))
                  (error "expected a positive integer" n))
                (if (and (fixnum? n) (fx<=? n fx-greatest))
                  ;; Pure fixnum unbiased rejection sampling
                  (let* ((limit (fx- fx-greatest
                                     (fxremainder fx-greatest n))))
                    (let loop ()
                      (let ((r (random-fixnum)))
                        (if (fx<? r limit)
                          (fxremainder r n)
                          (loop)))))
                  ;; Bignum fallback if bound exceeds fixnum size
                  (let* ((divisor (+ fx-greatest 1)))
                    (floor (* (/ (random-fixnum) divisor) n)))))))
        (case-lambda
          ((n)
           (randnum n))
          ((i n)
           (+ i (randnum n))))))

    ))

;;;---------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; coding: utf-8
;;; end:
