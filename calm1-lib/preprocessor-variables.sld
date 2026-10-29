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
;;;
;;; An implementation of variables for the preprocessor.
;;;
;;; This implementation has to be in a library, so it can be imported
;;; into an (eval) environment.
;;;
;;;
;;;---------------------------------------------------------------------

(define-library (calm1-lib preprocessor-variables)

  (export define-preprocessor-variable)

  (export :=)

  (export $a)
  (export $b)
  (export $c)
  (export $d)
  (export $e)
  (export $f)
  (export $g)
  (export $h)
  (export $i)
  (export $j)
  (export $k)
  (export $l)
  (export $m)
  (export $n)
  (export $o)
  (export $p)
  (export $q)
  (export $r)
  (export $s)
  (export $t)
  (export $u)
  (export $v)
  (export $w)
  (export $x)
  (export $y)
  (export $z)

  (export $A)
  (export $B)
  (export $C)
  (export $D)
  (export $E)
  (export $F)
  (export $G)
  (export $H)
  (export $I)
  (export $J)
  (export $K)
  (export $L)
  (export $M)
  (export $N)
  (export $O)
  (export $P)
  (export $Q)
  (export $R)
  (export $S)
  (export $T)
  (export $U)
  (export $V)
  (export $W)
  (export $X)
  (export $Y)
  (export $Z)

  (import (scheme base)
          (scheme case-lambda))

  (begin

    (define := '#(unique-object))

    (define-syntax define-preprocessor-variable
      (syntax-rules ()
        ((¶ name)
         (define name
           ;; The storage has to be mutable while leaving the
           ;; environment immutable.
           (let ((p (list #f)))
             (case-lambda
               (() (car p))
               ((symb value)
                (unless (eq? symb :=)
                  (error "expected the unique object :=" symb))
                (set-car! p value))))))))

    (define-preprocessor-variable $a)
    (define-preprocessor-variable $b)
    (define-preprocessor-variable $c)
    (define-preprocessor-variable $d)
    (define-preprocessor-variable $e)
    (define-preprocessor-variable $f)
    (define-preprocessor-variable $g)
    (define-preprocessor-variable $h)
    (define-preprocessor-variable $i)
    (define-preprocessor-variable $j)
    (define-preprocessor-variable $k)
    (define-preprocessor-variable $l)
    (define-preprocessor-variable $m)
    (define-preprocessor-variable $n)
    (define-preprocessor-variable $o)
    (define-preprocessor-variable $p)
    (define-preprocessor-variable $q)
    (define-preprocessor-variable $r)
    (define-preprocessor-variable $s)
    (define-preprocessor-variable $t)
    (define-preprocessor-variable $u)
    (define-preprocessor-variable $v)
    (define-preprocessor-variable $w)
    (define-preprocessor-variable $x)
    (define-preprocessor-variable $y)
    (define-preprocessor-variable $z)

    (define-preprocessor-variable $A)
    (define-preprocessor-variable $B)
    (define-preprocessor-variable $C)
    (define-preprocessor-variable $D)
    (define-preprocessor-variable $E)
    (define-preprocessor-variable $F)
    (define-preprocessor-variable $G)
    (define-preprocessor-variable $H)
    (define-preprocessor-variable $I)
    (define-preprocessor-variable $J)
    (define-preprocessor-variable $K)
    (define-preprocessor-variable $L)
    (define-preprocessor-variable $M)
    (define-preprocessor-variable $N)
    (define-preprocessor-variable $O)
    (define-preprocessor-variable $P)
    (define-preprocessor-variable $Q)
    (define-preprocessor-variable $R)
    (define-preprocessor-variable $S)
    (define-preprocessor-variable $T)
    (define-preprocessor-variable $U)
    (define-preprocessor-variable $V)
    (define-preprocessor-variable $W)
    (define-preprocessor-variable $X)
    (define-preprocessor-variable $Y)
    (define-preprocessor-variable $Z)

    ))

;;;---------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; coding: utf-8
;;; end:
