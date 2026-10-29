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
        (scheme write)
        (test)
        (scsh-process))

(test-group "everything"

  (test-group "eval, hide, dnl"
    (test "7" (run/string (calm1 -) (<< "(@ eval (+ 4 3))")))
    (test "567" (run/string
                 (calm1 -)
                 (<< "(@ eval (begin ($x := 4) 5)\
                      (begin ($y := 3) 6)(+ ($x) ($y)))")))
    (test "7" (run/string
               (calm1 -) (<< "(@ hide ($a := (+ 4 3)))(@ eval ($a))")))
    (test "7" (run/string
               (calm1 -)
               (<< "(@ hide ($x := 4)($y := 3)\
                    (display (+ ($x) ($y))))")))
    (test "7" (run/string
               (calm1 -)
               (<< "(@ dnl ($a := (+ 4 3)))discard\n(@ eval ($a))")))
    (test "567" (run/string
                 (calm1 -)
                 (<< "(@ dnl (display \"56\")($a := (+ 4 3)))discard\n\
                    (@ eval ($a))")))
    )

  (test-group "when, unless"
    (test "1234" (run/string
                  (calm1 -)
                  (<< "(@ when #t (number->string 1) \"23\" \"4\")")))
    (test "" (run/string
              (calm1 -)
              (<< "(@ when #f (number->string 1) \"23\" \"4\")")))
    (test "1234" (run/string
                  (calm1 -)
                  (<< "(@ unless #f (number->string 1) \"23\" \"4\")")))
    (test "" (run/string
              (calm1 -)
              (<< "(@ unless #t (number->string 1) \"23\" \"4\")")))
    )

  (test-group "if, while"
    (test "3" (run/string
               (calm1 -)
               (<< "(@ hide ($A := 3))\
                    (@ if (and (= ($A) 1) \"1\")\
                          (and (= ($A) 2) \"2\")\
                          (and (= ($A) 3) \"3\")\
                          (and (= ($A) 4) \"4\"))")))
    (test "1111122222"
          (run/string
           (calm1 -)
           (<< "(@ hide ($A := 1))\
                (@ while (and (<= 1 ($A)) (<= ($A) 5) \
                              (begin ($A := (+ ($A) 1)) 1)) \
                         (and (<= 6 ($A)) (<= ($A) 10) \
                              (begin ($A := (+ ($A) 1)) 2)))")))
    )
  
  )

(test-exit)
