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
    (test "7" (run/string (calm1) (<< "(@ eval (+ 4 3))")))
    (test "567" (run/string
                 (calm1)
                 (<< "(@ eval (begin ($x := 4) 5)\
                      (begin ($y := 3) 6)(+ ($x) ($y)))")))
    (test "7" (run/string
               (calm1) (<< "(@ hide ($a := (+ 4 3)))(@ eval ($a))")))
    (test "7" (run/string
               (calm1)
               (<< "(@ hide ($x := 4)($y := 3)\
                            (@in (+ ($x) ($y))))")))
    (test "7" (run/string
               (calm1)
               (<< "(@ dnl ($a := (+ 4 3)))discard\n(@ eval ($a))")))
    (test "567" (run/string
                 (calm1)
                 (<< "(@ dnl (@out \"56\")\
                      ($a := (+ 4 3)))discard\n\
                      (@ eval ($a))")))
    )

  (test-group "when, unless"
    (test "1234" (run/string
                  (calm1)
                  (<< "(@ when #t (number->string 1) \
                                   \"23\" \"4\")")))
    (test "" (run/string
              (calm1)
              (<< "(@ when #f (number->string 1) \
                               \"23\" \"4\")")))
    (test "1234" (run/string
                  (calm1)
                  (<< "(@ unless #f (number->string 1) \
                                     \"23\" \"4\")")))
    (test "" (run/string
              (calm1)
              (<< "(@ unless #t (number->string 1) \
                                 \"23\" \"4\")")))
    )

  (test-group "if, while"
    (test "3" (run/string
               (calm1)
               (<< "(@ hide ($A := 3))\
                    (@ if (and (= ($A) 1) (@in \"1\"))\
                          (and (= ($A) 2) (@in \"2\"))\
                          (and (= ($A) 3) (@in \"3\"))\
                          (and (= ($A) 4) (@in \"4\")))")))
    (test "2222211111=>1111122222"
          (run/string
           (calm1)
           (<< "(@ hide ($A := 1)($S := \"\"))\
                (@ while (and (<= 1 ($A)) (<= ($A) 5) \
                              (begin \
                                (@in 1) \
                                ($S := (string-append ($S) \"1\")) \
                                ($A := (+ ($A) 1)) #t)) \
                         (and (<= 6 ($A)) (<= ($A) 10) \
                              (begin \
                                (@in 2) \
                                ($S := (string-append ($S) \"2\")) \
                                ($A := (+ ($A) 1)) #t)))=>\
                (@ hide (@in ($S)))")))
    )

  (test-group "include-raw, include"
    (let ((text (run/string
                 (calm1) (<< "(@ include-raw \"run.scm\")")))
          (nominal (run/string (cat run.scm))))
      (test-assert "include-raw"(string=? text nominal)))
    (test "include"
          "7" (run/string
               (calm1) (<< "(@ include \"inclusion.txt\")")))
    )

  (test-group "define, @defn, undefine, pushdef, popdef"
    (test "(+ 4 3)"
          (run/string
           (calm1)
           (<< "(@ define \"add-three\" \"(+ (@ 1) 3)\")\
                (@ add-three 4)")))
    (test "i-am-called me or myself"
          (run/string
           (calm1)
           (<< "(@ define \"i-am-called\" \"(@ 0) (@ 1) (@ 2) (@ 3)\")\
                (@ i-am-called \"me\" \"or\" \"myself\")")))
    (test "7(@ eval (+ 3 4))(@ hide (+ 3 4))(@ 0)(@ 1)(@ 2)(@ 3)"
          (run/string
           (calm1)
           (<< "(@ define \"EVAL\" (@defn \"eval\"))\
                (@ undefine \"eval\" \"hide\")\
                (@ EVAL (+ 3 4))(@ eval (+ 3 4))(@ hide (+ 3 4))\
                (@ 0)(@ 1)(@ 2)(@ 3)")))
    (test "1234(@ one)(@ two)(@ three)(@ four)"
          (run/string
           (calm1)
           (<< "(@ pushdef \"one\" \"1\")\
                (@ pushdef \"two\" \"2\")\
                (@ pushdef \"three\" \"3\")\
                (@ pushdef \"four\" \"4\")\
                (@ one)(@ two)(@ three)(@ four)\
                (@ popdef \"one\" \"two\" \"three\" \"four\")\
                (@ one)(@ two)(@ three)(@ four)")))
    )

  )

(test-exit)
