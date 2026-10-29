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

(import (scheme base)
        (scheme char)
        (scheme write)
        (scheme process-context)
        (calm1-lib utf32-string)
        (calm1-lib snobol-match))

(define (->utf32 s)
  (cond ((string? s) (string->utf32-string s))
        (else s)))

(define (string-contains? haystack needle)
  (let ((haystack (->utf32 haystack))
        (needle (->utf32 needle)))
    (let ((n-len (utf32-string-length needle))
          (h-len (utf32-string-length haystack)))
      (cond ((zero? n-len) #t)
            ((> n-len h-len) #f)
            (else
             (let loop ((i 0))
               (if (> (+ i n-len) h-len)
                 #f
                 (if (utf32-string=? needle
                                     (utf32-string-copy
                                      haystack i (+ i n-len)))
                   #t
                   (loop (+ i 1))))))))))

;; Core assertion tracking procedure.
;; Evaluates a thunk; if the return value doesn't match expected data, 
;; it aborts execution immediately with a diagnostic message and exit code 1.
(define (assert-equal? label expected actual)
  (if (not (equal? expected actual))
    (begin
      (display "FAIL: ") (display label) (newline)
      (display "  Expected: ") (write expected) (newline)
      (display "  Actual:   ") (write actual) (newline)
      (exit 1))))

;; Helper to extract a bound key's value directly out of a successful match
(define (get-var match-res key)
  (if (pair? match-res)
    (let ((pair (assoc key (cdr match-res))))
      (if pair (cdr pair) #f))
    #f))

;;; --- Comprehensive Edge-Case & Polymorphism Test Suite ---

(define (run-tests)

  ;; ====================================================================
  ;; 1. EMPTY STRING & ZERO-LENGTH MATCH EDGE CASES
  ;; ====================================================================
  
  (assert-equal? "p:lit empty string on empty input"
                 0
                 (car (snobol-match (p:lit "")
                                    (->utf32 ""))))

  (assert-equal? "p:lit empty string on populated input (non-consuming match)"
                 0
                 (car (snobol-match (p:lit "")
                                    (->utf32 "abcdef"))))

  (assert-equal? "p:arb on pure empty string input"
                 0
                 (car (snobol-match (p:arb)
                                    (->utf32 ""))))

  (assert-equal? "p:maybe-many wrapping an empty literal (prevents infinite loop)"
                 0
                 (car (snobol-match (p:maybe-many (p:lit ""))
                                    (->utf32 "abc"))))

  (assert-equal? "p:maybe-many when matching pattern is entirely absent"
                 0
                 (car (snobol-match (p:maybe-many (p:lit "xyz"))
                                    (->utf32 "abcdef"))))


  ;; ====================================================================
  ;; 2. BACKTRACKING BOUNDARY OVERLAPS & PEEL-BACKS
  ;; ====================================================================

  ;; 1. Anchored ARB Peel-back
  ;; Using p:pos forces p:arb to reject its initial 0-character match and seek the second one.
  (let ((overlap-pat (p:seq (p:arb) (p:lit "aba") (p:pos 5))))
    (assert-equal? "p:arb handles overlapping suffix when forced by a tail anchor"
                   5
                   (car (snobol-match overlap-pat "ababa"))))

  ;; 2. Atomic p:break execution
  ;; Verifies p:break matches cleanly up to the boundary without internal shifting.
  (let ((break-test (p:seq (p:assign-local (p:break "b") 'break-data) (p:lit "b"))))
    (assert-equal? "p:break matches up to the designated character delimiter atomically"
                   (->utf32 "xyzaa")
                   (get-var (snobol-match break-test "xyzaab") 'break-data)))

  ;; 1. p:span verification (Atomic non-backtracking check)
  ;; SPAN must grab "aaa" all at once. Because it is atomic in SPITBOL,
  ;; if a downstream literal asks for "a", it CANNOT step back to yield it.
  (let ((span-atomic-pat (p:seq (p:span "a") (p:lit "a"))))
    (assert-equal? "p:span is atomic and rejects character-by-character backoffs"
                   #f
                   (snobol-match span-atomic-pat "aaaa")))

  ;; 2. p:breakx verification (Multi-delimiter jumping check)
  ;; Input: "xyzaab"
  ;; First stop: index 5 ("xyzaa"), then downstream (LIT "a") fails on "b".
  ;; BREAKX must retry by skipping past the first 'b' to look for a second 'b'.
  ;; In "xyzaab", there is no second 'b', so the match fails.
  (let ((breakx-fail-pat (p:seq (p:assign-local (p:breakx "b") 'break-data) 
                                (p:lit "a") 
                                (p:lit "b"))))
    (assert-equal? "p:breakx fails if there is no secondary delimiter match downstream"
                   #f
                   (snobol-match breakx-fail-pat "xyzaab")))

  ;; 3. p:breakx successful jump verification
  ;;
  ;; Input: "xyzab_and_abc"
  ;;
  ;; First stop: index 4 (before first 'b'). Downstream looks for
  ;; "bc". Index 4 is "b_", fails!
  ;;
  ;; Second stop: leaps to index 11 (before second 'b'). Downstream
  ;; looks for "bc". Index 11 is "bc". Success!
  ;;
  (let ((breakx-jump-pat (p:seq (p:assign-local (p:breakx "b") 'captured-prefix)
                                (p:lit "bc"))))
    (assert-equal? "p:breakx successfully jumps to the secondary delimiter branch"
                   13
                   (car (snobol-match breakx-jump-pat "xyzab_and_abc")))
    (assert-equal? "p:breakx variable capture tracking matches up to the final jump point"
                   (->utf32 "xyzab_and_a")
                   (get-var (snobol-match breakx-jump-pat "xyzab_and_abc") 'captured-prefix)))

  ;; ====================================================================
  ;; SPITBOL ARBNO BACKTRACKING SUITE
  ;; ====================================================================

  ;; 1. ARBNO minimal evaluation check
  ;; Input: "abc". Pattern: ARBNO("a") followed by ARB.
  ;; Because ARBNO is shortest-match-first, it should match 0 characters ("").
  ;; Then ARB consumes the rest ("abc"). Total index match is 3.
  (let ((arbno-min (p:seq (p:assign-local (p:arbno (p:lit "a")) 'seq-data) 
                          (p:arb))))
    (assert-equal? "p:arbno initially prioritizes matching zero characters"
                   (->utf32 "")
                   (get-var (snobol-match arbno-min "abc") 'seq-data)))

  ;; 2. ARBNO expansion check via downstream anchoring
  ;; Input: "aaab". Pattern: ARBNO("a") followed by "b".
  ;; The literal "b" forces ARBNO to expand step-by-step until it matches all three "a"s.
  (let ((arbno-expand (p:seq (p:assign-local (p:arbno (p:lit "a")) 'seq-data) 
                             (p:lit "b"))))
    (assert-equal? "p:arbno expands iteratively when forced by downstream patterns"
                   (->utf32 "aaa")
                   (get-var (snobol-match arbno-expand "aaab") 'seq-data)))

  ;; 3. ARBNO infinite loop prevention check
  ;; If passed an empty matching literal, ARBNO should safely exit loop generation.
  (let ((arbno-empty (p:seq (p:arbno (p:lit "")) (p:lit "xyz"))))
    (assert-equal? "p:arbno safely breaks execution if the pattern consumes nothing"
                   3
                   (car (snobol-match arbno-empty "xyz"))))

  ;; ====================================================================
  ;; 3. POSITIONAL, BOUNDARY, AND REJECTION CORNERS
  ;; ====================================================================

  (assert-equal? "p:len requesting more characters than string length"
                 #f
                 (snobol-match (p:len 10) "abc"))

  (assert-equal? "p:tab moving backwards absolute index (must fail)"
                 #f
                 (snobol-match (p:seq (p:len 5) (p:tab 2)) "abcdefgh"))

  (assert-equal? "p:rtab requesting more remaining characters than possible"
                 #f
                 (snobol-match (p:seq (p:len 3) (p:rtab 6)) "abcdef"))

  (assert-equal? "p:pos enforcing anchor assertion in middle of sequence"
                 #f
                 (snobol-match (p:seq (p:len 2) (p:pos 5)) "abcdef"))


  ;; ====================================================================
  ;; 4. COMPLEX BALANCED BRACKET BAL MISMATCHES
  ;; ====================================================================

  (assert-equal? "p:bal on raw text without any brackets (valid balanced case)"
                 5
                 (car (snobol-match (p:bal) "hello")))

  (assert-equal? "p:bal starts on a close bracket (must immediately fail)"
                 #f
                 (snobol-match (p:bal) "]abc"))

  (assert-equal? "p:bal with mismatched internal alternative brackets"
                 #f
                 (snobol-match (p:bal) "[a(b]c)"))

  (assert-equal? "p:bal matching empty paired brackets (e.g. '{}' or '[]')"
                 2
                 (car (snobol-match (p:bal) "[]")))

  (assert-equal? "p:bal deeply nested multi-bracket sequence evaluation"
                 11
                 (car (snobol-match (p:bal) "{a:[b(c)d]}")))


  ;; ====================================================================
  ;; 5. UNEVALUATED EXPRESSIONS (*expr) AND ENVIRONMENT RECOVERY
  ;; ====================================================================

  ;; Pattern tries capturing an ARB block into 'v, then matching 'v, then matching "X".
  ;; If "X" fails, it must drop out, peel back ARB, re-bind 'v, and try again.
  (let ((deep-env-pat (p:seq (p:assign-local (p:arb) 'v) (p:expr 'v) (p:lit "X"))))
    (assert-equal? "Thread-safe env correctly rolls back bad bindings during retry loops"
                   7
                   (car (snobol-match deep-env-pat "abcabcX"))))


  ;; ====================================================================
  ;; 6. REPLACEMENT COMPILATION CORNER CASES
  ;; ====================================================================

  (assert-equal? "snobol-replace returns #f gracefully on failed match input"
                 #f
                 (snobol-replace #f '("literal")))

  (let ((empty-match (snobol-match (p:lit "") "test")))
    (assert-equal? "snobol-replace with empty recipe list on valid match"
                   (->utf32 "")
                   (snobol-replace empty-match '())))

  (let* ((p (p:seq (p:assign-local (p:lit "foo") 'k)))
         (res (snobol-match p "foobar")))
    (assert-equal? "snobol-replace handling unregistered/missing lookup keys gracefully"
                   (->utf32 "foo-missing-")
                   (snobol-replace res '(k "-missing-" missing_key))))


  ;; ====================================================================
  ;; 7. SINGLE UNLISTED PREDICATE POLYMORPHISM (PROCEDURE? DETECTION)
  ;; ====================================================================

  ;; Testing p:any-of with a single standard R7RS procedure argument
  (let ((single-pred-any (p:seq (p:any-of char-alphabetic?) (p:lit "123"))))
    (assert-equal? "p:any-of with single unlisted procedure (valid match)"
                   4
                   (car (snobol-match single-pred-any "a123")))
    (assert-equal? "p:any-of with single unlisted procedure (rejection case)"
                   #f
                   (snobol-match single-pred-any "1123")))

  ;; Testing p:none-of with a single standalone procedure argument
  (let ((single-pred-none (p:seq (p:none-of char-whitespace?) (p:lit "xyz"))))
    (assert-equal? "p:none-of with single unlisted procedure (valid match)"
                   4
                   (car (snobol-match single-pred-none "bxyz")))
    (assert-equal? "p:none-of with single unlisted procedure (rejection case)"
                   #f
                   (snobol-match single-pred-none " xyz")))

  ;; Testing p:any-of with a custom user-defined anonymous lambda predicate
  (let* ((vowel? (lambda (c) (string-contains? "aeiouAEIOU" (string c))))
         (custom-pred-pat (p:seq (p:any-of vowel?) (p:lit "!"))))
    (assert-equal? "p:any-of with custom user-defined lambda predicate"
                   2
                   (car (snobol-match custom-pred-pat "e!"))))

  ;; ====================================================================
  ;; 8. SPITBOL IMMEDIATE ASSIGNMENT ($) SUITE
  ;; ====================================================================

  ;; 1. Immediate capture verification
  ;; Input: "123lbs". Pattern: SPAN(digits) immediately assigned to 'weight, followed by "lbs"
  (let ((imm-match (p:seq (p:immediate-assign (p:span "0123456789") 'weight)
                          (p:lit "lbs"))))
    (assert-equal? "p:immediate-assign captures text segments on successful sub-paths"
                   (->utf32 "123")
                   (get-var (snobol-match imm-match "123lbs") 'weight)))

  ;; 2. Tracking intermediate state inside recursive branches
  ;; Even if a pattern backtracks later, the environment snapshot captures 
  ;; what was matched at that exact moment.
  (let* ((vowel? (lambda (c) (string-contains? "aeiou" (string c))))
         (imm-track (p:seq (p:immediate-assign (p:any-of vowel?) 'first-vowel)
                           (p:lit "z"))))
    ;; Input: "az". Matches 'a', sets 'first-vowel to "a", matches "z". Success!
    (assert-equal? "p:immediate-assign logs state correctly before downstream execution"
                   (->utf32 "a")
                   (get-var (snobol-match imm-track "az") 'first-vowel)))

  ;; ====================================================================
  ;; 9. SPITBOL CURSOR POSITION ASSIGNMENT (@) SUITE
  ;; ====================================================================

  ;; 1. Standard index tracking
  ;; Input: "abcdef". Pattern: Skip 3 chars, log position 'pos, match "def"
  (let ((cursor-pat (p:seq (p:len 3) 
                           (p:cursor 'pos) 
                           (p:lit "def"))))
    (assert-equal? "p:cursor logs the precise numeric match index position"
                   (->utf32 "3")
                   (get-var (snobol-match cursor-pat "abcdef") 'pos)))

  ;; 2. Bracketing a matched slice with two cursors
  ;; Input: "   target   "
  ;; Pattern: BREAK("t") -> @start -> SPAN("target") -> @end
  (let ((span-bound-pat (p:seq (p:break "t")
                               (p:cursor 'start-idx)
                               (p:span "atgner")
                               (p:cursor 'end-idx))))
    (let ((res (snobol-match span-bound-pat "   target   ")))
      (assert-equal? "p:cursor tracks starting index boundary cleanly"
                     (->utf32 "3")
                     (get-var res 'start-idx))
      (assert-equal? "p:cursor tracks ending index boundary cleanly"
                     (->utf32 "9")
                     (get-var res 'end-idx))))

  ;; ====================================================================
  ;; 10. SPITBOL BRANCH TRUNCATION & COMPACTION SUITE
  ;; ====================================================================

 ;; 1. ABORT execution roadblock check
  ;; Alternation tries to match "xyz" and succeeds, hitting ABORT. 
  ;; Even though "xyz" is also available as a fallback alternative, 
  ;; ABORT terminates the entire match attempt, returning #f.
  (let ((abort-pat (p:alt (p:seq (p:lit "xyz") (p:abort)) 
                          (p:lit "xyz"))))
    (assert-equal? "p:abort immediately halts all evaluation paths"
                   #f
                   (snobol-match abort-pat "xyz")))

  ;; 2. FENCE backtrack prevention check
  ;; Input: "abcdef". Sequence matches "abc", crosses FENCE, then tries to match "xyz".
  ;; "xyz" fails. Downstream logic wants to backtrack into "abc", but FENCE drops the line.
  (let ((fence-pat (p:seq (p:lit "abc") (p:fence) (p:lit "xyz"))))
    (assert-equal? "p:fence permits forward progress but isolates backtracking paths"
                   #f
                   (snobol-match fence-pat "abcdef")))

  ;; 3. Environment Compaction Check
  ;; Input: "123". We trace a variable 'x being assigned at index 1, 2, and 3 consecutively.
  (let ((dupe-pat (p:seq (p:assign-local (p:len 1) 'x)
                         (p:assign-local (p:len 1) 'x)
                         (p:assign-local (p:len 1) 'x))))
    (let ((res (snobol-match dupe-pat "123")))
      ;; The final structure should contain exactly one unique instance of 'x mapping to "3"
      (assert-equal? "Environment compaction retains only the final written state"
                     (->utf32 "3")
                     (get-var res 'x))
      (assert-equal? "Environment contains no duplicate key definitions after compaction"
                     1
                     (length (cdr res)))))

  ;; If execution drops out of the bottom smoothly, everything is verified.
  (exit 0))

(run-tests)
