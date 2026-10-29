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

(define-library (calm1-lib snobol-match)

  (import (scheme base)
          (scheme char)
          (scheme case-lambda)
          (calm1-lib utf32-string))
  (cond-expand
    ((library (srfi 1)) (import (srfi 1)))
    ((library (scheme list)) (import (scheme list)))
    (loko (import (srfi :1 lists)))
    (else (import (srfi srfi-1))))

  (export bracket-pairs
          set-bracket-pairs!
          localize-bracket-pairs)
         
  (export snobol-match
          snobol-replace
          p:assign-local
          p:immediate-assign
          p:cursor
          p:lit
          p:seq
          p:alt
          p:len
          p:arb
          p:span
          p:break
          p:breakx
          p:arbno
          p:expr
          p:pred
          p:any
          p:notany
          p:any-of
          p:none-of
          p:many
          p:maybe-many
          p:fence
          p:abort
          p:pos
          p:bal
          p:tab
          p:rtab
          p:rem)

  (begin

    (define rassoc
      (case-lambda
        ((obj alist)
         (rassoc obj alist equal?))
        ((obj alist pred)
         (let loop ((ls alist))
           (cond ((null? ls) #f)
                 ((pred obj (cdr (car ls))) (car ls))
                 (else (loop (cdr ls))))))))

    (define (->utf32 s)
      (cond ((string? s) (string->utf32-string s))
            (else s)))

    ;;------------------------------------------------------------------
    ;;
    ;; Runner and environment environment lookups.
    ;;

    ;; Tail-recursive environment filter to keep only the newest entry
    ;; for each unique key.
    (define (compact-env env)
      (let loop ((rem env)
                 (seen '())
                 (acc '()))
        (cond ((null? rem) 
               acc)
              ((member (caar rem) seen) 
               (loop (cdr rem) seen acc))
              (else 
               (loop (cdr rem) 
                     (cons (caar rem) seen) 
                     (cons (car rem) acc))))))

    ;; Top-level engine driver. Evaluates a pattern against a target
    ;; string initializing an empty env. Returns a pair of
    ;; (final-index . environment-alist) on success, or #f.
    (define (snobol-match pat str)
      (let ((str (->utf32 str)))
        (pat str 0 '()
             (lambda (final-idx env retry) 
               (cons final-idx (compact-env env))) 
             (lambda () #f))))

    ;; Captures a matched text slice into a thread-safe lexical
    ;; context. Passes the updated environment map down the
    ;; continuation pipeline.
    (define (p:assign-local pat key)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (pat str idx env
               (lambda (next-idx next-env retry-fail)
                 (let* ((slice (utf32-string-copy str idx next-idx))
                        (updated-env (cons (cons key slice) next-env)))
                   (succeed next-idx updated-env retry-fail)))
               fail))))

    ;; SPITBOL $ Operator: Immediate Assignment. Extracts the slice
    ;; and updates the lexical environment instantly when 'pat'
    ;; succeeds, passing that state forward into subsequent
    ;; backtracking alternatives.
    (define (p:immediate-assign pat key)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (pat str idx env
               (lambda (next-idx next-env retry-fail)
                 (let* ((slice (utf32-string-copy str idx next-idx))
                        (updated-env (cons (cons key slice) next-env)))
                   ;; Pass the updated env forward, keeping the
                   ;; immediate capture locked into the environment
                   ;; timeline even on subsequent downstream retries.
                   (succeed next-idx updated-env retry-fail)))
               fail))))

    ;; SPITBOL @ Operator: Cursor Position Assignment. Non-consuming
    ;; primitive that instantly binds the current cursor position
    ;; index to 'key' within the environment alist, then passes
    ;; control forward.
    (define (p:cursor key)
      (let ((key (->utf32 key)))
        (lambda (str idx env succeed fail)
          (let ((str (->utf32 str)))
            (let* ((pos-str (number->utf32-string idx))
                   (updated-env (cons (cons key pos-str) env)))
              ;; Instantly succeed without advancing the cursor index
              ;; (idx remains unchanged)
              (succeed idx updated-env fail))))))

    ;; Extracts bound variable data out of the current stack context.
    (define (env-lookup env key default)
      (let ((pair (assoc key env)))
        (if pair (cdr pair) default)))

    ;; SNOBOL * Operator: Unevaluated expression reference.
    ;; Dynamically resolves a key's bound value at the moment of
    ;; evaluation.
    (define (p:expr key)
      (let ((key (->utf32 key)))
        (lambda (str idx env succeed fail)
          (let ((str (->utf32 str)))
            (let* ((val (env-lookup env key ""))
                   (lit-pat (p:lit val)))
              (lit-pat str idx env succeed fail))))))

    ;; Formats a single recipe element based on whether it is a key or
    ;; literal.
    (define (render-recipe-token token env)
      (if (symbol? token)
        (env-lookup env token "")
        token))

    ;; Assembles the replacement tokens into a final string wrapper
    (define (snobol-replace match-result recipe)
      (if (not match-result)
        #f ; Match failed, return #f to signal no substitution occurred
        (let* ((final-idx (car match-result))
               (env       (cdr match-result)))
          (let loop ((rem-recipe recipe)
                     (acc '()))
            (if (null? rem-recipe)
              (utf32-string-concatenate (map! ->utf32 (reverse! acc)))
              (let* ((token (car rem-recipe))
                     (chunk (render-recipe-token token env)))
                (loop (cdr rem-recipe) (cons chunk acc))))))))

    ;;------------------------------------------------------------------
    ;;
    ;; Structural combinators.
    ;;

    ;; Literal Matcher: Verifies exact substring presentation at
    ;; index.
    (define (p:lit lit)
      (let ((lit (->utf32 lit)))
        (let ((lit-len (utf32-string-length lit)))
          (lambda (str idx env succeed fail)
            (let ((str (->utf32 str)))
              (let ((end (+ idx lit-len)))
                (if (and (<= end (utf32-string-length str))
                         (utf32-string=? lit (utf32-string-copy
                                              str idx end)))
                  (succeed end env fail)
                  (fail))))))))

    ;; Variadic Sequence: Chains an arbitrary list of patterns
    ;; consecutively. Includes single-argument routing to prevent
    ;; crash loops.
    (define p:seq
      (case-lambda
        (() 
         (lambda (str idx env succeed fail)
           (succeed idx env fail)))
        ((p1) 
         p1) ;; Gracefully return the single pattern directly
        ((p1 p2)
         (lambda (str idx env succeed fail)
           (let ((str (->utf32 str)))
             (p1 str idx env
                 (lambda (next-idx next-env retry-fail)
                   (p2 str next-idx next-env succeed retry-fail))
                 fail))))
        (patterns
         (lambda (str idx env succeed fail)
           (let ((str (->utf32 str)))
             ((car patterns) str idx env
              (lambda (next-idx next-env retry-fail)
                ((apply p:seq (cdr patterns))
                 str next-idx next-env succeed retry-fail))
              fail))))))

    ;; Variadic Alternation: Tries alternative patterns down the list
    ;; sequentially.
    (define p:alt
      (case-lambda
        (() 
         (lambda (str idx env succeed fail)
           (fail)))
        ((p1) 
         p1) ;; Gracefully return the single pattern directly
        ((p1 p2)
         (lambda (str idx env succeed fail)
           (let ((str (->utf32 str)))
             (p1 str idx env succeed 
                 (lambda ()
                   (p2 str idx env succeed fail))))))
        (patterns
         (lambda (str idx env succeed fail)
           (let ((str (->utf32 str)))
             ((car patterns) str idx env succeed 
              (lambda () 
                ((apply p:alt (cdr patterns))
                 str idx env succeed fail))))))))

    ;;------------------------------------------------------------------
    ;;
    ;; Variable and arbitrary string consumers.
    ;;

    ;; Finds the furthest index matching the criteria using a string
    ;; character set.
    (define (span-forward-str str idx char-set-str len)
      (let ((str (->utf32 str)))
        (if (and (< idx len)
                 (char-in-string? char-set-str
                                  (utf32-string-ref str idx)))
          (span-forward-str str (+ idx 1) char-set-str len)
          idx)))

    ;; Checks if a character matches any predicate in a list.
    (define (matches-any-pred? char preds)
      (let loop ((p-list preds))
        (cond ((null? p-list) #f)
              (((car p-list) char) #t)
              (else (loop (cdr p-list))))))

    ;; Finds the furthest index matching the criteria using a list of
    ;; predicates.
    (define (span-forward-preds str idx preds len)
      (let ((str (->utf32 str)))
        (if (and (< idx len)
                 (matches-any-pred? (utf32-string-ref str idx) preds))
          (span-forward-preds str (+ idx 1) preds len)
          idx)))

    ;; Linearly scales down the matched index when subsequent paths fail.
    (define (span-backtrack str start-idx current-idx env succeed fail)
      (let ((str (->utf32 str)))
        (if (>= current-idx start-idx)
          (succeed current-idx env 
                   (lambda () 
                     (span-backtrack str start-idx (- current-idx 1)
                                     env succeed fail)))
          (fail))))

    (define (span-aux preds)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (let* ((len (utf32-string-length str))
                 (max-end (span-forward-preds str idx preds len)))
            (if (= max-end idx)
              (fail)
              (span-backtrack str (+ idx 1) max-end
                              env succeed fail))))))

    ;; Scans forward until it finds a character inside the
    ;; character-set string.
    (define (break-forward-str str idx char-set-str len)
      (let ((str (->utf32 str)))
        (if (and (< idx len)
                 (not (char-in-string? char-set-str
                                       (utf32-string-ref str idx))))
          (break-forward-str str (+ idx 1) char-set-str len)
          idx)))

    ;; Scans forward until it finds a character satisfying any of the
    ;; predicates.
    (define (break-forward-preds str idx preds len)
      (let ((str (->utf32 str)))
        (if (and (< idx len)
                 (not (matches-any-pred? (utf32-string-ref str idx) preds)))
          (break-forward-preds str (+ idx 1) preds len)
          idx)))

    ;; Drops the cursor backward one character at a time on downstream
    ;; failure.
    (define (break-backtrack str start-idx current-idx env succeed fail)
      (let ((str (->utf32 str)))
        (if (>= current-idx start-idx)
          (succeed current-idx env 
                   (lambda () 
                     (break-backtrack str start-idx (- current-idx 1) env
                                      succeed fail)))
          (fail))))

    (define (break-aux preds)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (let* ((len (utf32-string-length str))
                 (max-end (break-forward-preds str idx preds len)))
            (if (= max-end idx)
              (fail)
              (break-backtrack str (+ idx 1) max-end env succeed fail))))))

    ;; SNOBOL LEN(n): Consumes a fixed chunk of characters safely.
    (define (p:len n)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (let ((end (+ idx n)))
            (if (<= end (utf32-string-length str))
              (succeed end env fail)
              (fail))))))

    ;; SNOBOL ARB: Corrected to match minimally (0 characters initially)
    (define (arb-loop str idx env n succeed fail)
      (let ((str (->utf32 str)))
        (let ((end (+ idx n)))
          (if (<= end (utf32-string-length str))
            (succeed end env (lambda () (arb-loop str idx env (+ n 1)
                                                  succeed fail)))
            (fail)))))

    (define (p:arb)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (arb-loop str idx env 0 succeed fail))))

    ;; SNOBOL BREAK: An atomic, non-backtracking choice point. Once it
    ;; scans up to the breakpoint, it passes that fixed slice forward.
    (define p:break
      (case-lambda
        ((char-set-str)
         (let ((char-set-str (->utf32 char-set-str)))
           (lambda (str idx env succeed fail)
             (let ((str (->utf32 str)))
               (let* ((len (utf32-string-length str))
                      (max-end (break-forward-str
                                str idx char-set-str len)))
                 (if (< idx max-end)
                   ;; Pure SNOBOL: no backtrack wrapper here.
                   (succeed max-end env fail)
                   (fail)))))))
         (preds
          (lambda (str idx env succeed fail)
            (let ((str (->utf32 str)))
              (let* ((len (utf32-string-length str))
                     (max-end (break-forward-preds str idx preds len)))
                (if (< idx max-end)
                  (succeed max-end env fail)
                  (fail))))))))

    ;; SNOBOL SPAN: Similarly atomic and non-backtracking.
    (define p:span
      (case-lambda
        ((char-set-str)
         (let ((char-set-str (->utf32 char-set-str)))
           (lambda (str idx env succeed fail)
             (let ((str (->utf32 str)))
               (let* ((len (utf32-string-length str))
                      (max-end (span-forward-str
                                str idx char-set-str len)))
                 (if (< idx max-end)
                   (succeed max-end env fail)
                   (fail)))))))
        (preds
         (lambda (str idx env succeed fail)
           (let ((str (->utf32 str)))
             (let* ((len (utf32-string-length str))
                    (max-end (span-forward-preds str idx preds len)))
               (if (< idx max-end)
                 (succeed max-end env fail)
                 (fail))))))))

    ;; Internal tracking loop that jumps precisely from one delimiter
    ;; landmark to the next.
    (define (breakx-loop str start-idx current-delimiter-idx
                         char-set-str env succeed fail)
      (let ((str (->utf32 str)))
        (let ((len (utf32-string-length str)))
          (succeed current-delimiter-idx env
                   (lambda ()
                     ;; If downstream fails, scan for the NEXT
                     ;; occurrence strictly past this one.
                     (if (< current-delimiter-idx len)
                       (let ((next-end (break-forward-str
                                        str (+ current-delimiter-idx 1)
                                        char-set-str len)))
                         (if (<= next-end len)
                           ;; If another delimiter is found, pivot the
                           ;; retry straight to it.
                           (breakx-loop str start-idx next-end
                                        char-set-str env succeed fail)
                           (fail)))
                       (fail)))))))

    ;; SPITBOL BREAKX: Moves to the first delimiter boundary, and
    ;; leaps to subsequent delimiter positions on downstream failures.
    (define (p:breakx char-set-str)
      (let ((char-set-str (->utf32 char-set-str)))
        (lambda (str idx env succeed fail)
          (let ((str (->utf32 str)))
            (let* ((len (utf32-string-length str))
                   (first-end (break-forward-str str idx
                                                 char-set-str len)))
              ;; BREAKX must find at least one character before the
              ;; initial delimiter to succeed.
              (if (< idx first-end)
                (breakx-loop str idx first-end char-set-str env
                             succeed fail)
                (fail)))))))

    ;; Internal stepping loop for ARBNO. Evaluates the target pattern
    ;; 'pat' once, and if it succeeds, pipes its success continuation
    ;; into a recursive instance of itself to match more blocks.
    (define (arbno-loop pat str idx env succeed fail)
      (let ((str (->utf32 str)))
        (pat str idx env
             (lambda (next-idx next-env retry-fail)
               ;; CRITICAL SPITBOL SEMANTICS: Enforce forward progress
               ;; to prevent infinite loops on empty-matching patterns.
               (if (< idx next-idx)
                 (succeed next-idx next-env
                          (lambda ()
                            (arbno-loop pat str next-idx next-env
                                        succeed retry-fail)))
                 (succeed idx env fail)))
             fail)))

    ;; SNOBOL/SPITBOL ARBNO: Matches zero or more repetitions of 'pat'
    ;; minimally. Initially matches 0 characters, expanding to match
    ;; more blocks on downstream failure.
    (define (p:arbno pat)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          ;; Immediately succeed matching 0 characters.
          (succeed idx env
                   (lambda ()
                     ;; On downstream failure, fallback and try to
                     ;; match 1 or more pieces.
                     (arbno-loop pat str idx env succeed fail))))))

    ;;------------------------------------------------------------------
    ;;
    ;; Predicates and character matchers.
    ;;

    ;; Core abstraction for analyzing single character elements.
    (define (p:pred predicate)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (if (< idx (utf32-string-length str))
            (let ((char (utf32-string-ref str idx)))
              (if (predicate char)
                (succeed (+ idx 1) env fail)
                (fail)))
            (fail)))))

    ;; Tail-recursive search for validating single elements inside raw
    ;; string containers.
    (define (string-has-char? str target-char)
      (let ((str (->utf32 str)))
        (let ((len (utf32-string-length str)))
          (let loop ((i 0))
            (cond ((= i len) #f)
                  ((char=? (utf32-string-ref str i) target-char) #t)
                  (else (loop (+ i 1))))))))

    ;; SNOBOL ANY: Matches a character found inside a target set
    ;; string.
    (define (p:any char-set-str)
      (let ((char-set-str (->utf32 char-set-str)))
        (p:pred (lambda (char) 
                  (string-has-char? char-set-str char)))))

    ;; SNOBOL NOTANY: Matches a character absent from a target set
    ;; string.
    (define (p:notany char-set-str)
      (let ((char-set-str (->utf32 char-set-str)))
        (p:pred (lambda (char) 
                  (not (string-has-char? char-set-str char))))))

    ;; Internal character lookup helper to keep branch depth under the
    ;; ceiling.
    (define (char-in-string? str target-char)
      (let ((str (->utf32 str)))
        (let ((len (utf32-string-length str)))
          (let loop ((i 0))
            (cond ((= i len) #f)
                  ((char=? (utf32-string-ref str i) target-char) #t)
                  (else (loop (+ i 1))))))))

    ;; Matches a single character at the cursor index if it matches
    ;; any character in a string set, or satisfies any predicate in a
    ;; list.
    (define p:any-of
      (case-lambda
        ;;
        ;; Single-argument string variation:
        ;;
        ;;   (p:any-of "0123456789")
        ;;
        ;; Single-argument predicate variation:
        ;;
        ;;   (p:any-of char-numeric?)
        ;;
        ((arg)
         (if (procedure? arg)
           (p:pred arg)
           (let ((char-set-str (->utf32 arg)))
             (p:pred (lambda (char) 
                       (char-in-string? char-set-str char))))))
        ;;
        ;; Multi-argument predicate variation:
        ;;
        ;;   (p:any-of char-alphabetic? char-numeric?)
        ;;
        (preds
         (p:pred (lambda (char)
                   (let loop ((p-list preds))
                     (cond ((null? p-list) #f)
                           (((car p-list) char) #t)
                           (else (loop (cdr p-list))))))))))

    (define (none-of-aux preds)
      (p:pred (lambda (char)
                (let loop ((p-list preds))
                  (cond ((null? p-list) #t)
                        (((car p-list) char) #f)
                        (else (loop (cdr p-list))))))))
    
    ;; Matches a single character at the cursor index if it is NOT in
    ;; the string set, or fails to satisfy every predicate in the
    ;; provided list.
    (define p:none-of
      (case-lambda
        ;;
        ;; Single-argument string variation:
        ;;
        ;;   (p:none-of "aeiou")
        ;;
        ;; Single-argument predicate variation:
        ;;
        ;;   (p:none-of char-numeric?)
        ;;
        ((arg)
         (if (procedure? arg)
           (none-of-aux (list arg))
           (let ((char-set-str (->utf32 arg)))
             (p:pred (lambda (char)
                       (not (char-in-string? char-set-str char)))))))
        ;;
        ;; Multi-argument predicate list variation:
        ;;
        ;;   (p:none-of char-whitespace? char-numeric?)
        ;;
        (preds
         (none-of-aux preds))))

    ;; Internal stepping loop for p:many. Greedily attempts to match
    ;; another instance of `pat`. If it succeeds, it sets up a
    ;; backtracking choice point to the prior length.
    (define (many-loop pat str idx env succeed fail)
      (let ((str (->utf32 str)))
        (pat str idx env
             (lambda (next-idx next-env retry-fail)
               (if (< idx next-idx)
                 (many-loop pat str next-idx next-env succeed
                            (lambda () (succeed idx env retry-fail)))
                 (succeed idx env fail)))
             (lambda ()
               (succeed idx env fail)))))

    ;; SNOBOL equivalent to matching 1 or more repetitions of a
    ;; pattern. Evaluates `pat` exactly once, then hands control off
    ;; to the greedy backtracking loop.
    (define (p:many pat)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (pat str idx env
               (lambda (next-idx next-env retry-fail)
                 (many-loop pat str next-idx next-env
                            succeed retry-fail))
               fail))))

    ;; SNOBOL equivalent to matching 0 or more repetitions of a
    ;; pattern. Greedily matches as many instances as possible, but
    ;; succeeds with an unchanged cursor position if zero matches are
    ;; found.
    (define (p:maybe-many pat)
      (p:alt (p:many pat)
             (lambda (str idx env succeed fail)
               (succeed idx env fail))))

    ;;------------------------------------------------------------------
    ;;
    ;; Assertions and context controls.
    ;;

    ;; SNOBOL FENCE: Matches null initially, drops backtracking path
    ;; on fallback.
    (define (p:fence)
      (lambda (str idx env succeed fail)
        (succeed idx env (lambda () (fail)))))

    ;; SNOBOL ABORT: Instantly terminates the entire match attempt,
    ;; bypassing all retries.
    (define (p:abort)
      (lambda (str idx env succeed fail)
        ;; Returning a raw #f here intentionally breaks the
        ;; backtracking chain.
        #f))

    ;; SNOBOL POS(n): Confirms engine tracking pointer is exactly
    ;; positioned at index n.
    (define (p:pos n)
      (lambda (str idx env succeed fail)
        (if (= idx n)
          (succeed idx env fail)
          (fail))))

    ;;------------------------------------------------------------------
    ;;
    ;; p:bal
    ;;

    (define *bracket-pair-list*
      (make-parameter (list (list))))

    (define (bracket-pairs)
      (car (*bracket-pair-list*)))

    (define (set-bracket-pairs! lst)
      (unless (and (proper-list? lst)
                   (every (lambda (entry)
                            (and (pair? entry)
                                 (char? (car entry))
                                 (char? (cdr entry))))
                          lst))
        (error "not a correct bracket pair list" lst))
      (set-car! (*bracket-pair-list*) lst))

    (define-syntax localize-bracket-pairs
      (syntax-rules ()
        ((localize-bracket-pairs body ...)
         ;; Copy the bracket pairs.
         (parameterize ((*bracket-pair-list*
                         (list (map (lambda (pair)
                                      (cons (car pair) (cdr pair)))
                                    (car (*bracket-pair-list*))))))
           body ... (if #f #f)))))

    ;; Start with the brackets supported by popular Schemes.
    (set-bracket-pairs!
     (list '(#\( . #\))
           '(#\[ . #\]) 
           '(#\{ . #\})))

    ;; Checks if a character matches any registered open or close
    ;; bracket symbol.
    (define (any-bracket? char)
      (let ((is-open (assoc char (bracket-pairs)))
            (is-close (rassoc char (bracket-pairs))))
        (not (not (or is-open is-close)))))

    ;; Matches a single character that is not a registered bracket
    ;; type.
    (define (p:bal-text-char)
      (p:pred (lambda (char)
                (not (any-bracket? char)))))

    ;; Nested context structural matcher. Dynamically finds the
    ;; correct closing character for whatever open bracket it sees.
    (define (p:bal-nest)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (if (< idx (utf32-string-length str))
            (let* ((open-char (utf32-string-ref str idx))
                   (pair (assoc open-char (bracket-pairs))))
              (if pair
                (let* ((close-str (string (cdr pair)))
                       (open-str (string open-char))
                       (nest-pat (p:seq (p:lit open-str)
                                        (p:seq (p:bal-loop) 
                                               (p:lit close-str)))))
                  (nest-pat str idx env succeed fail))
                (fail)))
            (fail)))))

    ;; Balanced element parser: Either pure text or a matching bracket
    ;; block.
    (define (bal-element str idx env succeed fail)
      (let ((str (->utf32 str)))
        (let ((alt-matcher (p:alt (p:bal-text-char) (p:bal-nest))))
          (alt-matcher str idx env succeed fail))))

    ;; Zero-or-more tracking driver to safely loop consecutive
    ;; balanced components
    (define (bal-loop-proc str idx env succeed fail)
      (let ((str (->utf32 str)))
        (let ((loop-matcher (p:alt (p:seq bal-element (p:bal-loop))
                                   (lambda (s i e succ f)
                                     (succ i e f)))))
          (loop-matcher str idx env succeed fail))))

    ;; Thunk construction layer to shield recursive loop compilation
    ;; paths.
    (define (p:bal-loop)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (bal-loop-proc str idx env succeed fail))))

    ;; Global multi-bracket SNOBOL BAL variant.
    (define (p:bal)
      (p:seq bal-element (p:bal-loop)))


    ;;------------------------------------------------------------------
    ;;
    ;; Tabbing.
    ;;

    ;; SNOBOL TAB(n): Moves the cursor forward to absolute position n.
    ;; Fails if the cursor has already passed position n or if n
    ;; exceeds the string length.
    (define (p:tab n)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (if (and (>= n idx)
                   (<= n (utf32-string-length str)))
            (succeed n env fail)
            (fail)))))

    ;; SNOBOL RTAB(n): Moves the cursor forward, leaving exactly n
    ;; characters remaining. Fails if fewer than n characters remain
    ;; from the current cursor position.
    (define (p:rtab n)
      (lambda (str idx env succeed fail)
        (let ((str (->utf32 str)))
          (let ((target-idx (- (utf32-string-length str) n)))
            (if (>= target-idx idx)
              (succeed target-idx env fail)
              (fail))))))

    ;; SNOBOL REM: Matches the entire remainder of the string.
    (define (p:rem)
      (p:rtab 0))
    
    ))

;;;---------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; coding: utf-8
;;; end:
