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

(define-library (calm1-lib main)

  (import (scheme base)
          (scheme case-lambda)
          (scheme file)
          (scheme read)
          (scheme write)
          (scheme process-context)
          (scheme eval))

  (import (srfi 37)) ;; args-fold

  (cond-expand
    ;;
    ;; Try (srfi 1) before (scheme list), because what comes with Chibi
    ;; was broken the last I looked, and it is more likely a different
    ;; (srfi 1) is available than a different (scheme list) is. Chibi’s
    ;; library was mishandling improper lists.
    ;;
    ;; Adapting Olin Shivers’s reference implementation for SRFI-1 is
    ;; what I would do, if it mattered for a particular use. (I already
    ;; did this for my Pipchix project.)
    ;;
    ;;   — B. Schwartz, 27 Sept 2026
    ;;
    ((library (srfi 1)) (import (srfi 1)))
    ((library (scheme list)) (import (scheme list)))
    (else (import (srfi srfi-1))))

  (cond-expand
    ((library (scheme charset)) (import (scheme charset)))
    ((library (srfi 14)) (import (srfi 14)))
    (else (import (srfi srfi-14))))

  (cond-expand
    ((library (scheme fixnum)) (import (scheme fixnum)))
    ((library (srfi 143)) (import (srfi 143)))
    (else (import (srfi srfi-143))))

  (import (calm1-lib utf32-string)
          (calm1-lib snobol-match)
          (calm1-lib snobol-char-set)
          (calm1-lib random-fixnum)
          (calm1-lib define-record-factory)
          (calm1-lib preprocessor-variables)
          (calm1-lib preprocessor-macro-handler)
          (calm1-lib preprocessor-define-lookup))

  (export main)

  (begin

;;;---------------------------------------------------------------------

    (define *environment*
      (make-parameter
       (environment
        '(scheme base)
        '(scheme char)
        '(scheme case-lambda)
        '(scheme file)
        '(scheme read)
        '(scheme write)
        '(scheme inexact)
        '(scheme complex)
        '(srfi 37)
        (cond-expand
          ((library (scheme list)) (quote (scheme list)))
          ((library (srfi 1)) (quote (srfi 1)))
          (loko (quote (except (srfi :1 lists)
                               member map for-each
                               assoc list-copy make-list)))
          (else (quote (srfi srfi-1))))
        (cond-expand
          ((library (scheme charset)) (quote (scheme charset)))
          ((library (srfi 14)) (quote (srfi 14)))
          (loko (quote (srfi :14 char-sets)))
          (else (quote (srfi srfi-14))))
        (cond-expand
          ((library (scheme fixnum)) (quote (scheme fixnum)))
          ((library (srfi 143)) (quote (srfi 143)))
          (loko (quote (srfi :143 fixnums)))
          (else (quote (srfi srfi-143))))
        '(calm1-lib utf32-string)
        '(calm1-lib snobol-match)
        '(calm1-lib snobol-char-set)
        '(calm1-lib random-fixnum)
        '(calm1-lib preprocessor-variables)
        '(calm1-lib define-record-factory)
        '(calm1-lib preprocessor-macro-handler)
        '(calm1-lib preprocessor-define-lookup))))

    (define *program-name* (make-parameter #f))

    (define-syntax unspecified-value
      (syntax-rules ()
        ((¶)
         (if #f #f))))

    (define (utf32-string? obj)
      ;;
      ;; FIXME: This will suffice unless we put a proper first-class
      ;; wrapper on utf32-string.
      ;;
      (bytevector? obj))

    (define (->utf32 str)
      (cond ((string? str) (string->utf32-string str))
            (else str)))

    (define (utf32-> str)
      (cond ((utf32-string? str) (utf32-string->string str))
            (else str)))

    (define empty-string (string->utf32-string ""))
    (define (spaces n) (string->utf32-string (make-string n #\x20)))
    (define one-space (spaces 1))
    (define macro-open (string->utf32-string "(@ "))
    (define macro-close (string->utf32-string ")"))

    ;; "#|" and "|#" get converted to these code points, for handling
    ;; as if they were bracket characters. These code points are
    ;; chosen haphazardly from Supplemental Private Use Area-B.
    (define comment-left #\x10A631)
    (define comment-right #\x10A632)

    (define (comment-brackets->chars str)
      ;;
      ;; Convert "#|" and "|#" to single code points in an
      ;; out-of-the-way range.
      ;;
      (let* ((n (utf32-string-length str))
             (t (make-utf32-string n)))
        (let loop1 ((i 0)
                    (k 0))
          (if (= i n)
            (utf32-string-copy t 0 k)
            (let loop2 ((j i))
              (cond ((= j n)
                     (utf32-string-copy! t k str i j)
                     (loop1 j (+ k (- j i))))
                    ((and (not (= (+ j 1) n))
                          (char=? (utf32-string-ref str j) #\#)
                          (char=? (utf32-string-ref str (+ j 1)) #\|))
                     (utf32-string-copy! t k str i j)
                     (utf32-string-set! t (+ k (- j i)) comment-left)
                     (loop1 (+ j 2) (+ k (- j i) 1)))
                    ((and (not (= (+ j 1) n))
                          (char=? (utf32-string-ref str j) #\|)
                          (char=? (utf32-string-ref str (+ j 1)) #\#))
                     (utf32-string-copy! t k str i j)
                     (utf32-string-set! t (+ k (- j i)) comment-right)
                     (loop1 (+ j 2) (+ k (- j i) 1)))
                    (else
                     (loop2 (+ j 1)))))))))

    (define (chars->comment-brackets str)
      ;;
      ;; Convert certain code points to "#|" and "|#".
      ;;
      (let* ((n (utf32-string-length str))
             (t (make-utf32-string (+ n n))))
        (let loop1 ((i 0)
                    (k 0))
          (if (= i n)
            (utf32-string-copy t 0 k)
            (let loop2 ((j i))
              (cond ((= j n)
                     (utf32-string-copy! t k str i j)
                     (loop1 j (+ k (- j i))))
                    ((char=? (utf32-string-ref str j) comment-left)
                     (utf32-string-copy! t k str i j)
                     (utf32-string-set! t (+ k (- j i)) #\#)
                     (utf32-string-set! t (+ k (- j i) 1) #\|)
                     (loop1 (+ j 1) (+ k (- j i) 2)))
                    ((char=? (utf32-string-ref str j) comment-right)
                     (utf32-string-copy! t k str i j)
                     (utf32-string-set! t (+ k (- j i)) #\|)
                     (utf32-string-set! t (+ k (- j i) 1) #\#)
                     (loop1 (+ j 1) (+ k (- j i) 2)))
                    (else
                     (loop2 (+ j 1)))))))))

    (define random-fixnum
      (make-random-fixnum))

    (define random-integer
      (make-random-integer random-fixnum))

    (define (vector-shuffle! vec)
      ;;
      ;; Fisher-Yates shuffle.
      ;;
      (let loop ((i (- (vector-length vec) 1)))
        (when (positive? i)
          (let* ((j (random-integer (+ i 1))) ;; 0 <= j <= i
                 (tmp (vector-ref vec i)))
            (vector-set! vec i (vector-ref vec j))
            (vector-set! vec j tmp)
            (loop (- i 1))))))

    (define (eval-string str env)
      (eval (read (open-input-string str)) env))

    (define (evaluate str)
      (eval-string (string-append "(values "
                                  (if (bytevector? str)
                                    (utf32-string->string str)
                                    str)
                                  " )")
                   (*environment*)))

    (define (serialize obj)
      (let ((port (open-output-string)))
        (write obj port)
        (string->utf32-string (get-output-string port))))

    (define (stringize x)
      (cond ((utf32-string? x) x)
            ((string? x) (string->utf32-string x))
            ((symbol? x) (symbol->utf32-string x))
            ((number? x) (number->utf32-string x))
            ((char? x) (utf32-string (char->integer x)))
            (else (serialize x))))

    (define (getvar key match-result)
      (and match-result
           (let ((p (assoc key (cdr match-result))))
             (and p (cdr p)))))

    (define (remove-prefix prefix str)
      (let ((m (utf32-string-length prefix))
            (n (utf32-string-length str)))
        (if (<= n m)
          empty-string
          (utf32-string-copy str m))))

    (define read-to-string
      (let ((n 4096))
        (case-lambda
          (()
           (read-to-string (current-input-port)))
          ((port)
           (comment-brackets->chars
            (let loop ((lst '()))
              (let ((s (read-string n port)))
                (if (eof-object? s)
                  (utf32-string-concatenate
                   (map! string->utf32-string (reverse! lst)))
                  (loop (cons s lst))))))))))

    (define display-as-string
      (case-lambda
        ((obj)
         (display-as-string obj (current-output-port)))
        ((obj port)
         (display
          (utf32-string->string
           (chars->comment-brackets (stringize obj)))
          port))))

;;;---------------------------------------------------------------------

    (define *quick-pattern*
      (make-parameter
       (p:assign-local (p:break "(;#") 'snippet)))

    (define char-set:macro-name
      (char-set-difference char-set:graphic
                           (string->char-set "()[]{};|'`,@\"\\")))

    (define (macro-name? str)
      (let ((n (utf32-string-length str)))
        (and (not (zero? n))
             (let loop ((i 0))
               (cond ((= i n) #t)
                     ((char-set-contains? char-set:macro-name
                                          (utf32-string-ref str i))
                      (loop (+ i 1)))
                     (else #f))))))

    (define *macro-pattern*
      (make-parameter
       (p:assign-local
        (p:seq (p:lit "(")
               (p:maybe-many (p:span-char-set char-set:whitespace))
               (p:lit "@")
               (p:span-char-set char-set:whitespace)
               (p:assign-local (p:span-char-set char-set:macro-name)
                               'macro-name)
               (p:maybe-many (p:span-char-set char-set:whitespace))
               (p:assign-local
                (p:maybe-many
                 (p:seq (p:bal)
                        (p:maybe-many
                         (p:span-char-set char-set:whitespace))))
                'macro-body)
               (p:lit ")"))
        'macro-call)))

    (define *line-comment-pattern*
      (make-parameter
       (p:assign-local (p:seq
                        (p:lit ";")
                        (p:break "\n")
                        (p:lit "\n"))
                       'comment)))

    (define *form-comment-pattern*
      (make-parameter
       (p:assign-local (p:seq
                        (p:lit "#;")
                        (p:maybe-many
                         (p:span-char-set char-set:whitespace))
                        (p:lit "(")
                        (p:bal)
                        (p:lit ")"))
                       'comment)))

;;;---------------------------------------------------------------------

    (define *deterministic?*
      ;;
      ;; Are branching and looping deterministic?
      ;;
      (make-parameter #f))

    (define (process-text definitions text output-port)

      (define t (utf32-string-copy text))

      (define (shorten-t! n)
        (set! t (utf32-string-copy t n)))

      (define (remove-t-prefix! prefix)
        (set! t (remove-prefix (stringize prefix) t)))

      (define (reinsert! str)
        (set! t (utf32-string-append (stringize str) t)))

      (define (output-to-port obj)
        (display-as-string obj output-port))

      ;;----------------------------------------------------
      ;; (@ eval FORM ...)
      ;; (@ hide FORM ...)
      ;; (@ dnl FORM ...)
      ;;
      ;; Evaluate FORM ... as Scheme. The “eval” version expands the
      ;; results, whereas “hide” does not. The “dnl” version not only
      ;; hides the results, it deletes text up through the next
      ;; newline.
      ;;

      (define eval-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (let-values ((form-lst (evaluate macro-body)))
             (for-each (lambda (form)
                         (output-to-port (serialize form)))
                       form-lst)))))

      (define hide-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (let-values ((form-lst (evaluate macro-body)))
             (unspecified-value)))))

      (define dnl-handler
        (let ((pattern (p:seq (p:alt (p:seq (p:break "\n") (p:lit "\n"))
                                     (p:lit "\n"))
                              (p:cursor 'cursor))))
          (make-macro-handler
           (lambda (macro-call macro-name macro-body)
             (let-values ((form-lst (evaluate macro-body)))
               (let ((match-result (snobol-match pattern t)))
                 (when match-result
                   (let ((n (utf32-string->number
                             (getvar 'cursor match-result))))
                     (shorten-t! n)))))))))

      ;;----------------------------------------------------
      ;; (@ when PREDICATE FORM ...)
      ;; (@ unless PREDICATE FORM ...)
      ;;
      ;; For “when”, re-insert the evaluated results of FORM ... if
      ;; PREDICATE evaluates as true in Scheme. For “unless”, reverse
      ;; the sense of the PREDICATE.
      ;;

      (define-syntax when-or-unless-handler
        (syntax-rules ()
          ((¶ when-or-unless macro-body)
           (let-values ((form-lst (evaluate macro-body)))
             (unless (<= 1 (length form-lst))
               (error "expected PREDICATE FORM ..." form-lst))
             (let-values (((pred forms) (car+cdr form-lst)))
               (let ((str ""))
                 (when-or-unless
                  pred
                  (do ((p (reverse forms) (cdr p)))
                      ((null? p))
                    (set! str (string-append (car p) str))))
                 (reinsert! str)))))))

      (define when-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (when-or-unless-handler when macro-body))))

      (define unless-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (when-or-unless-handler unless macro-body))))

      ;;----------------------------------------------------
      ;; (@ if FORM ...)
      ;; (@ while FORM ...)
      ;;
      ;; Nondeterministic branching and looping. (These let you write
      ;; deterministic branching and looping by putting the logic in
      ;; the Scheme code.)
      ;;
      ;; True branches are converted to strings if possible, and
      ;; re-inserted.
      ;;

      (define-syntax if-or-while-handler
        (syntax-rules ()
          ((¶ macro-body proc)
           (let-values ((forms (evaluate macro-body)))
             (let ((n (length forms)))
               (if (zero? n)
                 ""
                 (let ((v (make-vector n)))
                   (do ((i 0 (+ i 1))
                        (p forms (cdr p)))
                       ((= i n))
                     (vector-set! v i (first p)))
                   (unless (*deterministic?*)
                     ;; Enforce non-determinism.
                     (vector-shuffle! v))
                   (proc forms v n))))))))

      (define if-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (if-or-while-handler
            macro-body
            (lambda (forms v n)
              (let loop ((i (- n 1)))
                (cond ((= i -1)
                       (error "no case is satisfied" forms))
                      ((vector-ref v i) =>
                       (lambda (x)
                         (reinsert! (stringize x))))
                      (else
                       (loop (- i 1))))))))))

      (define while-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (if-or-while-handler
            macro-body
            (lambda (forms v n)
              (let loop ((i (- n 1)))
                (cond ((= i -1)
                       (unspecified-value))
                      ((vector-ref v i) =>
                       ;; Reinsert both the expansion and the (@ while ...)
                       (lambda (x)
                         (reinsert!
                          (utf32-string-append
                           (stringize x)
                           macro-open
                           macro-name one-space macro-body
                           macro-close))))
                      (else
                       (loop (- i 1))))))))))

      ;;----------------------------------------------------
      ;; (@ include-raw FORM ...)
      ;;
      ;; Non-recursive include of the files specified by FORM ...
      ;;

      (define include-raw-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (let-values ((filenames (evaluate macro-body)))
             (do ((f filenames (cdr f)))
                 ((null? f))
               (with-input-from-file (car f)
                 (lambda ()
                   (output-to-port (read-to-string)))))))))

      ;;----------------------------------------------------
      ;; (@ include FORM ...)
      ;;
      ;; Recursive include of the files specified by FORM ...
      ;;

      (define include-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (let-values ((filenames (evaluate macro-body)))
             (do ((f (reverse filenames) (cdr f)))
                 ((null? f))
               (with-input-from-file (car f)
                 (lambda ()
                   (reinsert! (read-to-string)))))))))

      ;;----------------------------------------------------
      ;; (@ define MACRO-NAME MACRO-BODY)
      ;; (@ pushdef MACRO-NAME MACRO-BODY)
      ;;
      ;; Define a macro with the name given by the form MACRO-NAME and
      ;; definition by the form MACRO-BODY, which (in the current
      ;; implementation) must evaluate to a string. When a macro call is
      ;; made, MACRO-BODY is inserted and then evaluated recursively.
      ;;
      ;; The “define” version pops any top definition of the macro,
      ;; whereas “pushdef” does not.
      ;;

      (define reinsert-popdef-i!
        (let ((before (->utf32 "(@ popdef \""))
              (after (->utf32 "\")")))
          (lambda (i)
            (reinsert! (utf32-string-append
                        before (number->utf32-string i) after)))))

      (define reinsert-pushdef-i!
        (let ((before (->utf32 "(@ pushdef \""))
              (between (->utf32 "\" "))
              (after (->utf32 ")")))
          (lambda (i value)
            (reinsert! (utf32-string-append
                        before (number->utf32-string i)
                        between (serialize value) after)))))

      (define-syntax define-macro
        (syntax-rules ()
          ((¶ set-or-push-macro-handler! macro-body)
           (let*-values (((name body) (evaluate macro-body))
                         ((name) (if (bytevector? name)
                                   (utf32-string->string name)
                                   name)))
             (unless (string? name)
               (error "macro name must be a string" name))
             (set-or-push-macro-handler!
              name
              (if (macro-handler? body)
                body
                (make-macro-handler
                 (lambda (mac-call mac-name mac-body)
                   (let-values ((vals (evaluate mac-body)))
                     (let ((n (length vals)))
                       (reinsert-popdef-i! 0)
                       (do ((i 1 (+ i 1)))
                           ((= i (+ n 1)))
                         (reinsert-popdef-i! i))
                       (reinsert! body)
                       (reinsert-pushdef-i! 0 name)
                       (do ((i 1 (+ i 1))
                            (p vals (cdr p)))
                           ((= i (+ n 1)))
                         (reinsert-pushdef-i! i (car p)))
                       ))))))))))

      (define definition-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (define-macro set-macro-handler! macro-body))))

      (define pushdef-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (define-macro push-macro-handler! macro-body))))

      ;;----------------------------------------------------
      ;; (@ popdef MACRO-NAME ...)
      ;;
      ;; Pop definitions.
      ;;

      (define popdef-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (let-values ((names (evaluate macro-body)))
             (do ((nm names (cdr nm)))
                 ((null? nm))
               (unless (string? (car nm))
                 (error "macro name must be a string" (car nm)))
               (pop-macro-handler! (car nm)))))))

      ;;----------------------------------------------------
      ;; (@ undefine MACRO-NAME ...)
      ;;
      ;; Remove all definitions corresponding to the given macro
      ;; names.
      ;;

      (define undefine-handler
        (make-macro-handler
         (lambda (macro-call macro-name macro-body)
           (let-values ((names (evaluate macro-body)))
             (do ((nm names (cdr nm)))
                 ((null? nm))
               (unless (string? (car nm))
                 (error "macro name must be a string" (car nm)))
               (remove-macro-handlers! (car nm)))))))

      ;;----------------------------------------------------

      (define (handle-quick-result match-result)
        (let ((snippet (getvar 'snippet match-result)))
          (remove-t-prefix! snippet)
          (output-to-port snippet)))

      (define (handle-macro-call match-result)
        (let ((macro-call (getvar 'macro-call match-result))
              (macro-name (getvar 'macro-name match-result))
              (macro-body (getvar 'macro-body match-result)))
          (remove-t-prefix! macro-call)
          (let ((macro-handler (get-macro-handler macro-name)))
            (if (not macro-handler)
              (output-to-port macro-call)
              ((macro-handler-procedure macro-handler)
               macro-call macro-name macro-body)))))

      (define (handle-line-comment match-result)
        (let ((comment (getvar 'comment match-result)))
          (remove-t-prefix! comment)
          (output-to-port comment)))

      (define (handle-form-comment match-result)
        (let ((comment (getvar 'comment match-result)))
          (remove-t-prefix! comment)
          (output-to-port comment)))

      (define (apply-definitions-list definitions)
        (do ((defs definitions (cdr defs)))
            ((null? defs))
          (let ((macro-name (first (car defs)))
                (macro-body (second (car defs)))
                (define? (third (car defs))))
            (if define?
              ((macro-handler-procedure definition-handler)
               empty-string empty-string
               (utf32-string-append (serialize macro-name)
                                    one-space
                                    (serialize macro-body)))
              (remove-macro-handlers! macro-name)))))

      (set-macro-handler! "dnl" dnl-handler)
      (set-macro-handler! "eval" eval-handler)
      (set-macro-handler! "hide" hide-handler)
      (set-macro-handler! "when" when-handler)
      (set-macro-handler! "unless" unless-handler)
      (set-macro-handler! "if" if-handler)
      (set-macro-handler! "while" while-handler)
      (set-macro-handler! "include-raw" include-raw-handler)
      (set-macro-handler! "include" include-handler)
      (set-macro-handler! "define" definition-handler)
      (set-macro-handler! "undefine" undefine-handler)
      (set-macro-handler! "pushdef" pushdef-handler)
      (set-macro-handler! "popdef" popdef-handler)

      (apply-definitions-list definitions)
      (let loop ()
        (cond ((= 0 (utf32-string-length t))
               (unspecified-value))
              ((snobol-match (*quick-pattern*) t) =>
               (lambda (match-result)
                 (handle-quick-result match-result)
                 (loop)))
              ((snobol-match (*macro-pattern*) t) =>
               (lambda (match-result)
                 (handle-macro-call match-result)
                 (loop)))
              ((snobol-match (*form-comment-pattern*) t) =>
               (lambda (match-result)
                 (handle-form-comment match-result)
                 (loop)))
              ((snobol-match (*line-comment-pattern*) t) =>
               (lambda (match-result)
                 (handle-line-comment match-result)
                 (loop)))
              (else
               (output-to-port (utf32-string-copy t 0 1))
               (set! t (utf32-string-copy t 1))
               (loop)))))

    (define (run-the-program definitions input-files output-file)
      (let* ((use-stdout? (string=? output-file "-"))
             (output-port (if use-stdout?
                            (current-output-port)
                            (open-output-file output-file)))
             (texts (list)))
        (do ((p input-files (cdr p)))
            ((null-list? p))
          (let* ((input-file (car p))
                 (use-stdin? (string=? input-file "-"))
                 (input-port (if use-stdin?
                               (current-input-port)
                               (open-input-file input-file))))
            (let ((text (read-to-string input-port)))
              (set! texts (append! texts (list text))))
            (unless use-stdin?
              (close-input-port input-port))))
        (localize-bracket-pairs
         (set-bracket-pairs!
          (cons (cons comment-left comment-right)
                (bracket-pairs)))
         (process-text definitions
                       (utf32-string-concatenate texts)
                       output-port))
        (unless use-stdout?
          (close-output-port output-port))))

;;;---------------------------------------------------------------------

    (define-record-factory <seed>

      (predicate> seed?)

      (constructor> initial-seed
                    (lambda (construct)
                      (lambda ()
                        (construct (list) ; positionals
                                   (list) ; definitions
                                   #f     ; deterministic?
                                   "-"    ; output
                                   ))))

      ;;
      ;; Positional arguments.
      ;;
      (getter> 1 get-positionals)
      (setter> 1 set-positionals!
               (lambda (setter!)
                 (lambda (obj value)
                   (unless (and (proper-list? value)
                                (every string? value))
                     (error "expected proper list of strings" value))
                   (setter! obj value))))

      ;;
      ;; Custom macro definitions: (("name" "body" #t) ...)
      ;;
      ;;    #t is for define
      ;;    #f is for undefine
      ;;
      ;;    For undefine, the body will be ignored and may be any
      ;;    value. We will set it to a peculiar symbol that might help
      ;;    in debugging.
      ;;
      (getter> 2 get-definitions)
      (setter> 2 set-definitions!
               (lambda (setter!)
                 (lambda (obj value)
                   (unless (and (proper-list? value)
                                (every
                                 (lambda (entry)
                                   (and (proper-list? entry)
                                        (= 3 (length entry))
                                        (utf32-string? (first entry))
                                        (boolean? (third entry))
                                        (or (not (third entry))
                                            (utf32-string?
                                             (second entry)))))
                                 value))
                     (error "expected a list of \
                             ((\"name\" \"body\" #t) ...)"
                            (map (lambda (entry) (map utf32-> entry))
                                 value)))
                   (setter! obj value))))

      ;;
      ;; Deterministic?  #t or #f  [default #f]
      ;;
      (getter> 3 get-deterministic)
      (setter> 3 set-deterministic!
               (lambda (setter!)
                 (lambda (obj value)
                   (unless (boolean? value)
                     (error "expected a boolean" value))
                   (setter! obj value))))

      (getter> 4 get-output)
      (setter> 4 set-output!)

      ) ;; end <seed>

    (define options
      (list
       ;;
       ;; -o file, --output=file
       ;;
       (option
        '(#\o "output") #t #f
        (lambda (opt name arg seed)
          (set-output! seed arg)
          seed))

       ;;
       ;; -D name[=value], --define=name[=value]
       ;;
       ;; If the value is left out, the body of the macro is an empty
       ;; string. This is the same behavior as m4.
       ;;
       ;; You can have a macro name with an equals sign in it by using
       ;; escape sequences.
       ;;
       (option
        '(#\D "define") #t #f
        (let* ((name-set (char-set-difference
                          char-set:graphic
                          (string->char-set "=")))
               (pattern
                (p:alt
                 (p:seq
                  (p:assign-local
                   (p:many (p:any-char-set name-set))
                   'macro-name)
                  (p:lit "=")
                  (p:assign-local
                   (p:maybe-many (p:any-char-set char-set:full))
                   'macro-body))
                 (p:assign-local
                  (p:many (p:any-char-set name-set))
                  'macro-name))))
          (lambda (opt name arg seed)
            (cond
              ((and arg (snobol-match pattern arg)) =>
               (lambda (match-result)
                 (let ((macro-name (getvar 'macro-name match-result))
                       (macro-body (or (getvar 'macro-body match-result)
                                       empty-string)))
                   (set-definitions!
                    seed (append! (get-definitions seed)
                                  (list (list macro-name
                                              macro-body #t)))))
                 seed))
              (else
               (set-definitions!
                seed (append! (get-definitions seed)
                              (list (list empty-string
                                          empty-string #t))))
               seed)))))

       ;;
       ;; -U name, --undefine=name
       ;;
       (option
        '(#\U "undefine") #t #f
        (let* ((name-set char-set:graphic)
               (pattern
                (p:assign-local
                 (p:many (p:any-char-set name-set))
                 'macro-name))
               (filler
                (string->symbol
                 (list->string
                  (reverse
                   (string->list "pihS\xA0;sserpxE\xA0;tenalP"))))))
          (lambda (opt name arg seed)
            (cond
              ((and arg (snobol-match pattern arg)) =>
               (lambda (match-result)
                 (let ((macro-name (getvar 'macro-name match-result)))
                   (set-definitions!
                    seed (append! (get-definitions seed)
                                  (list (list macro-name
                                              filler #f))))
                   seed)))
              (else
               (set-definitions!
                seed (append! (get-definitions seed)
                              (list (list "" filler #f))))
               seed)))))

       (option
        '("deterministic") #f #f
        (lambda (opt name arg seed)
          (set-deterministic! seed #t)
          seed))

       (option
        '("nondeterministic") #f #f
        (lambda (opt name arg seed)
          (set-deterministic! seed #f)
          seed))

       (option
        '("help") #f #f
        (lambda (opt name arg seed)
          (usage-handler 0)))

       (option
        '("version") #f #f
        (lambda (opt name arg seed)
          (version-handler 0)))

       ;;
       ;; FIXME: ADD -I --include
       ;;
       ;; FIXME: MAYBE ADD -s --synclines (by counting \n characters) but
       ;; this may be more trouble than it is worth.
       ;;
       ))

    (define (parse-arguments arguments)

      (define (handle-unknown-option opt name arg seed)
        (let ((port (current-error-port)))
          (display (*program-name*) port)
          (display ": unrecognized option: " port)
          (display name port)
          (newline port)
          (try-help port)
          (exit 1)))

      (define (handle-positionals str seed)
        (set-positionals! seed (append! (get-positionals seed)
                                        (list str)))
        seed)

      (args-fold arguments options
                 handle-unknown-option
                 handle-positionals
                 (initial-seed)))

;;;---------------------------------------------------------------------

    (define (exception-handler err)
      (let ((port (current-error-port)))
        (display "Error: " port)
        (cond ((error-object? err)
               (display (error-object-message err) port)
               (let ((irritants (error-object-irritants err)))
                 (unless (null? irritants)
                   (display ": " port)
                   (write irritants port))))
              ((string? err)
               (display err port))
              (else
               (write err port)))
        (newline port)
        (exit 1)))

    (define (try-help port)
      (display "Try “" port)
      (display (*program-name*) port)
      (display "” --help' for more information.\n" port))

    (define (usage-handler exit-status)
      (let ((port (current-output-port)))
        (display "Usage: " port)
        (display (*program-name*) port)
        (display " [OPTION]... [FILE]...\n" port)
        (display "Process macros in FILEs. If no FILE or if FILE is \
                  “-”, standard input\nis read.\n\n"
                 port)
        (display "Mandatory or optional arguments to long options \
                  are mandatory or optional\n\
                  for short options too.\n\n"
                 port)

        (display "  -D, --define=NAME[=VALUE]" port)
        (display "    define NAME as having VALUE, or empty\n"
                 port)

        (display "  -U, --undefine=NAME" port)
        (display "          undefine NAME\n" port)

        (display "  -o, --output=FILE" port)
        (display "            output to FILE, or to standard output\n"
                 port)
        (display "                                 if FILE is “-”\n"
                 port)

        (display "      --deterministic" port)
        (display "          deterministic “if”, “while”\n"
                 port)

        (display "      --nondeterministic" port)
        (display "       nondeterministic “if”, “while” [default]\n"
                 port)

        (display "      --help" port)
        (display "                   display this help and exit\n"
                 port)

        (display "      --version" port)
        (display "                output version information and exit\n"
                 port)

        (exit exit-status)))

    (define (version-handler exit-status)
      (let ((port (current-output-port)))
        (display (*program-name*) port)
        (display " (" port)
        (display "anxiety reduction by text generation" port)
        (display ")\nexperimental version (●)\n" port)
        (display "\nCopyright © 2026 Barry Schwartz\n" port)
        (display "\n\
Permission is hereby granted, free of charge, to any person\n\
obtaining a copy of this software and associated documentation\n\
files (the “Software”), to deal in the Software without\n\
restriction, including without limitation the rights to use,\n\
copy, modify, merge, publish, distribute, sublicense, and/or sell\n\
copies of the Software, and to permit persons to whom the\n\
Software is furnished to do so, subject to the following\n\
conditions:\n\
\n\
The above copyright notice and this permission notice shall be\n\
included in all copies or substantial portions of the Software.\n\
\n\
THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,\n\
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES\n\
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND\n\
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT\n\
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,\n\
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING\n\
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR\n\
OTHER DEALINGS IN THE SOFTWARE.\n"
                 port)
        (exit exit-status)))

    (define (check-definitions definitions)
      (let ((port (current-error-port)))
        (for-each (lambda (def)
                    (unless (macro-name? (first def))
                      (display (*program-name*) port)
                      (display ": not a legal macro name: “" port)
                      (display (utf32-> (first def)) port)
                      (display "”" port)
                      (newline port)
                      (try-help port)
                      (exit 1)))
                  definitions)))

    (define (main arguments)
      (guard (exc (else (exception-handler exc)))
        (parameterize ((*program-name* (first arguments)))
          (let* ((arguments (cdr arguments))
                 (seed (parse-arguments arguments))
                 (positionals (get-positionals seed))
                 (definitions (get-definitions seed))
                 (deterministic? (get-deterministic seed))
                 (output-filename (get-output seed)))
            (check-definitions definitions)
            (parameterize ((*deterministic?* deterministic?))
              (case (length positionals)
                ((0) (run-the-program definitions '("-")
                                      output-filename))
                (else (run-the-program definitions positionals
                                       output-filename))))))))

    ))

;;;---------------------------------------------------------------------
;;; local variables:
;;; mode: scheme
;;; coding: utf-8
;;; end:
