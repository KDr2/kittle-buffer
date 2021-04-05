#lang racket

(require srfi/13) ; the string SRFI

(provide parse run step reset-kbf
         set-char-outputer set-char-inputer
         BUFFER POINTER-STACK POINTERS
         pointer
         set-error-handler)

(define BUFFER-SIZE 50)
(define BUFFER (make-vector BUFFER-SIZE 0))

(define POINTER-STACK '(0))
(define POINTERS (make-hash '((0 . 0))))

(define (output-char chr) (display chr))
(define (set-char-outputer f) (set! output-char f))
(define input-char read-char)
(define (set-char-inputer f) (set! input-char f))

(define error-handler raise)
(define (set-error-handler eh) (set! error-handler eh))

;; basic facilities

(define (pointer idx)
  (let ([ptr-idx (list-ref POINTER-STACK idx)])
    (hash-ref POINTERS ptr-idx)))
(define (current-pointer) (pointer 0))

(define (current-value)
  (vector-ref BUFFER (current-pointer)))

(define (reset-kbf)
  (set! BUFFER (make-vector BUFFER-SIZE 0))
  (set! POINTER-STACK '(0))
  (set! POINTERS (make-hash '((0 . 0))))
  (set! output-char display)
  (set! input-char read-char))

(define (runtime-error err)
  (error-handler (cons 'RUNTIME-ERROR err)))

(define (parse-error err)
  (error-handler (cons 'PARSE-ERROR err)))

;; built-in functions
(define (b2i x) (if x 1 0))

(define BUILTIN-FUNCTIONS (make-hash))
(hash-set! BUILTIN-FUNCTIONS 0 (lambda (x) x))
(hash-set! BUILTIN-FUNCTIONS 1 (lambda (x)
                                 (let ([s (~a x)])
                                   (for ([c s]) (output-char c))
                                   (string-length s))))
(hash-set! BUILTIN-FUNCTIONS 21 +)
(hash-set! BUILTIN-FUNCTIONS 22 -)
(hash-set! BUILTIN-FUNCTIONS 23 *)
(hash-set! BUILTIN-FUNCTIONS 24 quotient)
(hash-set! BUILTIN-FUNCTIONS 25 modulo)
(hash-set! BUILTIN-FUNCTIONS 26 expt)
(hash-set! BUILTIN-FUNCTIONS 27 (compose b2i =))
(hash-set! BUILTIN-FUNCTIONS 28 (compose b2i >))
(hash-set! BUILTIN-FUNCTIONS 29 (compose b2i <))
(hash-set! BUILTIN-FUNCTIONS 30 (compose b2i >=))
(hash-set! BUILTIN-FUNCTIONS 31 (compose b2i <=))
(hash-set! BUILTIN-FUNCTIONS 32 bitwise-ior)
(hash-set! BUILTIN-FUNCTIONS 33 bitwise-and)
(hash-set! BUILTIN-FUNCTIONS 34 bitwise-xor)
(hash-set! BUILTIN-FUNCTIONS 35 arithmetic-shift)
(hash-set! BUILTIN-FUNCTIONS 36 random)


;; instructions
;; +
(define (ins-plus delta)
  (let ([ptr (current-pointer)]
        [val (current-value)])
    (vector-set! BUFFER ptr (+ val delta))))

;; -
(define (ins-minus delta)
  (let ([ptr (current-pointer)]
        [val (current-value)])
    (vector-set! BUFFER ptr (- val delta))))

;; <
(define (ins-left delta)
  (let ([ptr-idx (car POINTER-STACK)]
        [ptr (current-pointer)])
    (hash-set! POINTERS ptr-idx (max 0 (- ptr delta)))))

;; >
(define (ins-right delta)
  (let ([ptr-idx (car POINTER-STACK)]
        [ptr (current-pointer)])
    (hash-set! POINTERS
               ptr-idx
               (min (- BUFFER-SIZE 1) (+ ptr delta)))))

;; ,
(define (ins-comma)
  (let ([ptr (current-pointer)]
        [val (char->integer (input-char))])
    (vector-set! BUFFER ptr val)))

;; .
(define (ins-dot)
  (let ([ptr (current-pointer)])
    (output-char (integer->char (modulo (vector-ref BUFFER ptr) 256)))))

;; ^
(define (ins-caret val)
  (if (< val 0)
      (when (> (length POINTER-STACK) 1)
        (set! POINTER-STACK (cdr POINTER-STACK)))
      (begin
        (set! POINTER-STACK (cons val POINTER-STACK))
        (when (not (hash-has-key? POINTERS val))
          (hash-set! POINTERS val 0)))))

;; @
(define (ins-at val)
  (if (< (length POINTER-STACK) (+ 2 val))
      (runtime-error '(bad-function-call . arity))
      (let* ([retv-ptr (list-ref POINTER-STACK (+ 1 val))]
             [func-ptr (list-ref POINTER-STACK val)]
             [args-ptr (map (lambda (x) (list-ref POINTER-STACK x))
                            (reverse (build-list val values)))]
             [retv 0]
             [func-key (vector-ref BUFFER (hash-ref POINTERS func-ptr))]
             [func (if (hash-has-key? BUILTIN-FUNCTIONS func-key)
                       (hash-ref BUILTIN-FUNCTIONS func-key)
                       (runtime-error '(bad-function-call . function-id)))]
             [args (map (lambda (x) (vector-ref BUFFER (hash-ref POINTERS x))) args-ptr)])
        (with-handlers
            ([(lambda (v) #t)
              (lambda (ex)
                (runtime-error (cons 'bad-function-call ex)))])
          (set! retv (apply func args)))
        (vector-set! BUFFER (hash-ref POINTERS retv-ptr) retv))))

;; -- parser --
;; instructions is a list of vector(symbol arg peer pos)

(define (instr? chr)
  (case chr
    [(#\+ #\- #\< #\> #\[ #\] #\, #\.) #t] ;; vanilla BF
    [(#\^ #\@ #\?) #t] ;; kbf extension
    [else #f]))

(define (can-have-arg chr)
  (case chr
    [(#\+ #\- #\< #\> #\^ #\@) #t]
    [else #f]))

;; ! support
(define (set-string-inputer str)
  (let* ([idx 0]
         [f (lambda ()
              (let ([p idx])
                (set! idx (modulo (+ 1 idx) (string-length str)))
                (string-ref str p)))])
    (set-char-inputer f)))

(define (remove-bang str)
  (let ([p (string-contains str "!")])
    (if p
        (begin
          (when (< p (sub1 (string-length str)))
            (set-string-inputer (substring str (+ 1 p))))
          (substring str 0 p))
        str)))

;; parse instructions
(define (parse-0 str)
  (let ([instructions '()]
        [current-ins-start 0]
        [current-ins (make-vector 4 -1)]
        [str (remove-bang str)])
    (for ([chr str]
          [idx (in-range (string-length str))])
      (cond
       [(instr? chr) (begin
                       (when (and (> idx 0) (instr? (vector-ref current-ins 0)))
                         (vector-set! current-ins 3 (cons (vector-ref current-ins 3) (- idx 1)))
                         (set! instructions (append instructions (list current-ins)))
                         (set! current-ins-start idx)
                         (set! current-ins (make-vector 4 -1)))
                       (vector-set! current-ins 3 idx)
                       (vector-set! current-ins 0 chr))]
       [(char-numeric? chr) (if (can-have-arg (vector-ref current-ins 0))
                                (let ([n (- (char->integer chr) 48)]
                                      [a (vector-ref current-ins 1)])
                                  (if (< a 0)
                                      (vector-set! current-ins 1 n)
                                      (vector-set! current-ins 1 (+ n (* 10 a)))))
                                (parse-error (cons 'bad-arg current-ins)))]
       [(char-whitespace? chr) '()]
       [else (parse-error (cons 'bad-instruction (cons chr idx)))]))
    (vector-set! current-ins 3
                 (cons (vector-ref current-ins 3) (- (string-length str) 1)))
    (set! instructions (append instructions (list current-ins)))
    instructions))

;; fix arguments
(define (parse-1 instructions)
  (for ([ins instructions])
    (when (< (vector-ref ins 1) 0)
      (case (vector-ref ins 0)
        [(#\+ #\- #\< #\>) (vector-set! ins 1 1)]
        [(#\@) (parse-error (cons 'bad-arg ins))]
        [else '()])))
  instructions)

;; find [] pairs
(define (parse-2 instructions)
  (let ([lefts '()])
    (for ([i (in-range (length instructions))])
      (when (char=? #\[ (vector-ref (list-ref instructions i) 0))
        (set! lefts (cons (list-tail instructions i) lefts)))
      (when (char=? #\] (vector-ref (list-ref instructions i) 0))
        (when (= 0 (length lefts))
          (parse-error (cons 'mismatch-brackets 'redundant-right)))
        (vector-set! (caar lefts) 2 (list-tail instructions i))
        (vector-set! (list-ref instructions i) 2 (car lefts))
        (set! lefts (cdr lefts))))
    (when (not (= 0 (length lefts)))
      (parse-error (cons 'mismatch-brackets 'redundant-left))))
  instructions)

(define (parse str)
  (parse-2 (parse-1 (parse-0 str))))

;; -- execution --

(define (execute-1 ins)
  (case (vector-ref ins 0)
    [(#\+) (ins-plus (vector-ref ins 1))]
    [(#\-) (ins-minus (vector-ref ins 1))]
    [(#\<) (ins-left (vector-ref ins 1))]
    [(#\>) (ins-right (vector-ref ins 1))]
    [(#\,) (ins-comma)]
    [(#\.) (ins-dot)]
    [(#\^) (ins-caret (vector-ref ins 1))]
    [(#\@) (ins-at (vector-ref ins 1))]
    [(#\?) '()]
    [else (runtime-error (cons 'bad-instruction ins))]))

(define (execute instructions)
  (when (not (equal? instructions '()))
    (let ([ins (car instructions)])
      (case (vector-ref ins 0)
        [(#\[) (if (= (current-value) 0)
                   (execute (cdr (vector-ref ins 2)))
                   (execute (cdr instructions)))]
        [(#\]) (if (= (current-value) 0)
                   (execute (cdr instructions))
                   (execute (cdr (vector-ref ins 2))))]
        [else (begin
                (execute-1 ins)
                (execute (cdr instructions)))]))))

(define (step instructions)
  (if (equal? instructions '())
      (cons '#() '())
      (let ([ins (car instructions)])
        (case (vector-ref ins 0)
          [(#\[) (if (= (current-value) 0)
                     (cons ins (cdr (vector-ref ins 2)))
                     (cons ins (cdr instructions)))]
          [(#\]) (if (= (current-value) 0)
                     (cons ins (cdr instructions))
                     (cons ins (cdr (vector-ref ins 2))))]
          [else (begin
                  (execute-1 ins)
                  (cons ins (cdr instructions)))]))))

(define (run str)
  (reset-kbf)
  (execute (parse str)))
