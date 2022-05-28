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
(struct instruction (symbol arg peer pos)
        #:mutable #:transparent)

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
        [current-ins (instruction -1 -1 -1 -1)]
        [str (remove-bang str)])
    (for ([chr str]
          [idx (in-range (string-length str))])
         (cond
          [(instr? chr) (begin
                          (when (and (> idx 0) (instr? (instruction-symbol current-ins)))
                            (set-instruction-pos! current-ins (cons (instruction-pos current-ins) (- idx 1)))
                            (set! instructions (append instructions (list current-ins)))
                            (set! current-ins-start idx)
                            (set! current-ins (instruction -1 -1 -1 -1)))
                          (set-instruction-pos! current-ins idx)
                          (set-instruction-symbol! current-ins chr))]
          [(char-numeric? chr) (if (can-have-arg (instruction-symbol current-ins))
                                   (let ([n (- (char->integer chr) 48)]
                                         [a (instruction-arg current-ins)])
                                     (if (< a 0)
                                         (set-instruction-arg! current-ins n)
                                         (set-instruction-arg! current-ins (+ n (* 10 a)))))
                                   (parse-error (cons 'bad-arg current-ins)))]
          [(char-whitespace? chr) '()]
          [else (parse-error (cons 'bad-instruction (cons chr idx)))]))
    (set-instruction-pos! current-ins
                          (cons (instruction-pos current-ins) (- (string-length str) 1)))
    (set! instructions (append instructions (list current-ins)))
    instructions))

;; fix arguments
(define (parse-1 instructions)
  (for ([ins instructions])
       (when (< (instruction-arg ins) 0)
         (case (instruction-symbol ins)
           [(#\+ #\- #\< #\>) (set-instruction-arg! ins 1)]
           [(#\@) (parse-error (cons 'bad-arg ins))]
           [else '()])))
  instructions)

;; find [] pairs
(define (parse-2 instructions)
  (let ([lefts '()])
    (for ([i (in-range (length instructions))])
         (when (char=? #\[ (instruction-symbol (list-ref instructions i)))
           (set! lefts (cons (list-tail instructions i) lefts)))
         (when (char=? #\] (instruction-symbol (list-ref instructions i)))
           (when (= 0 (length lefts))
             (parse-error (cons 'mismatch-brackets 'redundant-right)))
           (set-instruction-peer! (caar lefts) (list-tail instructions i))
           (set-instruction-peer! (list-ref instructions i) (car lefts))
           (set! lefts (cdr lefts))))
    (when (not (= 0 (length lefts)))
      (parse-error (cons 'mismatch-brackets 'redundant-left))))
  instructions)

(define (parse str)
  (parse-2 (parse-1 (parse-0 str))))

;; -- execution --

(define (execute-1 ins)
  (case (instruction-symbol ins)
    [(#\+) (ins-plus (instruction-arg ins))]
    [(#\-) (ins-minus (instruction-arg ins))]
    [(#\<) (ins-left (instruction-arg ins))]
    [(#\>) (ins-right (instruction-arg ins))]
    [(#\,) (ins-comma)]
    [(#\.) (ins-dot)]
    [(#\^) (ins-caret (instruction-arg ins))]
    [(#\@) (ins-at (instruction-arg ins))]
    [(#\?) '()]
    [else (runtime-error (cons 'bad-instruction ins))]))

(define (execute instructions)
  (when (not (equal? instructions '()))
    (let ([ins (car instructions)])
      (case (instruction-symbol ins)
        [(#\[) (if (= (current-value) 0)
                   (execute (cdr (instruction-peer ins)))
                   (execute (cdr instructions)))]
        [(#\]) (if (= (current-value) 0)
                   (execute (cdr instructions))
                   (execute (cdr (instruction-peer ins))))]
        [else (begin
                (execute-1 ins)
                (execute (cdr instructions)))]))))

(define (step instructions)
  (if (equal? instructions '())
      (cons '#() '())
      (let ([ins (car instructions)])
        (case (instruction-symbol ins)
          [(#\[) (if (= (current-value) 0)
                     (cons ins (cdr (instruction-peer ins)))
                     (cons ins (cdr instructions)))]
          [(#\]) (if (= (current-value) 0)
                     (cons ins (cdr instructions))
                     (cons ins (cdr (instruction-peer ins))))]
          [else (begin
                  (execute-1 ins)
                  (cons ins (cdr instructions)))]))))

(define (run str)
  (reset-kbf)
  (execute (parse str)))
