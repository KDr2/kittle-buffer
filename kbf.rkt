#lang racket

(provide parse run step
         set-char-outputer
         BUFFER POINTER-STACK POINTERS)

(define BUFFER-SIZE 50)
(define BUFFER (make-vector BUFFER-SIZE 0))

(define POINTER-STACK '(0))
(define POINTERS (make-hash '((0 . 0))))

(define (output-char chr) (display chr))
(define (set-char-outputer f) (set! output-char f))


;; basic facilities
(define (current-pointer)
  (let ([ptr-idx (car POINTER-STACK)])
    (hash-ref POINTERS ptr-idx)))

(define (current-value)
  (vector-ref BUFFER (current-pointer)))

(define (reset-kbf)
  (set! BUFFER (make-vector BUFFER-SIZE 0))
  (set! POINTER-STACK '(0))
  (set! POINTERS (make-hash '((0 . 0)))))

(define (runtime-error err)
  (raise (cons 'RUNTIME-ERROR err)))

(define (parse-error err)
  (raise (cons 'PARSE-ERROR err)))

;; built-in functions
(define BUILTIN-FUNCTIONS (make-hash))
(hash-set! BUILTIN-FUNCTIONS 0 (lambda (x) x))
(hash-set! BUILTIN-FUNCTIONS 21 +)
(hash-set! BUILTIN-FUNCTIONS 22 -)
(hash-set! BUILTIN-FUNCTIONS 23 *)
(hash-set! BUILTIN-FUNCTIONS 24 /)

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
        [val (char->integer (read-char))])
    (vector-set! BUFFER ptr val)))

;; .
(define (ins-dot)
  (let ([ptr (current-pointer)])
    (output-char (integer->char (vector-ref BUFFER ptr)))))

;; ^
(define (ins-caret val)
  (if (< val 0)
      (set! POINTER-STACK (cdr POINTER-STACK))
      (begin
        (set! POINTER-STACK (cons val POINTER-STACK))
        (when (not (hash-has-key? POINTERS val))
          (hash-set! POINTERS val 0)))))

;; @
(define (ins-at val)
  (if (< (length POINTER-STACK) (+ 2 val))
      (runtime-error 'bad-function-call)
      (let* ([retv-ptr (list-ref POINTER-STACK (+ 1 val))]
             [func-ptr (list-ref POINTER-STACK val)]
             [args-ptr (map (lambda (x) (list-ref POINTER-STACK x)) (build-list val values))]
             [retv 0]
             [func (hash-ref BUILTIN-FUNCTIONS
                             (vector-ref BUFFER (hash-ref POINTERS func-ptr)))]
             [args (map (lambda (x) (vector-ref BUFFER (hash-ref POINTERS x))) args-ptr)])
        (set! retv (apply func args))
        (vector-set! BUFFER (hash-ref POINTERS retv-ptr) retv))))

;; -- parser --

;; instructions is a list of (symbol arg peer pos)

(define (instr? chr)
  (case chr
    [(#\+ #\- #\< #\> #\[ #\] #\, #\.) #t] ;; vanilla BF
    [(#\^ #\@) #t] ;; kbf extension
    [else #f]))

(define (can-have-arg chr)
  (case chr
    [(#\+ #\- #\< #\> #\^ #\@) #t]
    [else #f]))

;; parse instructions
(define (parse-0 str)
  (let ([instructions '()]
        [current-ins-start 0]
        [current-ins (make-vector 4 -1)])
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
       [else (parse-error (cons 'bad-instruction chr idx))]))
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
    (for ([i (in-range (- (length instructions) 1))])
      (when (char=? #\[ (vector-ref (list-ref instructions i) 0))
        (set! lefts (cons (list-tail instructions i) lefts)))
      (when (char=? #\] (vector-ref (list-ref instructions i) 0))
        (vector-set! (caar lefts) 2 (list-tail instructions i))
        (vector-set! (list-ref instructions i) 2 (car lefts))
        (set! lefts (cdr lefts)))))
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
      (cons '() '())
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
