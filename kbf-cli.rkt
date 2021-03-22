#lang racket

(require kittle-buffer/interpreter)

;; test

(define (test)
  (let ([hello-world
         "++++++++[>++++[>++>+++>+++>+<<<<-]
          >+>+>->>+[<]<-]>>.>---.+++++++..++
          +.>>.<-.<.+++.------.--------.>>+.>++."]
        [code1 "^0^1>+21^2>>+50^3>>>+50@2^0."])
    (display (parse hello-world))
    (display "\n")
    (run hello-world)
    (display (parse code1))
    (display "\n")
    (run code1)
    (display "\n")))

;; (test)

;; entrance
(define (main)
  (let* ([args (current-command-line-arguments)])
    (let ([in (if (or (= 0 (vector-length args)) (string=? (vector-ref args 0) "-"))
                  (current-input-port)
                  (open-input-file (vector-ref args 0)))])
      (run (read-string 10240 in)))))

(main)