#lang racket

(require kittle-buffer/kbf)

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
(define (read-source)
  (let* ([args (current-command-line-arguments)]
         [in (if (vector-empty? args)
                 (current-input-port)
                 (open-input-file (vector-ref args 0)))])
    (read-string 10240 in)))

(run (read-source))
