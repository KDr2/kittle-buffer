#lang racket

(require kittle-buffer/interpreter)
(require kittle-buffer/ui)

;; entrance
(define (main)
  (let* ([args (current-command-line-arguments)])
    (if (= 0 (vector-length args))
        (wmain)
        (let ([in (if (string=? (vector-ref args 0) "-")
                      (current-input-port)
                      (open-input-file (vector-ref args 0)))])
          (run (read-string 10240 in))))))

(main)
