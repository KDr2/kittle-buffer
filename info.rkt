#lang info
(define collection "kittle-buffer")
(define version "0.2.1")
(define pkg-desc "Kittle-Buffer, an extended Brainf**k implementation.")
(define scribblings '(("kbf.scrbl")))
(define pkg-authors '(Killian))
(define license '(Apache-2.0 OR MIT))

(define deps '("base" "gui" "draw" "srfi"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define test-omit-paths '("kbf.rkt" "kbf-cli.rkt"))
