#lang info
(define collection "packs")
(define deps '("base"
               "rackunit-lib"
               "typed-racket-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/packs.scrbl" ())))
(define pkg-desc "A pack of Monads")
(define version "0.0.1")
(define pkg-authors '(soura.jagat@gmail.com))
