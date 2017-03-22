#lang typed/racket

(require packs)

(struct Probably ([p : Real]))

(: Probably-return (-> Real Probably))
(define (Probably-return r) (Probably r))

(: Probably-bind (-> Probably (-> Real Probably) Probably))
(define (Probably-bind p f)
  (if (> (Probably-p p) 4.5)
      (f (Probably-p p))
      (Probably-return 0.0)))

(: randomness (-> Real Probably))
(define (randomness r)
  (Probably-return (* r (random 1 100))))

(: p Probably)
(define p (do<> (Probably-bind Probably-return)
                (:= [r1 : Real] (randomness 10.2))
                (:= [r2 : Real] (randomness r1))
                (:= [r3 : Real] (randomness r2))
                (:= [r4 : Real] (randomness r3))
                (<< r4)))

