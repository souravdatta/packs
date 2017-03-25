#lang typed/racket

(require packs)


(define-type Bag (Listof Symbol))

(define bad-items '(pistol gun powder battery liquid))

(: scan-bag (-> Bag (Pack Bag)))
(define (scan-bag b)
  (if (for/and : Boolean ([x b])
        (not (member x bad-items)))
      (return (cons '<scanned> b))
      (return #f)))

(: tag-bag (-> Symbol Bag (Pack Bag)))
(define (tag-bag dest b)
  (if (eqv? dest 'mordor)
      (return #f)
      (return (cons '<tagged> b))))

(: process-bag (-> Symbol (-> Bag (Pack Bag))))
(define (process-bag for-country)
  (comp+ scan-bag (curry tag-bag for-country)))

(: move-to-loading (PackF Bag))
(define (move-to-loading b)
  (return (cons '<loaded> b)))

(: retrieve-tag (PackF Bag))
(define (retrieve-tag b)
  (return (cons '<retrieved> b)))
           

(define process (comp+ (process-bag 'uae)
                       move-to-loading
                       retrieve-tag))

(define p1 (pmap/pack process '((a b) (battery c) (c p))))

(define p2 (pmap/pack process (in-vector #{'#((a b) (battery c) (c p)) :: (Vectorof Bag)})))

