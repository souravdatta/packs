#lang typed/racket

(require (for-syntax syntax/parse))

(provide (struct-out Pack)
         Pack-Content
         return
         bind
         pmap
         pfail?
         Pack-or
         do+
         do<>
         PackF
         comp+
         pmap/pack)

(define-type (Pack-Content a) (U a False))

(struct (a) Pack ([content : (Pack-Content a)]))

(: return (All (a) (-> (Pack-Content a) (Pack a))))
(define (return x) (Pack x))

(: pmap (All (a b) (-> (Pack a) (-> a b) (Pack b))))
(define (pmap mond f)
  (let ([v (Pack-content mond)])
    (if v
        (return (f v))
        (return #f))))

(: bind (All (a b) (-> (Pack a) (-> a (Pack b)) (Pack b))))
(define (bind bx f)
  (let ([c : (Pack-Content a) (Pack-content bx)])
    (if c
        (f c)
        (return #f))))

(: pfail? (All (a) (-> (Pack a) Boolean)))
(define (pfail? m)
  (false? (Pack-content m)))

(: Pack-or (All (a) (-> (Pack a)
                        (Pack a)
                        (Pack a))))
(define (Pack-or m1 m2)
  (if (pfail? m1)
      m2
      m1))

(define-syntax (do+ stx)
  (syntax-parse stx
    ([_ ((~literal :=) (var-decl:id : var-type:expr) val-decl:expr) e2 ...]
     #'(bind val-decl (λ ([var-decl : var-type]) (do+ e2 ...))))
    ([_ ((~literal <<) val-decl:expr)]
     #'(return val-decl))))

(define-syntax (do<> stx)
  (syntax-parse stx
    ([_ (mond-binder:id mond-return:id) ((~literal :=) (var-decl:id : var-type:expr) val-decl:expr) e2 ...]
     #'(mond-binder val-decl (λ ([var-decl : var-type]) (do<> (mond-binder mond-return) e2 ...))))
    ([_ (mond-binder:id mond-return:id) ((~literal <<) val-decl:expr)]
     #'(mond-return val-decl))))

(define-type (PackF a) (-> a (Pack a)))
	 
(: comp+ (All (a) (->* ((PackF a)) () #:rest (PackF a)
                       (PackF a))))
(define (comp+ f1 . restfs)
  (lambda ([x : a])
    (call/cc (lambda ([ret : (-> (Pack a) Void)])
               (for ([f : (PackF a) (cons f1 restfs)])
                 (let* ([result (f x)]
                        [result-content (Pack-content result)])
                   (if (not result-content)
                       (ret (return #f))
                       (set! x result-content))))
               (return x)))))

(: pmap/pack (All (a b)
                 (-> (PackF a) (Sequenceof a) (Sequenceof (Pack a)))))
(define (pmap/pack f seq)
  (sequence-map f seq))

			   

