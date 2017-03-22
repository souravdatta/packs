#lang typed/racket

(require packs)

(: p-a (-> Char (Pack Char)))
(define (p-a c)
  (if (char=? c #\a)
      (return c)
      (return #f)))

(: p-ab (-> Char Char (Pack Char)))
(define (p-ab p c)
  (if (and (char=? c #\b) (char=? p #\a))
      (return c)
      (return #f)))

(: p-abc (-> Char Char Char (Pack Char)))
(define (p-abc p1 p2 c)
  (if (and (char=? c #\c) (char=? p2 #\b) (char=? p1 #\a))
      (return c)
      (return #f)))

(: parse-abc (-> String (Pack Char)))
(define (parse-abc str)
  (do+ 
   [:= (c1 : Char) (p-a (string-ref str 0))]
   [:= (c2 : Char) (p-ab c1 (string-ref str 1))]
   [:= (c3 : Char) (p-abc c1 c2 (string-ref str 2))]
   (<< c3)))

(: p-aba (-> Char Char Char (Pack Char)))
(define (p-aba p1 p2 c)
  (if (and (char=? c #\a) (char=? p2 #\b) (char=? p1 #\a))
      (return c)
      (return #f)))

(: parse-aba (-> String (Pack Char)))
(define (parse-aba str)
  (do+ 
   [:= (c1 : Char) (p-a (string-ref str 0))]
   [:= (c2 : Char) (p-ab c1 (string-ref str 1))]
   [:= (c3 : Char) (p-aba c1 c2 (string-ref str 2))]
   (<< c3)))

(: p-last (-> Char Char (Pack Symbol)))
(define (p-last p1 c)
  (if (and (or (char=? p1 #\c)
               (char=? p1 #\a))
           (char=? c #\.))
      (return 'ok)
      (return #f)))

(: parse-example (-> String (Pack Symbol)))
(define (parse-example s)
  (do+
   (:= [p1 : Char] (Pack-or (parse-abc s) (parse-aba s)))
   (:= [p2 : Symbol] (p-last p1 (string-ref s 3)))
   (<< p2)))
