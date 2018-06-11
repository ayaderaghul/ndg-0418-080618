#lang racket
(require racket/hash plot)

(require "auto.rkt" "main.rkt" "inout.rkt" "cons.rkt")

(provide (all-defined-out))

(define (generate-population t m c)
  (define cs (build-vector c (lambda (_) (C))))
  (define ms (build-vector m (lambda (_) (M))))
  (define ts (build-vector t (lambda (_) (T2))))
  (define hs (build-vector (- 1000 c m t) (lambda (_) (H))))
  (vector-append cs ms ts hs))

(define (scan-for-TM pop)
  (define rank (scan pop))
  (list
   (hash-ref rank (T2) 0)
   (hash-ref rank (M) 0)))

(define TESTS
  (list
   (list 100 100 100)
   (list 100 200 100)
   (list 100 300 100)

   (list 100 400 100)
   (list 200 100 100)
   (list 200 200 100)
   
   (list 200 300 100)
   (list 300 300 100)
   (list 300 400 100)
  
   (list 300 500 100)
   (list 400 500 50)
   (list 400 400 100)
   ))

(define REP (gen-name-rep LOCATION 2 "rep.png"))

(define (evolve-t population cycles speed coor-file)
  (cond
   [(zero? cycles) (out-data coor-file (list (scan-for-TM population)))]
   [else
    (out-data coor-file (list (scan-for-TM population)))
    (define p2 (match-population population))
    (define pp (population-payoffs p2))
    (define p3 (regenerate p2 speed))
    (define p4 (vector-map reset p3))
    (evolve-t p4 (- cycles 1) speed coor-file)]))

(define (plot-rep files)
  (define lines-list
    (for/list ([i (in-list files)])
      (lines (input->coors (csvfile->list i)))))
  (plot lines-list #:x-min 0 #:x-max 1000 #:y-min 0 #:y-max 1000 
        #:x-label "pT" #:y-label "pM" #:title "replicator dynamics: NDG"
        #:out-file REP
        ))
(define T (list 0 1 2 3 4 5 6 7 8))
(define FILES
  (for/list ([i (in-list T)])
    (gen-name-rep LOCATION 2 (number->string i))))
(define (main)
  (collect-garbage)
  (for ([i (in-list T)]
        [n (in-list FILES)])
    (define pop (apply generate-population (list-ref TESTS i)))    
    (evolve-t pop 1000 10 n))
  (plot-rep FILES))
