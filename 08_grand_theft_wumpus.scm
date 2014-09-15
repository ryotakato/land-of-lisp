(use srfi-27)
(use srfi-42)

(load "./07_graph_util")

;global variables
(define *congestion-city-nodes* '())
(define *congestion-city-edges* '())
(define *visited-nodes* '())
(define *node-num* 30)
(define *edge-num* 45)
(define *worm-num* 3)
(define *cop-odds* 15)
(define *player-pos* 1)


;function
(define (random-node)
  (+ 1 (random-integer *node-num*))
  )

(define (edge-pair a b)
  (if (eqv? a b)
    '()
    (list (cons a b) (cons b a))
    )
  )

(define (make-edge-list)
  (append-ec (: i *edge-num*) (edge-pair (random-node) (random-node)))
  )


(define (direct-edges node edge-list)
  (let ((careq? (lambda (x) (eqv? (car x) node))))
    (filter careq? edge-list)
    )
  )

(define (get-connected node edge-list)
  (let1 visited '() 
    (define (traverse node)
      (unless (member node visited)
        (push! visited node)
        (for-each (lambda (edge) (traverse (cdr edge))) (direct-edges node edge-list))
        )
      )
    (traverse node)
    visited
    )
  )

(define (find-islands nodes edge-list)
  (let1 islands '()
    (define (find-island nodes)
      (cond 
        ((pair? nodes)
          (let* 
            ((connected (get-connected (car nodes) edge-list))
             (unconnected (lset-difference eqv? nodes connected)))
            (push! islands connected)
            (cond (unconnected (find-island unconnected)))
            )
          )
        )
      )
    (find-island nodes)
    islands
    )
  )

(define (connect-with-bridges islands)
  (cond
    ((null? (cdr islands)) '())
    (else
      (append (edge-pair (caar islands) (caadr islands)) (connect-with-bridges (cdr islands)))
      )
    )
  )

(define (connect-all-islands nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list)
  )

(define (make-city-edges)
  (let* 
    ((nodes (iota *node-num* 1))
     (edges (connect-all-islands nodes (make-edge-list)))
     (cops (filter (lambda (x) (zero? (random-integer *cop-odds*))) edges)))
    (add-cops (edges-to-alist edges) cops)
    )
  )

(define (edges-to-alist edge-list)
  (define (traverse nodes edge-alist)
    (cond 
      ((null? nodes) edge-alist)
      (else 
        (push! edge-alist (cons (car nodes) (map (^x (list (cdr x))) (delete-duplicates (direct-edges (car nodes) edge-list)))))
        (traverse (cdr nodes) edge-alist)
        )
      )
    )
  (traverse (reverse (delete-duplicates (map car edge-list))) '())
  )

(define (add-cops edge-alist edges-with-cops)
  (map 
    (^x 
      (let 
         ((node1 (car x))
          (node1-edges (cdr x)))
         (cons node1 
           (map 
             (^ (edge) 
               (let ((node2 (car edge)))
                  (if (null? (lset-intersection equal? (edge-pair node1 node2) edges-with-cops))
                    edge
                    (list (car edge) 'cops)))
                  ) 
             node1-edges))
         )
      )
    edge-alist)
  )




(define (neighbors node edge-alist)
  (map car (assoc-ref edge-alist node))
  )

(define (within-one a b edge-alist)
  (memv b (neighbors a edge-alist))
  )

(define (within-two a b edge-alist)
  (or (within-one a b edge-alist)
    (any (^x (within-one x b edge-alist)) (neighbors a edge-alist)))
  )


(define (make-city-nodes edge-alist)
  (let 
    ((wumpus (random-node))
     (glow-worms (let loop ((x 0) (y '()))
                   (if (= x *worm-num*) y (loop (+ x 1) (cons (random-node) y)))))
     )
    (let loop ((n *node-num*) (y '()))
      (if (= n 0)
        y
        (loop (- n 1) 
          (cons (append (list n)
            (cond 
              ((eqv? n wumpus) '(wumpus))
              ((within-two n wumpus edge-alist) '(blood!))
              (else '())
              )
            (cond 
              ((memv n glow-worms) '(glow-worm))
              ((any (cut within-one n <> edge-alist) glow-worms) '(lights!))
              (else '())
              )
            (cond-list
              ((any (^e (not (null? (cdr e)))) (assv-ref edge-alist n '())) 'sirens!)
              )
          ) y))
        )
      )
    )
  )


(define (find-empty-node)
  (let1 x (random-node)
    (if (not (null? (assv-ref *congestion-city-nodes* x)))
      (find-empty-node)
      x
      )
    )
  )


(define (draw-city)
  (ugraph->png "city.dot" *congestion-city-nodes* *congestion-city-edges*)
  )


(define (known-city-nodes)
  (map 
    (^(node) 
      (if (memv node *visited-nodes*)
        (let1 n (assv node *congestion-city-nodes*)
          (if (eqv? node *player-pos*)
            (append n '(*))
            n)
          )
        (list node '?))) 
    (delete-duplicates 
      (append *visited-nodes* 
        (append-map (^(node) (neighbors node *congestion-city-edges*)) *visited-nodes* )))
    )
  )

(define (known-city-edges)
  (map 
    (^(node) 
      (cons node 
        (map 
          (^x 
            (if (memv x *visited-nodes*)
              (x)
              (list (car x))
              )) 
          (cdr (assv node *congestion-city-edges*))))
      ) 
    *visited-nodes*)
  )

(define (draw-known-city)
  (ugraph->png "known-city.dot" (known-city-nodes) (known-city-edges))
  )

(define (new-game)
  (set! *congestion-city-edges* (make-city-edges))
  (set! *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (set! *player-pos* (find-empty-node))
  (set! *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city)
  )

(define (walk pos)
  (handle-direction pos #f)
  )

(define (charge pos)
  (handle-direction pos #t)
  )

(define (handle-direction pos charging)
  (if-let1 edge (assv pos (cdr (assv *player-pos* *congestion-city-edges*)))
    (handle-new-place edge pos charging)
    (display "That location does not exist!")
    )
  )

(define (handle-new-place edge pos charging)
  (let* 
    ((node (assv pos *congestion-city-nodes*))
     (has-worm (and (memv 'glow-worm node)
                 (not (memv pos *visited-nodes*))))
     )
    (unless (memv pos *visited-nodes*) (push! *visited-nodes* pos))
    (set! *player-pos* pos)
    (draw-known-city)
    (cond
      ((memv 'cops edge) (display "You ran into the cops. Game Over."))
      ((memv 'wumpus node) 
       (if charging 
         (display "You found the Wumpus!")
         (display "You ran into the Wumpus")
         ))
      (charging (display "You wasted your last bullet. Game Over."))
      (has-worm (let1 new-pos (random-node)
                  (display "You ran into a Glow Worm Gang! You're now at ")
                  (display new-pos)
                  (handle-new-place '() new-pos #f)
                  ))
      )

    )
  )


;try
;(print (make-edge-list))
;(print (list-ec (: i 5) (* i i)))
;(print (direct-edges 2 (make-edge-list)))
;(print (get-connected 2 (make-edge-list)))
;(print (connect-all-islands (iota *node-num* 1) (make-edge-list)))

;(print (edges-to-alist (make-edge-list)))

;(define a (make-city-edges))
;(print a)
;(print (neighbors 10 a))
;(print (within-one 10 20 a))
;(print (within-two 10 20 a))
;(print (make-city-nodes a))
;(new-game)

;(new-game)

;(print (known-city-nodes))
;(print ' == )
;(print (known-city-edges))


