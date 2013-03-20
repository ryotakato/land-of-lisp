(use srfi-1)

;global variables
(define *nodes* 
  '((living-room (you are in the living-room.
                      a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
                 there is a well in front of you.))
    (attic (you are in the attic.
                there is a giant welding torch in the corner.))
    )
  )

(define *edges*
  '((living-room (garden west door)
                 (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder)))
  )

(define *objects* 
  '(whiskey bucket frog chain))

(define *object-locations* 
  '((whiskey living-room)
    (bucket living-room)
    (chain garden)
    (frog garden)
  ))

(define *location* 'living-room)


;functions
(define (describe-location location nodes)
  (cadr (assoc location nodes))
  )

(define (describe-path edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.)
  )

(define (describe-paths location edges)
  (apply append (map describe-path (cdr (assoc location edges))))
  )

(define (objects-at loc objs obj-locs)
  (let ((at-loc? (lambda (obj) (eq? loc (cadr (assoc obj obj-locs))))))
      (filter at-loc? objs)
    )
  )

(define (describe-objects loc objs obj-locs)
  (let ((describe-obj (lambda (obj) `(you see a ,obj on the floor.))))
     (apply append (map describe-obj (objects-at loc objs obj-locs)))
   )
  )

;cmd
(define (look)
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*))
  )

(define (walk direction)
   (define (correct-way edge) (eq? direction (cadr edge)))
   (if-let1 next (find correct-way (cdr (assoc *location* *edges*)))
     (begin
       (set! *location* (car next))
       (look))
     '(you cannot go that way.))
  )

(define (pickup object)
  (cond 
    ((member object (objects-at *location* *objects* *object-locations*))
     (push! *object-locations* (list object 'body))
     `(you are now carring the ,object)
     )
    (else '(you connot get that.))
   )
  )

(define (inventory)
  (cons 'items- (objects-at 'body *objects* *object-locations*))
  )


(define (say-hello)
  (print "Please type your name:")
  (let ((name (read-line)))
    (print "Nice to meet you, ")
    (print name)
    )
  )

(define (game-repl)
  (let ((cmd (game-read)))
    (unless (eq? (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl)
      )
    )
  )

(define (game-read)
  (let ((cmd (read-from-string (string-append "(" (read-line) ")" )))
        (quote-it (lambda (x) (list 'quote x)))
        )
    (if (null? cmd) 
      (game-read)
      (cons (car cmd) (map quote-it (cdr cmd))))
    )
  )

(define *allowed-commands* '(look walk pickup inventory))

(define (game-eval sexp)
  (if (member (car sexp) *allowed-commands*)
    (eval sexp (current-module))
    '(i do not know that command.)
    )
  )


(define (tweak-text lst caps lit)
  (if (null? lst)
    '()
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond 
        ((eqv? item #\space) (cons item (tweak-text rest caps lit)))
        ((member item '(#\! #\? #\.)) (cons item (tweak-text rest #t lit)))
        ((eqv? item #\") (tweak-text rest caps (not lit)))
        (lit (cons item (tweak-text rest #f lit)))
        (caps (cons (char-upcase item) (tweak-text rest #f lit)))
        (else (cons (char-downcase item) (tweak-text rest #f #f)))
        )
      )
    )
  )

(define (game-print lst)
  (display (list->string (tweak-text (string->list (string-trim-both (write-to-string lst) #[() ] )) #t #f)))
  (newline)
  )


;try
;(print (describe-location 'garden *nodes*))
;(print (describe-path '(garden west door)))
;(print (describe-paths 'living-room *edges*))
;(print (objects-at 'attic *objects* *object-locations*))
;(print (describe-objects 'living-room *objects* *object-locations*))
;(say-hello)
;(print (look))
;(print '------------)
;(print (walk 'west))
;(print '------------)
;(print (walk 'east))
;(print '------------)
;(print (walk 'upstairs))
;(print '------------)
;(print (walk 'downstairs))
;(print '------------)
;(print (pickup 'bucket))
;(print '------------)
;(print (pickup 'whiskey))
;(print '------------)
;(print (inventory))


