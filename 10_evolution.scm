(use gauche.record)
(use gauche.sequence)
(use srfi-27) ; random-integer
(use srfi-42) ; list-ec


;global variables
(define *width* 100)
(define *height* 30)
(define *jungle* '(45 10 10 10))
(define *plant-energy* 30)



;function
(define *plants* (make-hash-table 'equal?))

(define (random-plant left top width height)
  (let1 pos (cons (+ left (random-integer width)) (+ top (random-integer height)))
    (hash-table-put! *plants* pos #t)
    )
  )

(define (add-plants)
  (if (= (random-integer 2) 0)
    (apply random-plant *jungle*)
    (random-plant 0 0 *width* *height*)
    )
  )


(define-record-type animal #t #t 
  (x) (y) (energy) (dir) (genes))

(define *animals* 
  (list (make-animal (ash *width* -1) (ash *height* -1) 1000 0 (list-ec (: i 8) (+ 1 (random-integer 100))))))


(define (move animal)
  (let 
    ((dir (animal-dir animal))
     (x (animal-x animal))
     (y (animal-y animal))
     )
    (set! (animal-x animal) 
      (modulo (+ x 
              (cond
                ((and (>= dir 2) (< dir 5)) 1)
                ((or (= dir 1) (= dir 5)) 0)
                (else -1)
                )
              ) *width*)
      )
    (set! (animal-y animal) 
      (modulo (+ y 
              (cond
                ((and (>= dir 0) (< dir 3)) -1)
                ((or (>= dir 4) (< dir 7)) 1)
                (else 0)
                )
              ) *height*)
      )
    (dec! (animal-energy animal))
    )
  )

(define (turn animal)
  (let1 x (random-integer (apply + (animal-genes animal)))
    (define (angle genes x)
      (let1 xnu (- x (car genes))
        (if (< xnu 0)
          0
          (+ 1 (angle (cdr genes) xnu))
          )
        )
      )
    (set! (animal-dir animal) (modulo (+ (animal-dir animal) (angle (animal-genes animal) x)) 8))
    )
  )

(define (eat animal)
  (let1 pos (cons (animal-x animal) (animal-y animal))
    (when (hash-table-get *plants* pos #f)
      (inc! (animal-energy animal) *plant-energy*)
      (hash-table-delete! *plants* pos)
      )
    )
  )


(define *reproduction-energy* 200)

(define (reproduce animal)
  (let1 e (animal-energy animal)
    (when (>= e *reproduction-energy*)
      (set! (animal-energy animal) (ash e -1))
      (let 
        ((animal-nu 
           (make-animal (animal-x animal) (animal-y animal) (animal-energy animal) (animal-dir animal) (list-copy (animal-genes animal))))
         (mutation (random-integer 8))
         )
        (update! (~ animal-nu 'genes mutation)
          (^v (max 1 (+ v (* 5 (- (random-integer 9) 4)))))
          )
        (push! *animals* animal-nu)
        )
      )
    )
  )

(define *generation* 1)

(define (update-world)
  (set! *animals* (remove! (^(animal) (<= (animal-energy animal) 0)) *animals*))
  (dolist (animal *animals*) 
    (turn animal)
    (move animal)
    (eat animal)
    (reproduce animal)
    )
  (add-plants)

  (newline)
  (display "G:")
  (display *generation*)
  (display " M:")
  (display (size-of *animals*))
  (display " P:")
  (display (size-of *plants*))
  (inc! *generation*)
  )

(define (draw-world)
  (dotimes (y *height*)
    (newline)
    (display "|")
    (dotimes (x *width*)
      (display 
        (cond 
          ((any (^(animal) (and (= (animal-x animal) x) (= (animal-y animal) y))) *animals*) #\M)
          ((hash-table-get *plants* (cons x y) #f) #\*)
          (else #\space)
          )
        )
      )
    (display "|")
    )
  )

(define (evolution)
  (read-line)
  (let loop ()
    (draw-world)
    (newline)
    (display ">")
    (flush)
    (let1 str (read-line)
      (unless (equal? str "quit")
        (let1 x (string->number str)
          (if x 
            (dotimes (i x)
              (update-world)
              (when (zero? (modulo i 1000))
                (newline)
                (display #\.)
                (flush)
                )
              )
            (update-world)
            )
          )
        (loop)
        )
      )
    )
  )


;try





