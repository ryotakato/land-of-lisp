(use srfi-13)

(define *small* 1)
(define *big* 100)

(print (string-concatenate (cons "small:" (cons (number->string *small*) '()))))
(print (string-concatenate (cons "big:" (cons (number->string *big*) '()))))



(define (guess-my-number)
  (ash (+ *big* *small*) -1
   )
  )

(define (smaller)
  (set! *big* (- (guess-my-number) 1))
  (guess-my-number)
  )

(define (bigger)
  (set! *small* (+ (guess-my-number) 1))
  (guess-my-number)
  )

(define (start-over)
  (set! *small* 1)
  (set! *big* 100)
  (guess-my-number)
  )

(print (bigger))
(print (smaller))
(print (smaller))
