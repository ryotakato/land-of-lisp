(use gauche.sequence)
(use srfi-1) ; reduce

(load "./10_evolution")

;; override draw-world
(define (draw-world)
  (dotimes [y *height*]
    (newline)
    (display "|")
    (dotimes [x *width*]
      (display (cond [(find (^[animal] (and (= (animal-x animal) x)
                                           (= (animal-y animal) y)))
                            *animals*)
                      => (^a (with-color #\M (animal-genes a)))]
                     [(hash-table-get *plants* (cons x y) #f) #\*]
                     [else #\space])))
    (display "|")))


;; this is based on xterm colors (see http://en.wikipedia.org/wiki/ANSI_escape_code)
(define *terminal-colors*
  '(((0 0 0)   "30")
    ((205 0 0) "31")
    ((0 205 0) "32")
    ((205 205 0) "33")
    ((0 0 238)  "34")
    ((205 0 205) "35")
    ((0 205 205) "36")
    ((229 229 229) "37")
    ((127 127 127) "30;1")
    ((255 0 0) "31;1")
    ((0 255 0) "32;1")
    ((255 255 0) "33;1")
    ((92 92 255) "34;1")
    ((255 0 255) "35;1")
    ((0 255 255) "36;1")
    ((255 255 255) "37;1")))


(define (gene-color gene)
  (define (max3-idx l n ml)
    (let1 m (find-max l :key cadr)
      (if (>= n 3)
        ml
        (max3-idx (remove (^x (eq? m x)) l) (+ 1 n) (append ml (list (car m))))
        )
      )
    )
  (map (^x (* (/. x 8) 256)) (max3-idx (map-with-index list gene) 0 '()))
  )

(define (find-closest-terminal-color color)
  (define (distance c1 c2)
    (apply + (map (^p (expt (- (p c1) (p c2)) 2)) (list car cadr caddr))))
  (find-min *terminal-colors* :key (^e (distance (car e) color))))

(define (with-color char gene)
  (let1 entry (find-closest-terminal-color (gene-color gene))
    (format "\u001b[~am~a\u001b[0m" (cadr entry) char)))

;; small spice; generate different patterns for every execution.
(random-source-randomize! default-random-source)

