(use srfi-1)
(use gauche.process)

;global variables
(define *wizard-nodes* 
  '((living-room (you are in the living-room.
                      a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
                 there is a well in front of you.))
    (attic (you are in the attic.
                there is a giant welding torch in the corner.))
    )
  )

(define *wizard-edges*
  '((living-room (garden west door)
                 (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder)))
  )

(define *max-label-length* 30)


;functions
(define (dot-name exp)
  (regexp-replace-all #/[^a-zA-Z0-9]/ (write-to-string exp) "_")
  )

(define (dot-label exp)
  (if exp
    (let ((s (write-to-string exp)))
      (if (> (string-length s) *max-label-length*)
        (string-append (substring s 0 (- *max-label-length* 3)) "...")
        s
        )
      )
    ""
    )
  )

(define (nodes->dot nodes)
  (dolist (node nodes)
    (newline)
    (display (dot-name (car node)))
    (display "[label=\"")
    (display (dot-label node))
    (display "\"];")
    )
  )

(define (edges->dot edges)
  (dolist (node edges)
    (dolist (edge (cdr node))
      (newline)
      (display (dot-name (car node)))
      (display "->")
      (display (dot-name (car edge)))
      (display "[label=\"")
      (display (dot-label (cdr edge)))
      (display "\"];")
      )
    )
  )

(define (uedges->dot edges)
  (pair-for-each 
    (lambda (lst)
      (dolist (edge (cdar lst))
        (unless (assv (car edge) (cdr lst))
          (newline)
          (display (dot-name (caar lst)))
          (display "--")
          (display (dot-name (car edge)))
          (display "[label=\"")
          (display (dot-label (cdr edge)))
          (display "\"];")
          )
        )
      )
    edges)
  )

(define (graph->dot nodes edges)
  (display "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (newline)
  (display "}")
  )

(define (ugraph->dot nodes edges)
  (display "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (newline)
  (display "}")
  )

(define (dot->png fname thunk)
  (with-output-to-file fname thunk :if-exists :supersede)
  (run-process `("dot" "-Tpng" "-O" ,fname))
  )

(define (graph->png fname nodes edges)
  (dot->png fname 
    (lambda () (graph->dot nodes edges)))
  )

(define (ugraph->png fname nodes edges)
  (dot->png fname 
    (lambda () (ugraph->dot nodes edges)))
  )

;try
;(print (dot-name 'ab2*c?a!De))
;(print (dot-label 'abcdeabcdeabcdeabcdeabcdeabcd))
;(print (dot-label 'abcdeabcdeabcdeabcdeabcdeabcde))
;(print (dot-label 'abcdeabcdeabcdeabcdeabcdeabcdea))
;(nodes->dot *wizard-nodes*)
;(edges->dot *wizard-edges*)
;(graph->dot *wizard-nodes* *wizard-edges*)
;(graph->png "wizard.dot" *wizard-nodes* *wizard-edges*)
;(uedges->dot *wizard-edges*)
(ugraph->png "uwizard.dot" *wizard-nodes* *wizard-edges*)



