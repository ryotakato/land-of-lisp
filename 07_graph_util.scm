 (use srfi-13)


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




;try
;(print (dot-name 'ab2*c?a!De))
;(print (dot-label 'abcdeabcdeabcdeabcdeabcdeabcd))
;(print (dot-label 'abcdeabcdeabcdeabcdeabcdeabcde))
;(print (dot-label 'abcdeabcdeabcdeabcdeabcdeabcdea))
(nodes->dot *wizard-nodes*)



