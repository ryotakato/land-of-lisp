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



;functions
(define (dot-name exp)
  (regexp-replace-all #/[^a-z0-9]/ (write-to-string exp) "_")
  )


;try
(display (dot-name 'ab2*c?a!de))
