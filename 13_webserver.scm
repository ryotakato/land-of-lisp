
(use gauche.net)
(use srfi-13)     ; string-trim
(use util.match)
(use rfc.822)     ; rfc822-header->list
(use rfc.uri)     ; uri-parse
(use text.parse)  ; read-string
(use www.cgi)     ; cgi-parse-parameters



; main handler
(define (serve request-handler)
  (let1 server-sock (make-server-socket 'inet 8080 :reuse-addr? #t)
    ; try-finary for server-sock
    (unwind-protect 
      ; waiting for request
      (let loop ([client (socket-accept server-sock)])
        ; try-finary for client
        (unwind-protect 
          (if-let1 q (parse-request (socket-input-port client))
            (with-output-to-port (socket-output-port client)
              ; call request-handler
              (cut apply request-handler q))
            )
          (socket-close client))
        (loop (socket-accept server-sock))
        )
      (socket-close server-sock))))

; parse request
(define (parse-request iport)
  (rxmatch-case (read-line iport)
    [#/^(GET|POST)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ meth abs-path)
      (receive (scheme info host port path query flag) (uri-parse abs-path)
        (let* ([hdrs (map (^p (cons (string->symbol (car p))
                                    (x->string (cadr p))))
                          (rfc822-header->list iport))]
               [body (if-let1 p (assq-ref hdrs 'content-length)
                       (string-trim-both (read-string (x->integer p) iport)) "")]
               [params (map (^p (cons (string->symbol (car p))
                                      (x->string (cadr p))))
                            (cgi-parse-parameters :query-string (or query body)))]
               )
          (list (string-trim path #[/])
                hdrs params)
          )
        )
     ]
    [else #f]))

(define (hello-request-handler path header params)
  (if (equal? path "greeting")
    (if-let1 name (assoc 'name params)
      (format #t "Nice to meet you, ~a!" (cdr name))
      (print "<html><form>What is yout name?<input name='name' /></form></html>")
      )
    (print "Sorry... I don't know that page.")
    )
  )


(serve hello-request-handler)

