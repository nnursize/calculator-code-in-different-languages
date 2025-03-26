#lang racket

(define var-env (make-hash))

(define (evaluate expr)

  (cond
    ((number? expr) expr)

    ((symbol? expr) 
     (if (hash-has-key? var-env expr)
         (hash-ref var-env expr)
         (error "Unbound variable" expr)))

    ((pair? expr)
     
  (let ((op (car expr)))

       (cond
         [(eq? op 'assign)
          (let ((var (cadr expr))
                (val-expr (caddr expr)))

            (if (symbol? var)
                (let ((val (evaluate val-expr)))
                  (hash-set! var-env var val)
                  val)
                (error "Invalid variable" var)))]

         [(eq? op '+) 
          (+ (evaluate (cadr expr)) (evaluate (caddr expr)))]
         [(eq? op '-)
          (- (evaluate (cadr expr)) (evaluate (caddr expr)))]
         [(eq? op '*) 
          (* (evaluate (cadr expr)) (evaluate (caddr expr)))]
         [(eq? op '/)
          (let ((divisor (evaluate (caddr expr))))
            (if (zero? divisor)
                (error "Division by zero")
                (/ (evaluate (cadr expr)) divisor)))]
         [else (error "Unknown operator" op)]
        
        )
      )
      )
    (else (error "Invalid expression" expr))
    )
  )


(define (main)
  (display "Calculator (type 'exit to quit)\n")

  (let loop ()
    (display "> ")
    (flush-output)
    
    (let ((input (read)))
      
      (cond
       ((eof-object? input) (display "Exiting Calculator\n"))
       ((and (symbol? input) (eq? input 'exit))
        (display "Exiting Calculator\n"))
       (else
        (with-handlers ([exn:fail? (lambda (exn)
                                     (display "Error: ")
                                     (display (exn-message exn))
                                     (newline)
                                     (loop))])
          (let ((result (evaluate input)))
               (display "Result: ")
               (display result)
               (newline)
               (loop))
         )
        )
      )

    )
   )
 )

(main) 
