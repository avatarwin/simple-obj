(module simple-obj (make-obj Object send define-method dispatch)

  (import scheme
          chicken
          simple-exceptions
          lolevel data-structures extras)
  
  (begin-for-syntax
   (import scheme chicken lolevel data-structures simple-exceptions)
   (use srfi-1))

  (use srfi-1)

(define (make-obj)
  (let ((o (make-vector 6 #f)))
	 (vector-set! o 0 'Object) ;; tag
	 (vector-set! o 1 "Object") ;; class-name ?
	 (vector-set! o 3 '()) ;; msg-list
    (vector-set! o 4 '()) ;; class vars
    (vector-set! o 5 #f) ;; is-instance?
	 o))


(define-syntax wrap-self
  (er-macro-transformer
   (lambda (x r c)
     (let* ((self (second x))
           (args (third x))
           (body (fourth x))
           (%lambda (r 'lambda))
           (%list (r 'list))
           (%unquote (r 'unquote))
           (%append (r 'append)))
       `(,%lambda (self)
                  (,%lambda ,args
                            ,body))))))


(define (dispatch obj msg args)
  (let* ((msg-list (vector-ref obj 3))
         (m        (assoc msg msg-list)))
    (if m
        (begin
          (apply ((cdr m) obj) args)))))



(define-syntax define-method
  (syntax-rules ()
    ((_ obj method "args:" (arg ...) "bodies:" body ...)
     (vector-set! obj 3
                  (append
                   (list (cons (quote method)
                               (wrap-self self (arg ...)
                                          body ...)))
                   (filter (lambda (x)
                             (not (and (pair? x)
                                       (equal? (car x)
                                               (quote method)))))
                           (vector-ref obj 3)))))
    ((_ obj method (arg ...) body ...)
     (define-method obj method
       "args:" (arg ...)
       "bodies:" body ...))
    ((_ obj method arglist body ...)
     (define-method obj method
       "args:" arglist
       "bodies:" body ...))    
    ))

(define-syntax send
  (syntax-rules (:answer)
    ((send obj :answer msg args body ...)
     (define-method obj msg args body ...))
    ((send obj msg args ...)
     (dispatch obj (quote msg) (list  args ...)))
    ((send obj msg)
     (send obj msg '()))
    ))

(define Object (make-obj))

(send Object :answer :copy ()
      (object-copy self))
      

(send Object :answer :rename (new-name)
      (vector-set! self 1 new-name))

(send Object :answer :get (var)
      (let ((v (assoc var (vector-ref self 4))))
        (if v (cdr v) #f)))

(send Object :answer :set (var val)
  (let* ((vars (vector-ref self 4))
         (v    (assoc var vars)))
    (if v
        (set-cdr! v val)
        (vector-set! self 4
                     (append vars (list
                                   (cons var val))))
        )))

(send Object :answer :instance ()
  (let ((i (send self :copy)))
    (send i :setup)
    i ))


  
  
)
