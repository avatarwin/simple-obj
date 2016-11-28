(module simple-obj (Object class-of? object? object-name object-msgs object-vars send)

  (import scheme
          chicken
          simple-exceptions
          lolevel data-structures extras)
  
  (begin-for-syntax
   (import scheme chicken lolevel data-structures simple-exceptions)
   (use srfi-1))

  (use srfi-1)

(define self #f)

(define (make-obj)
  (let [(o (make-vector 6 #f))]
	 (vector-set! o 0 'Object) ;; tag
	 (vector-set! o 1 "Object") ;; class-name ?
	 (vector-set! o 3 '()) ;; msg-list
    (vector-set! o 4 '()) ;; class vars
    (vector-set! o 5 #f) ;; is-instance?
	 o))


(define Object (make-obj)) ;; create the superclass object

(define (class-of? obj ref-obj)
  (unless (and (object? obj) (object? ref-obj))
    'not-an-object)
  (equal? (vector-ref obj 1)
          (vector-ref ref-obj 1)))
 
      
(define (object? o)
  (and (eqv? (vector-ref o 0) 'Object)))


(define (object-name o)
  (vector-ref o 1))

(define (object-msgs o)
  (unless (object? o)
    (raise 'not-an-object))
  (vector-ref o 3))

(define (object-vars o)
  (unless (object? o)
    (raise 'not-an-object))
  (vector-ref o 4))


(begin-for-syntax
(define (object? o)
  (and (eqv? (vector-ref o 0) 'Object)))


(define (object-name o)
  (vector-ref o 1))

(define (object-msgs o)
  (unless (object? o)
    (raise 'not-an-object))
  (vector-ref o 3))

(define (object-vars o)
  (unless (object? o)
    (raise 'not-an-object))
  (vector-ref o 4))

 (define (dispatcher obj msg args)
  (unless (object? obj)
    (raise 'not-an-object))
  (letrec* ((v args)
         (self obj)
         (msg-list (object-msgs obj))
         (m  (assoc msg msg-list)))
    (if m
        (apply (cdr m) args)        
        (list 'no-such-message m))))
)

(define-syntax send
  (syntax-rules (with-args :answer self)
    ((send obj :answer msg args body ...)
     (define-method obj msg args body ...))
    ((send obj msg with-args args)
     (dispatcher obj (quote msg) args))
    ((send obj msg arg1 ...)
     (send obj msg with-args (list arg1 ...)))))
     

(define-syntax define-method
  (syntax-rules (rewrite)
    ((_ rewrite arglist)
     (append (list 'self) arglist))
    ((_ obj msg)
     (begin
       (unless (vector-ref obj 2)
         (vector-set! obj 3
                      (alist-delete (quote msg) (vector-ref obj 3))))))
   
    ((_ obj msg body)
     (begin
       (unless (vector-ref obj 2)
         (vector-set!
          obj 3
          (append (alist-delete (quote msg) (vector-ref obj 3))
                  (list (cons (quote msg)
                              (lambda () (set! self obj) body))))))))
    
    ((_ obj msg arglist body ...)
     (begin
       (unless (vector-ref obj 2)
         (vector-set!
          obj 3
          (append (alist-delete (quote msg) (vector-ref obj 3))
                  (list (cons (quote msg)
                              (lambda arglist (set! self obj) body ...))))))))
    ))


(define (make-show-helper o)
  (let* ((inst? (lambda ()
                  (if (vector-ref self 5)
                      "instance of "
                      "")))                       
         (p (lambda ()
             (display
              (format #f "~Aclass ~A\n   defined messages:\n~A" (inst?) (object-name o)
                      (apply string-append (map (lambda (i) (format #f "     ~A\n" (->string (car i)))) (object-msgs o))))))))
    p))

(define (make-set-helper o var val)
  (let ((p (lambda ()
             (let* [(vars (object-vars o))
                    (v (assoc var (object-vars o)))]             
               (if v
                   (set-cdr! v val)
                   (vector-set! o 4 (append vars (list (cons var val)))))))))
    p))

(define (make-get-helper o var)
  (let ((p (lambda ()
             (let ((v (assoc var (object-vars o))))
               (if v (cdr v) #f)))))
    p))


(define (make-instance-helper o)
  (let ((p (lambda ()
             (let ((inst (make-vector (vector-length o))))
;               (display (format #f "creating instance of ~A...\n" (object-name self)))
               (vector-copy! o inst)
               (vector-set! inst 4 (object-copy (vector-ref self 4)))
               (vector-set! inst 5 #t)
               (send inst :answer :show () ((make-show-helper inst)))
               (send inst :answer :set (var val) ((make-set-helper inst var val)))
               (send inst :answer :get (var)     ((make-get-helper inst var)))
               inst))))
    p))
        
    
(send Object :answer :show ()
      ((make-show-helper self)))

(send Object :answer :get (var)
      (let ((v (assoc var (object-vars self))))
        (if v (cdr v) #f)))

(send Object :answer :set (var val)
      (let* [(vars (object-vars self))
             (v (assoc var (object-vars self)))]             
        (if v
            (set-cdr! v val)
            (vector-set! self 4 (append vars (list (cons var val)))))))

(send Object :answer :new (name)
      (let [(o (make-obj))]
        (vector-set! o 1 (->string name))
        (send o :answer :show () ((make-show-helper o)))
        (send o :answer :get (var) ((make-get-helper o var)))
        (send o :answer :set (var val) ((make-set-helper o var val)))
        (send o :answer :instance () ((make-instance-helper o)))
        o))


(vector-set! Object 2 #t) ;; lock the "Object" object so that it can't be modified
)
