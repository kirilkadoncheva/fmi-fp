(define (dist x1 y1 x2 y2)
  (define dx (- x2 x1))
  (define dy (- y2 y1))
  (define (sq x) (* x x))
  (sqrt (+ (sq dx) (sq dy)))
  )

(define (fact n)
(define (for r i)
  (if(<= i n)(for(* r i) (+ i 1))r))
  (for 1 1)
  )



(define (new-dist x1 y1 x2 y2)
(let ((dx (- x2 x1))
(dy (- y2 y1)))
  (define (sq x) (* x x))
(sqrt (+ (sq dx) (sq dy)))))

(define (area x1 y1 x2 y2 x3 y3)
  (let ((a (dist x1 y1 x2 y2))
    (b (dist x2 y2 x3 y3))
    (c (dist x3 y3 x1 y1))
    )
    (let ((p (/ (+ a b c) 2)))
     (sqrt (* p (- p a) (- p b) (- p c))))
      ))
    
    
(define (area-new x1 y1 x2 y2 x3 y3)
  (let* ((a (dist x1 y1 x2 y2))
        (b (dist x2 y2 x3 y3))
        (c (dist x3 y3 x1 y1))
        (p (/ (+ a b c) 2)))
    (sqrt (* p (- p a) (- p b) (- p c)))))

(define (pow x n)
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (pow x (- n))))
        (else (* x (pow x (- n 1))))))

(define (evenn? x)
   (= (remainder x 2) 0)
          )
(define (sq x) (* x x))

(define (qpow x n)
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (pow x (- n))))
        ((even? n) (sq (qpow x (quotient n 2))))
       (else (* x (qpow x (- n 1))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1) 
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib n)
(define (iter i fi fi-1)
(if (= i n) fi
(iter (+ i 1) (+ fi fi-1) fi)))
(if (= n 0) 0
(iter 1 1 0)))

(define (sum1 k)
  (if (> k 100) 0 (+ (* k k) (sum1 (+ k 1)))))

(define (sum2 a b f dx)
  (if (> a b) 0 (+ (* dx (f a)) (sum2 (+ a dx) b dx))))

(define (sum3 x)
  (if(> x (expt 10 1000)) 0 (+ x (sum3 (exp x)))))

(define (sum a b term next)
  (if(> a b) 0 (+ (term a) (sum (next a) b term next))))

(define (prod a b term next)
  (if(> a b) 1 (* (term a) (prod (next a) b term next))))

(define (accumulate op nv a b term next)
  (if (> a b) nv (op (term a) (accumulate op nv (next a) b term next))))

(define (1+ x) (+ x 1))
(define (id x) x)
(define (con p a b)
     (accumulate (lambda (a b) (and a b)) #t a b (lambda (a) (p a)) 1+))

(define (p n x)
  (define (term i) (* (- (+ n 1) i) (expt x i)))
  (define (next i) (+ i 1))
  (accumulate + 0 0 n term next))


(define (evklid a b)
     (cond ((= a b) a)
           ((> a b)(evklid (- a b) b))
           (else (evklid a (- b a)))))

(define (prime? n)
     (if (= n 1) #f (accumulate (lambda (a b) (and a b) ) #t 2 (- n 1) (lambda (a) (not (= 0 (remainder n a)))) 1+)))


(define (sumdivisors n)
         (accumulate + 0 1 n (lambda (a) (if (= 0 (remainder n a)) 1 0)) (lambda (a) (+ a 1))))