(define (fixed-point? f x) (= (f x) x))

(define (id x) x) 

(define (branch p? f g x)
        ((if (p? x) f g) x))

(define (sum a b term next)
  (if(> a b) 0 (+ (term a) (sum (next a) b term next))))

(define (prod a b term next)
  (if(> a b) 1 (* (term a) (prod (next a) b term next))))

(define (accumulate op nv a b term next)
  (if (> a b) nv (op (term a) (accumulate op nv (next a) b term next))))

(define (sum a b term next)
        (accumulate + 0 a b term next))


(define (product a b term next)
        (accumulate * 1 a b term next))

(define (1+ x) (+ x 1))

(define (p n x)
        (define (term i) (* (- (1+ n) i) (expt x i)))
        (accumulate + 0 0 n term 1+))

(define (accumulate-i op nv a b term next)
        (if(> a b) nv
            (accumulate-i op (op nv (term a)) (next a) b term next)))
(define (p n x)
  (define (op u v) (+ (* u x) v ))
   (accumulate-i op 0 1 (1+ n) id 1+))

(define (integral a b f dx)
        (* dx (accumulate + 0 a b f (lambda (x) (+ x dx)))))

(define (p n x)
      (accumulate-i (lambda (u v) (+ (* u x) v)) 0 1 (+ n 1) (lambda (x) x) (lambda (x) (+ x 1))))

(define (fact n)
   (accumulate * 1 1 n id 1+))

(define (my-expt x n)
     (if(>= n 0)(accumulate * 1 1 n (lambda (i) x) 1+)
            (/ 1 (accumulate * 1 1 (- n) (lambda (i) x) 1+))))

(define (summ x n)
    (accumulate + 0 0 n (lambda (i) (/ (my-expt x i) (fact i))) 1+))



(define (exists? p? a b)
        (accumulate (lambda (a b) (or a b)) #f a b (lambda (a) (p? a)) 1+))

(define (square x) (* x x))


(define (twice f x) (f (f x)))

(define (twice f) (lambda (x) (f (f x))))

(define (n+ n) (lambda (i) (+ i n)))

(define 1+ (n+ 1))

(define (compose f g) (lambda (x) (f (g x))))

(define (derive f dx)
        (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (repeated f n)
         (lambda (x) (if (= n 0) x (f ((repeated f (- n 1)) x)))))

(define (repeated f n)
         (if (= n 0) id
                         (compose f (repeated f (- n 1)))))

(define (repeated f n)
  (accumulate compose id 1 n (lambda (i) f) 1+))

(define (derive-n f n dx)
   (if (= n 0) f (derive (derive-n f (- n 1) dx) dx)))

(define (derive-n f n dx)
   ((repeated (lambda (f) (derive f dx)) n) f))

(define (derive-n f n dx)
        ((accumulate compose id 1 n (lambda (i) (lambda (f) (derive f dx)))  1+) f))

(define my-#t (lambda (x y) x))


(define my-#f (lambda (x y) y))

(define (lambda-if b x y) ((b x y)))

(define (my-not b) (lambda (x y) (b y x)))

(define (Y gamma)
    (define (gamma-inf me) (lambda (n) ((gamma (me me)) n)))
    (gamma-inf gamma-inf))

