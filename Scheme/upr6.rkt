(load "/Documents/uni/2 курс/ФП/check.rkt")

(define (id x) x)
(define (compose-simple f g)
     (lambda (x) (f (g x))))
(define (compose . fns)
     (if (null? fns)
         id
         (compose-simple (car fns) (apply compose (cdr fns)))))

(define (double x) (* 2 x))
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (flip fn)
  (lambda args (apply fn (reverse args))))


(define (zip x y)
    (if (or (null? x) (null? y))
        '()
        (cons (list (car x) (car y)) (zip (cdr x) (cdr y)))))

(define (zip-with fn x y)
   (if (or (null? x) (null? y))
        '()
        (cons (fn (car x) (car y)) (zip-with fn (cdr x) (cdr y)))))

(define (every? p l)
   (if(null? l)
      #t
      (and (p (car l)) (every? p (cdr l)))))

(define (any? p l)
   (not (every? (lambda (x) (not (p x))) l)))

(define (zip-with* fn . ls)
      (if (or (any? null? ls) (null? ls))
          '()
          (cons (apply fn (map car ls)) (apply zip-with* fn (map cdr ls)))))

(define (juxt-one . fns)
  
  (lambda (x) (map (lambda (fn) (fn x)) fns)))

(define (juxt . fns)
  (lambda args (map (lambda (fn) (apply fn args)) fns)))

(define (my-length l)
   (if (null? l)
       0
       (+ 1 (my-length (cdr l)))))

(define (dimensions matrix)
   (cons (my-length matrix) (my-length (car matrix))) )

(define (reverse-columns matrix)
  (map reverse matrix))

(define (nth-element l n)
         (define (for matrix counter)
           (if (null? matrix)
               '()
               (if (= counter n)
                   (car matrix)
                   (for (cdr matrix) (+ 1 counter)))))
  (for l 0))

(define (nth-column matrix n)
    (map (lambda (x) (nth-element x n)) matrix))

(define (interval a b)
   (if (> a b)
       '()
       (cons a (interval (+ a 1) b))))

(define (main-diagonal matrix)
  (define indeces (interval 0 (- (my-length matrix) 1)))
  (map (lambda (x i) (nth-element x i)) matrix indeces))

(define (transpose matrix)
  (define l (interval 0 (- (my-length (car matrix)) 1)))
     (map (lambda (i) (nth-column matrix i)) l))

(define (for-all-columns? p matrix)
  (if (null? matrix)
      #t
       (and (every? p (car matrix)) (for-all-columns? p (cdr matrix))))
   )



(define (odd-exists? l)
  (any? odd? l))