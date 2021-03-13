(define (my-length l)
  (if(null? l) 0 (+ 1 (my-length (cdr l)))))

(define (sum l)
  (if (null? l) 0 (+ (car l) (sum (cdr l)))))

(define l (list 1 2 3 4 5 6 -7))

(define (member? x l)
  (cond ((null? l) #f)
        ((equal? (car l) x) #t)
        (else (member? x (cdr l)))))

(define (last l)
        (cond ((null? l) (list))
              ((= (my-length l) 1) (car l))
              (else (last (cdr l)))))

(define (nth l n)
        (define (for i l)
        (if (= i n) (if (not (null? l))(car l) (list)) (if (not (null? l))(for (+ i 1) (cdr l)) (list))))
  (if (null? l) (list) (for 0 l)))

(define (scale l x)
        (if (null? l) '() (cons (* x (car l)) (scale (cdr l) x))))

(define (my-map f l)
        (if (null? l) '() (cons (f (car l)) (my-map  f (cdr l)))) )

(define (my-filter p l)
        (if (null? l) '() (if (p (car l))(cons  (car l) (my-filter  p (cdr l))) (my-filter  p (cdr l)))) )

(define (reject p l)
         (my-filter (lambda (x) (not (p x))) l))

(define (foldr combiner nv l)
        (if (null? l) nv
            (combiner (car l) (foldr combiner nv (cdr l)))))

(define (foldl combiner nv l)
        (if (null? l) nv
            (foldl combiner (combiner (car l) nv) (cdr l))))

(define (add-last l x)
        (if (null? l)
            (cons x '())
            (cons (car l) (add-last (cdr l) x))))

(define (my-reverse l)
        (if  (null? l) 
             '()
             (add-last (my-reverse (cdr l)) (car l))))


(define (map-foldl f l)
     (my-reverse(foldl (lambda (a b) (cons (f a) b)) '() l)))

(define (filter-foldl p l)
         (if (null? l)
              '()
               (foldl (lambda (a b) (if (p a) (append  b (list a)) b)) '() l)))

