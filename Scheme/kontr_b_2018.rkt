(define (count-digits n)
    (define (iter counter i)
         (if (< i 10)
             counter
             (iter (+ 1 counter) (quotient i 10) )))
  (iter 1 n))

(define (my-exp x n)
     (define (iter i result)
        (if (= i ( abs n))
            result
            (iter (+ i 1) (* result x))))
  (if(>= n 0)
     (iter 0 1)
     (/ 1 (iter 0 1) )))

(define (narcissistic? n)
   (define count (count-digits n))
  (define (get-sum n sum)
    (if (= n 0)
        sum
        (get-sum (quotient n 10) (+ sum (my-exp (remainder n 10) count)))))
 (= n (get-sum n 0)))

(define (sum-divisors n)

  (define (get-sum i sum)
      (if (= i n)
          sum
          (if (= 0 (remainder n i))
               (get-sum (+ i 1) (+ sum i))
               (get-sum (+ i 1) sum)
               )
           ))
  (if(= n 0)
     0
     (get-sum 1 0)))

(define (friendly? a b)
     (and (= b (sum-divisors a)) (= a (sum-divisors b))))

(define (apply-f i j f)
  (define (iter j result)
    (if (<= j i)
        result
        (iter (- j 1)(f  (- j 1) result ))))
  (iter j j)
    )

(define (filter p l)
      (if (null? l)
          l
          (if (p (car l))
              (cons (car l) (filter p (cdr l)))
              (filter p (cdr l)))))
(define (foldl op nv l)
    (if (null? l)
        nv
        (foldl op (op (car l) nv) (cdr l))))

(define (get-all-values f x l )
    (map (lambda (a) (apply-f x a f)) (filter (lambda (a) (> a x)) l)) )

(define (find-max f a b)
  (define interval (to-list a b))
     (define (iter i l)
        (if (> i b)
            l
            (iter (+ 1 i) (append (get-all-values f i interval) l))))
  
 (foldl (lambda (x y) (if (>= x y) x y)) (car  (iter a '()))  (iter a '()) ))

(define (to-list a b)
   (if (> a b)
       '()
       (cons a (to-list (+ 1 a) b))))

(define (length-interval x)
       (abs (- (car x) (cdr x))))

(define (is-included? x y)
    (and (>= (car x) (car y)) (<= (cdr x) (cdr y))))

(define (min-element l)
      (if(null? l) 0(foldl (lambda (x y) (if (<= (length-interval x) (length-interval y)) x y)) (car l) (cdr l))))
(define il '((24 . 26)
                               (90 . 110)
                               (0 . 100)
                               (10 . 89)
                               (1 . 5)
                               (-4 . 25)))

(define (shortest-interval-supersets il)
    (define min (min-element il))
    (sort-by car (filter (lambda (x) (is-included? min x)) il)))

(define (comparing op x)
     (op x))

(define (sort-by comparing l)
  (define (insert x l)
    (cond ((null? l)
           (list x))
          ((>= (comparing x) (comparing (car l)))
           (cons x l))
          (else (cons (car l)
                      (insert x (cdr l))))))

  (foldl insert '() l))



(define (my-reverse l)
      (if (null? l)
          l
          (append  (reverse (cdr l)) (list (car l)))))

(define (calcPoly l x)
     (foldr (lambda (a nv) (+ (* x nv) a)) 0 (my-reverse l)))

(define l '(1 2 3))

(define (get-last-digit n)
    (remainder n 10))

(define (get-first-digit n)
    (if (< n 10)
        n
        (get-first-digit (quotient n 10))))

(define (same-first-last? a b)
    
  (= (get-last-digit a) (get-first-digit b)))




(define (numGame l)
     
     (if (or (null? l) (null? (cdr l)))
          #t
          (and (same-first-last? (car l) (car(cdr l))) (numGame (cdr l))))
  )

(define (square n) (* n n))

(define (generate a b l)
  (if (or (null? l) (> a b))
      '()
       (if (member (square a) l)
           (cons a (generate (+ 1 a) b l))
           (generate (+ 1 a) b l)))
  )

(define (every? p l)
    (and (p (car l)) (every? p (cdr l)))

  )
(define (to-list a b)
     (if (> a b)
         '()
         (cons a (to-list (+ 1 a) b))))
(define (get-length a b)
         (if (> a b)
             0
             (- b a)))

(define (predicate? f g x)
     (= (f x) (g x)))

(define (largestInterval f g a b)
    (define (construct-list-possible a b)
            (if (> a b)
                '()
                (if (every? (lambda (x)(predicate? f g x)) (to-list a b))
                    (cons (cons a( cons b ) )))))
  )


