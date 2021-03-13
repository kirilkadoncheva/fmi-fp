(define (count-while p l)
       (if(null? l)
                  0
                 (if (p (car l)) (+ 1 (count-while p (cdr l))) 0))
        
  )

(define (take-while p l)
   (if (null? l)
       l
       (if (p (car l)) (cons (car l) (take-while p (cdr l))) '())))

(define (drop-while p l)
   (if (null? l)
       l
       (if (p (car l))(drop-while p (cdr l)) l))) 

(define (next-look-and-say y)
   (define (take-first-equals l)
       (take-while (lambda (x) (= x (car l))) l))

  (define (drop-first-equals l)
       (drop-while (lambda (x) (= x (car l))) l))
  
  (if(null? y)
     y
    (cons (length(take-first-equals y))
          (cons (car y)
           (next-look-and-say (drop-first-equals y)))))
    )


(define (min-element l)
  (apply min l)
   )

(define (delete-element p l)
   (if (null? l)
        l
        (if (p (car l))
            (delete-element p (cdr l))
            (cons (car l) (delete-element p (cdr l))))))

(define (remove x l)
  (if(not (member x l))
     l
    (append (take-while (lambda (y) (not (= x y))) l)
            (cdr(drop-while (lambda (y) (not (= y x))) l)))))

(define (every? p l)
     (if (null? l)
         #t
       (and (p (car l)) (every? p (cdr l)))))

(define l (list 1 2 3 4 5 6 7 8 9))
(define le (list 77 18 9 6 8 12 18 24 26))

(define (any? p l)
        (not (every? (lambda (x) (not (p x))) l)))


(define (1+ n) (+ 1 n))


(define (enumerate-interval from to)
  (define (for result from)
    (if(> from to)
           result
            (for (append result (list from) ) (1+ from))))
  (for '() from)
     )

(define (take-while p l)
        (if(null? l)
            '()
             (if (p (car l))
                  (cons (car l) (take-while p (cdr l)))
                   '())))

(define (drop-while p l)
        (if (null? l)
            l
            (if (p (car l))
                (drop-while p (cdr l))
                 l)))
(define (foldr combiner nv l)
        (if (null? l) nv
            (combiner (car l) (foldr combiner nv (cdr l)))))

(define (foldl combiner nv l)
        (if (null? l) nv
            (foldl combiner (combiner (car l) nv) (cdr l))))

(define (min-element l)
  (if(null? l) 0
   (foldl (lambda (a b) (if (<= a b) a b)) (car l) l)))



(define (remove l x)
  ( if(not (member x l))
      l
       (if (null? l)
          l
          (append (take-while (lambda (a) (not (= x a))) l) (cdr (drop-while (lambda (a) (not (= x a))) l)) )))
      )

(define (selection-sort l)
 (define min-el (min-element l))
  (
   if(null? l)
     l
     (cons min-el (selection-sort(remove l min-el))))
  )

(define (my-filter p l)
        (if (null? l) '() (if (p (car l))(cons  (car l) (my-filter  p (cdr l))) (my-filter  p (cdr l)))) )


(define (quicksort l)
  (if (null? l)
       l
       (append
        (quicksort (my-filter (lambda (x) (< x (car l))) (cdr l)))
         (list (car l))
         (quicksort (my-filter (lambda (x) (>= x (car l))) (cdr l)))))
   )


(define (count-digits-iter n)
  (define (for-count counter n)
    (if (< n 10) counter (for-count (+ counter 1) (quotient n 10))))
  (if (< n 0) (for-count 1 (- n))(for-count 1 n))
 
  )

(define (sum-divisors-iter n)
        (define (sum-up-to k sum)
          (cond ((= k 0) sum)
                ((divides? k n) (sum-up-to (- k 1) (+ sum k)))
                (else (sum-up-to (- k 1) sum))))
  (sum-up-to n 0))

(define (divides? k n)
      (= (remainder n k) 0))


(define (count-divisors-iter n)
       (define (count-up-to k result)
         (cond ((= k 0) result)
               ((divides? k n) (count-up-to (- k 1) (+ 1 result)))
               (else (count-up-to (- k 1) result))))
  (count-up-to n 0))


(define (prime? n)
    (= (count-divisors-iter n) 2))

(define (prime-sum-pairs n)
  
  (define list-to-n (enumerate-interval 1 n))
  (define (all-is j l)
     (if (null? l)
         l
         (if (and (> (car l) j) (prime? (+ (car l) j)))
             (cons (list (car l) j  (+ j (car l))) (all-is j (cdr l)))
              (all-is j (cdr l)))))
 
  (define(for a b result)
    (if (> a b)
        result
        
           (for (+ 1 a) b (append  result (all-is a list-to-n)))))
  (for 1 n '())
  )

(load "/Documents/uni/2 курс/ФП/check.rkt")

(define (my-reverse n)
    (define (for i result)
           (if (= i 0)
               result
               (for (quotient i 10) (+ (* result 10) (remainder i 10)))))
  (if (< n 0) (for (- n) 0)(for n 0)))

(define (diff-reverse n)
   (- n (my-reverse n)))

(define (count-digit digit n)
      (define (for i result)
        (if (= i 0)
            result
            (if (= digit (remainder i 10))
                (for (quotient i 10) (+ 1 result))
                (for (quotient i 10) result)
                )
            ))
  (for n 0))

(define (make-result result i count)
        (define (for j result)
           (if (> j count)
               result
               (for (+ 1 j) (+ (* result 10) i))))
  (for 1 result))

(define (multBy10 i result)
     (define (for j result)
       (if (> j i)
           result
           (for (+ j 1) (* 10 result))))
  (for 1 result))

(define (sort-digits n)
         (define (get-all i result)
           ( 

            if(= i 0)
             result
              
              
                 (if (not (= (count-digit i n) 0))
                    (get-all (- i 1) (make-result result i (count-digit i n)))
                    (get-all (- i 1) result)))
               
             )
  (define count-0 (count-digit 0 n))
  (if (= 0 count-0)
      (get-all 9 0)
      (multBy10 count-0 (get-all 9 0)))
  
  )

(define (count-digits n)
      (define (for i counter)
        (if(< i 10)
           (+ counter 1)
           (for (quotient i 10) (+ counter 1))))
  (for n 0))

(define (last-digit n)
      (remainder n 10))

(define (without-last-digit n)
       (quotient n 10))

(define (middle-digit n)
        (define (kth-digit-from-last k n)
           (if (= k 0)
               (last-digit n)
               (kth-digit-from-last (- k 1) (without-last-digit n))))
  (define count (count-digits n))
  (if (even? count)
      -1
      (kth-digit-from-last (quotient count 2) n)))

(define (every? p l)
     (if (null? l)
         #t
       (and (p (car l)) (every? p (cdr l)))))

(define l (list 1 2 3 4 5 6 7 8 9))
(define le (list 77 19 9 6 8 12 18 24 26))

(define (any? p l)
        (not (every? (lambda (x) (not (p x))) l)))

(define (endomorphism? l op f)
    (and (every? (lambda (x) (member (f x) l)) l)
         (every? (lambda (x) (every? (lambda (y) (= (+ (f x) (f y)) (f (+ x y)))) l)) l)))

(define (meet-twice? f g a b)
  (define a-b-list (enumerate-interval a b))
  (any? (lambda (x) (any? (lambda (y) (and (= (f x) (g x)) (= (f y) (g y)) (not (= x y)))) a-b-list)) a-b-list))

(define (drop-while-counter p l counter)
       (if (null? l)
           counter
           (if (p (car l))
               (drop-while-counter p (cdr l) (+ 1 counter))
               counter)))

(define (next-look-and-say l)
      
              (if (null? l)
                  '()
                  (append (list (drop-while-counter (lambda (x) (= x (car l))) l 0))
                        (list (car l)) (next-look-and-say (drop-while (lambda (x) (= x (car l))) l)))))
  

(define l1 '( 3 3 3 3))
(define (1+ n) (+ n 1))
(define (square n) (* n n))

(define (apply-func f g x)
 
    (define (for i result)
        (if (= i (quotient x 2))
            result
            (for (+ i 1) (f (g result)))))
  
      (for 0 x)
      )

(define (foldl op nv l)
    (if (null? l)
        nv
        (foldl op (op nv (car l)) (cdr l))))

(define (filter p l)
    (if (null? l)
        l
        (if (p (car l))
            (cons (car l) (filter p (cdr l)))
            (filter p (cdr l)))))

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
      (if (= n 0)
          (lambda (x) x)
           (compose f (repeated f (- n 1) ))))

(define (alternate f g n)
     (repeated (compose f g) n))

(define (permutable? a b f g)
  
 (define interval (enumerate-interval a b))
  (every? (lambda (x) (= ((alternate f g (/ x 2)) x) ((alternate g f (/ x 2)) x))) (filter even? interval)))

(define (cube x) (* x x x))
(define (const-42 x) 42)
(define (2^ x) (expt 2 x))

;(define (abs x)
 ;  (if (< x 0) -x x))
(define (length-interval x)
       (abs (- (car x) (cdr x))))
(define (max-element l)
      (if (null? l)
          0
          (foldl (lambda (x y) (if (> (length-interval x) (length-interval y)) x y)) (car l) l)))

(define (includes? l x)
    (and (>= (car x) (car l)) (<= (cdr x) (cdr l))))

(define (new-filter p l)
    (if (null? l)
        l
            (if (p (car l))
            (append (car l) (filter p (cdr l)))
            (filter p (cdr l)))))

(define (foldl1 combiner nv l)
        (if (null? l) nv
            (foldl1 combiner (combiner (car l) nv) (cdr l))))


(define (sort-by comparing l)
  (define (insert x l)
    (cond ((null? l)
           (list x))
          ((<= (comparing x) (comparing (car l)))
           (cons x l))
          (else (cons (car l)
                      (insert x (cdr l))))))

  (foldl1 insert '() l))

(define (longest-interval-subsets l)
      (define longest (max-element l))
  (if (null? l)
      l
   (sort-by car(filter (lambda (x) (includes? longest x)) l))))

(define d '((24 . 25)
                            (90 . 110)
                            (0 . 100)
                            (10 . 109)
                            (1 . 3)
                            (-4 . 2)))