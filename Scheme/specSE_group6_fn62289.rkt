(define (foldr combiner nv l)
        (if (null? l) nv
            (combiner (car l) (foldr combiner nv (cdr l)))))

(define (foldl combiner nv l)
        (if (null? l) nv
            (foldl combiner (combiner (car l) nv) (cdr l))))

(define (accumulate op nv term a next b)
  (if (> a b) nv
      (op (term a) (accumulate op nv term (next a) next b))))

(define (filter p l)
      (if (null? l)
          l
          (if (p (car l))
              (cons (car l) (filter p (cdr l)))
              (filter p (cdr l)))))

(define (divides? n k)
     (if (= k 0)
         #f
         (= 0 (remainder n k))))

(define (to-list n)
  (if (< n 0)
      (to-list (- n))
      
       (if (< n 10)
           (list n)
           (cons (remainder n 10) (to-list (quotient n 10))))))

(define (sum-digit-divisors n)
  (define l (to-list n))
    (foldl + 0 (filter (lambda (x) (divides? n x)) l)))

(define (interval-to-list a b)
     (if (> a b)
         '()
         (cons a (interval-to-list (+ 1 a) b))))

(define (find-sum-all-n m b)
      (if (<= b m)
          0
          (if (= (sum-digit-divisors m) (sum-digit-divisors b))
              (+ 1 (find-sum-all-n m (- b 1)))
              (find-sum-all-n m (- b 1)))))

(define (same-sum a b)
     
     
              (if (> a b)
                  0
                  (+ (find-sum-all-n a b) (same-sum (+ 1 a) b))))

(define (every? p l)
  (if (null? l)
      #t
      (and (p (car l)) (every? p (cdr l))))
     )

(define (any? p l)
     (not (every? (lambda (x) (not (p x))) l)))

(define l '(7 8 6))

(define (get-all-products ml ll)
      (if (null? ml)
          '()
          (cons (map (lambda (x) ((car ml) x)) ll) (get-all-products (cdr ml) ll))))

(define (sum l)
     (apply + l))

(define (bigger? l1 l2)
       (if (null? l2)
           #t
           (and (>= (car l1) (car l2)) (bigger? (cdr l1) (cdr l2)))))

(define (is-biggest? l ll)
  (if (or (null? ll)(null? l))
      #t
      (and (bigger? l (car ll)) (is-biggest? l (cdr ll))))
       )

(define (remove-same l)
     (if (null? l)
         l
         (if (member (car l) (cdr l))
             (remove-same (cdr l))
             (cons (car l) (remove-same (cdr l))))))

(define (best-metric? ml ll)
  (define result-list (remove-same (get-all-products ml ll)))
 ;result-list
   (any? (lambda (x) (is-biggest? x result-list)) result-list)
  )

  (define ml (list car sum))
  (define ll '( ( 100 -100) ( 29 1) ( 42)))

  (define l '(1 2  1 3 4 5 6 7 7 8 9 9))

  (define (prod l) (apply * l))
  (define (minus l) (apply - l))

  ; (define ml (list sum prod))
  ;(define ll '( ( 0 1 2) (3 -4 5) ( 1337 0)))

   (define l1 '(1 (2 (2 4) 1) 0 (3 (1))))

   (define (get-index x l)
        (if (not(member x l))
            -1
            (if (or (null? l)(= x (car l) ))
                0
                (+ 1 (get-index x (cdr l)) )
                    )))

  
(define (atom? x)
     (and (not (pair? x)) (not( list? x))))

   ;(define (get-level n dl)
   ;  (define (helper counter i j dl)
    ;        (if (or (null? dl)(> counter n))
     ;           '()
      ;          
       ;          (if (atom? (car dl))
        ;             (if (= counter n)
         ;                (cons (car dl) (helper (+ 1 counter) i (+ 1 j) (cdr dl)) )
          ;               (helper (+ 1 counter) i (+ j 1) (cdr dl)))
           ;          (+ (helper (+ counter 1) i (+ j 1) (car (car dl))) )
            ;         ;(if (null? (car dl))
                         
                         ;(helper i (+ i 1) 1 (cdr dl))
                      ;(helper (+ 1 counter) i (+ j 1) (cdr dl)))))
             ;     )
     ;(helper 1 1 1 dl))

   
   


