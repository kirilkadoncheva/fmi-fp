(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (fibonacci-iter n)
        (define (for-iter last-1 last i)
          (cond 
            ((= i n) last)
            (else (for-iter last (+ last-1 last) (+ i 1)))))
       (if (= n 0) 0 (for-iter 0 1 1)))

(define (f n)
  (cond ((< n 3) n)
     
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(define (f-iter n)
        (define (for-iter last-2 last-1 last i)
         
          (if (= i n) last
            (for-iter last-1 last (+ last (* 2 last-1) (* 3 last-2)) (+ i 1))))
  (if(< n 3) n 
        (for-iter 0 1 2 2)))



(define (binomial-coefficient row index)
    (if (or (<= row 2) (= index 1) (= index row)) 1
        (+ (binomial-coefficient (- row 1) (- index 1)) (binomial-coefficient (- row 1) index))))

(define (accumulate op nv term a next b)
  (if (> a b) nv
      (op (term a) (accumulate op nv term (next a) next b))))

(define (accumulate-iter op nv term a next b); първия тест с ламбда не работи
      (if (> a b) nv
           (accumulate op (op (term a) nv) term (next a) next b)))

(define (sum term a next b)
  (accumulate + 0 term a next b) )

(define (1+ x) (+ x 1))
(define (square x) (* x x))
(define (product term a next b)
    (accumulate * 1 term a next b))

(define (identity x) x)
(define (2+ x) (+ x 2))

(define (count predicate a b)
  (sum (lambda (a)
         (if(predicate a)
            1
            0))
       a
       (lambda (a) (+ a 1))
       b
       
 ))

(define (count-a predicate a b)
     (accumulate + 0 (lambda (a) (if (predicate a) 1 0)) a 1+ b))

(define (count-digits n)
  (cond ((< n 0) (count-digits (- n)))
        (else (if (< n 10) 1 (+ 1 (count-digits (quotient n 10))))))
 
  )
(define (reverse-digits n)
  (cond ((< n 0) (reverse-digits (- n)))
        ((< n 10) n)
        (else (+ (* (expt 10 (- (count-digits n) 1)) (remainder n 10))  (reverse-digits (quotient n 10)))))
  )

(define (palindrome? n)
  (if (= n (reverse-digits n)) #t #f))


(define (count-palindromes a b)
  (count palindrome? a b))

(define (exists? predicate a b)
        (accumulate (lambda (a b) (or a b)) #f (lambda (a) (predicate a)) a 1+ b))

(define (for-all? predicate a b)
        (accumulate (lambda (a b) (and a b)) #t (lambda (a) (predicate a)) a 1+ b))

(define (double f)
        (lambda (x) (f (f x))))

(define (compose f g)
        (lambda (x) (f (g x))))

(define (repeated f n)
  
    (if (= n 0) identity (lambda (x) (f ((repeated f (- n 1)) x) ))
 ))

(define (repeated f n)
  
     (lambda (x) (if(= n 0) x (f ((repeated f (- n 1)) x) )))
 )

(define (repeated f n)
  
    (if (= n 0) (lambda (x) x) (compose f (repeated f (- n 1))))
 )