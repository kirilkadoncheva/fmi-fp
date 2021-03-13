(define (factorial1-iter n)
  (define (for product counter)
    (if(> counter n) product (for (* product counter) (+ counter 1))))
  (for 1 1))

(define (factorial-iter n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if(> counter max-count) product (fact-iter (* product counter) (+ 1 counter) max-count)))

(define (sum-iter start end)
   (define (for-sum a b sum)
     (if(> a b) sum (for-sum (+ a 1) b (+ sum a))))
  (for-sum start end 0))

(define (expt-iter x n)
   (define (for-prod counter product)
     (if(> counter n) product (for-prod (+ counter 1) (* x product))))
  (cond ((< n 0) (/ 1(expt-iter x (- n))))
        ((= n 0) 1)
        (else (for-prod 1 1)))
  )

(define (count-digits n)
  (cond ((< n 0) (count-digits (- n)))
        (else (if (< n 10) 1 (+ 1 (count-digits (quotient n 10))))))
 
  )

(define (count-digits-iter n)
  (define (for-count counter n)
    (if (< n 10) counter (for-count (+ counter 1) (quotient n 10))))
  (if (< n 0) (for-count 1 (- n))(for-count 1 n))
 
  )

(define (sum-digits n)
  (cond ((< n 0) (sum-digits (- n)))
        (else (if (< n 10) n (+ (remainder n 10) (count-digits (quotient n 10))))))
  )

(define (sum-digits-iter n)
  (define (for-sum-iter sum num)
    (if (= num 0) sum (for-sum-iter (+ sum (remainder num 10)) (quotient num 10))))
  (cond ((< n 0) (sum-digits-iter (- n)))
        (else (for-sum-iter 0 n))
  ))

(define (reverse-digits n)
  (cond ((< n 0) (reverse-digits (- n)))
        ((< n 10) n)
        (else (+ (* (expt 10 (- (count-digits n) 1)) (remainder n 10))  (reverse-digits (quotient n 10)))))
  )

(define (reverse-digits-iter n)
  (define (for-reverse result num)
    (if(= num 0) result (for-reverse (+ (* result 10) (remainder num 10)) (quotient num 10))))
  (cond ((< n 0) (reverse-digits-iter (- n)))
      
        (else (for-reverse 0 n)))
  )

(define (divides? k n)
      (= (remainder n k) 0))

(define (count-divisors n)
  (define(count-divisors-up-to k)
        (cond ((= k 0) 0)
              ((divides? k n) (+ 1 (count-divisors-up-to (- k 1))))
               (else  (count-divisors-up-to (- k 1)))))
  (count-divisors-up-to n))

(define (count-divisors-iter n)
       (define (count-up-to k result)
         (cond ((= k 0) result)
               ((divides? k n) (count-up-to (- k 1) (+ 1 result)))
               (else (count-up-to (- k 1) result))))
  (count-up-to n 0))


(define (sum-divisors n)
        (define (sum-divisors-up-to k)
          (cond ((= k 0) 0)
                ((divides? k n) (+ k (sum-divisors-up-to (- k 1))))
                (else (sum-divisors-up-to (- k 1)))))
  (sum-divisors-up-to n)
         )


(define (sum-divisors-iter n)
        (define (sum-up-to k sum)
          (cond ((= k 0) sum)
                ((divides? k n) (sum-up-to (- k 1) (+ sum k)))
                (else (sum-up-to (- k 1) sum))))
  (sum-up-to n 0))

(define (prime? n)
    (= (count-divisors-iter n) 2))

(define (prime?-iter n)
  (define (iter k)
    (or (= k n)
        (and (not (divides? k n)) (iter (+ k 1)))))

  (and (not (= n 1)) (iter 2)))

(define (square x) (* x x))

(define (fast-expt-iter x n)
  (define (iter product x n)
    (cond ((= n 0) product)
          ((even? n) (iter product (square x) (/ n 2)))
          (else (iter (* product x) x (- n 1)))))

  (if (< n 0)
      (/ 1 (fast-expt-iter x (- n)))
      (iter 1 x n)))