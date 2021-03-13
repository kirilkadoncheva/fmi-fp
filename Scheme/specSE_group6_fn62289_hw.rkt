;Задача 1 -----------------------------------------------------------

(define (foldr combiner nv l)
        (if (null? l) nv
            (combiner (car l) (foldr combiner nv (cdr l)))))

(define (plus x)
   (cons + (list 0 x)))

(define (minus x)
  (cons + (list 0 (- x))))


(define (times x)
   (cons * (list 1 x)))

(define (div x)
  (cons * (list 1 (/ 1 x))))


(define (one . args)
 
   ( if(null? args)
       1
       (foldr (car (apply append args)) (car (cdr(apply append args))) (append (cddr (apply append args)) (list 1)))
      ))


(define (two . args)
 
   ( if(null? args)
       2
       (foldr (car (apply append args)) (car (cdr(apply append args))) (append (cddr (apply append args)) (list 2)))
      ))


(define (three . args)
 
   ( if(null? args)
       3
      (foldr (car (apply append args)) (car (cdr(apply append args))) (append (cddr (apply append args)) (list 3)))
      ))


(define (four . args)
 
   ( if(null? args)
       4
      (foldr (car (apply append args)) (car (cdr(apply append args))) (append (cddr (apply append args)) (list 4)))
      ))


(define (five . args)
 
   ( if(null? args)
       5
      (foldr (car (apply append args)) (car (cdr(apply append args))) (append (cddr (apply append args)) (list 5)))
      ))


(define (six . args)
 
   ( if(null? args)
       6
      (foldr (car (apply append args)) (car (cdr(apply append args))) (append (cddr (apply append args)) (list 6)))
      ))

(define (seven . args)
 
   ( if(null? args)
       7
      (foldr (car (apply append args)) (car (cdr(apply append args))) (append (cddr (apply append args)) (list 7)))
      ))


(define (eight . args)
 
   ( if(null? args)
       8
       (foldr (car (apply append args)) (car (cdr(apply append args))) (append (cddr (apply append args)) (list 8)))
      ))

(define (nine . args)
 
   ( if(null? args)
       9
       (foldr (car (apply append args)) (car (cdr(apply append args))) (append (cddr (apply append args)) (list 9)))
      ))


;Задача 2 -------------------------------------------------------------------------

(define l '(1 2 3 4 5 6 7 8 9 ))
(define (my-length l)
  (if(null? l)
     0
     (+ 1 (my-length(cdr l)))))

(define (take-first-k-elements l k)
     (if (or (<= k 0) (null? l))
         '()
         (cons (car l) (take-first-k-elements (cdr l) (- k 1)))))

(define (prefixes xs)
  (define (take-pref i count)
         (if (> i count)
             '()
             (cons (take-first-k-elements xs i) (take-pref (+ i 1) count))))
  (take-pref 0 (my-length xs)))

;Задача 3 -----------------------------------------------------------------------

(define (leap? y)
  (or (and (= 0 (remainder y 4)) (not (= 0 (remainder y 100))))
      (= 0 (remainder y 400))))

(define (possible-month? month)
    (and (<= month 12) (>= month 1)))

(define (days-in-month m y)
  (if (not (possible-month? m))
      -1
      (cond ((= m 1) 31)
            ((= m 2) (if (leap? y)
                         29
                         28))
            ((= m 3) 31)
            ((= m 4) 30)
            ((= m 5) 31)
            ((= m 6) 30)
            ((= m 7) 31)
            ((= m 8) 31)
            ((= m 9) 30)
            ((= m 10) 31)
            ((= m 11) 30)
            (else  31))))

(define (possible-date? d m y)
    (and (possible-month? m) (and (>= d 1) (<= d (days-in-month m y)))))

(define (make-date d m y)
   (if (not (possible-date? d m y))
       #f
       (list 'date d m y)))
(define (is-date? x)
    (and (list? x) (not (null? x)) (equal? 'date (car x)) (= 4 (length x))
         (possible-date? (list-ref x 1 ) (list-ref x 2) (list-ref x 3))))

(define (day date)
  (if (is-date? date)
      (list-ref date 1)
      'not-date))

(define (month date)
  (if (is-date? date)
      (list-ref date 2)
      'not-date))

(define (year date)
  (if (is-date? date)
      (list-ref date 3)
      'not-date))

(define (date? x)
  (is-date? x))

(define (date->string x)
  (if (not (date? x))
      'not-date
      (string-append (number->string (day x))
                     "." (number->string (month x))
                     "." (number->string (year x)))))



(define (next-day x)
   (if (not (date? x))
       'not-date
       (let* ((d (day x))
              (m (month x))
              (y (year x)))
         (cond ((date? (make-date (+ 1 d) m y)) (make-date (+ 1 d) m y))
               ((date? (make-date 1 (+ 1 m) y)) (make-date 1 (+ 1 m) y))
               (else (make-date 1 1 (+ 1 y))))
             ))
        )

(define (date< x y)
  (if (or (not (date? x)) (not (date? y)))
      'invalid-input
      (cond ((< (year x) (year y)) #t)
            ((= (year x) (year y))
             (or (< (month x) (month y)) (and (= (month x) (month y)) (< (day x) (day y))))))))

(define (helper m)
   (cond ((= m 0) 0)
         ((= m 1) 3)
         ((= m 2) 2)
         ((= m 3) 5)
         ((= m 4) 0)
         ((= m 5) 3)
         ((= m 6) 5)
         ((= m 7) 1)
         ((= m 8) 4)
         ((= m 9) 6)
         ((= m 10) 2)
         ((= m 11) 4)))

(define (get-expr d m y)
  (remainder (+ y (quotient y 4) (- (quotient y 100)) (quotient y 400) (helper (- m 1)) d) 7))

(define (day-of-the-week d m y)
   (if (< m 3)
       (get-expr d m (- y 1))
       (get-expr d m y)))



(define (weekday x)
 (if (not (date? x))
      'not-date
       (let ((id (day-of-the-week (day x) (month x) (year x))))
         (cond ((= id 0) 'Sunday)
               ((= id 1) 'Monday)
               ((= id 2) 'Tuesday)
               ((= id 3) 'Wednesday)
               ((= id 4) 'Thursday)
               ((= id 5) 'Friday)
               ((= id 6) 'Saturday)) )
         ))

(define (date-after-n-days n x)
  (define (for i x)
    (if (= i n)
        x
        (for (+ i 1) (next-day x))))
  (if (not (date? x))
      'not-date
      (for 0 x)))

(define (is-valid-weekday? x)
  (or (equal? x 'Monday)
      (equal? x 'Tuesday)
      (equal? x 'Wednesday)
      (equal? x 'Thursday)
      (equal? x 'Friday)
      (equal? x 'Saturday)
      (equal? x 'Sunday)
      
      ))

(define (get-day-id x)
  (if (not (is-valid-weekday? x))
      'invalid-input
      (cond ((equal? x 'Monday) 1)
            ((equal? x 'Tuesday) 2)
            ((equal? x 'Wednesday) 3)
            ((equal? x 'Thursday) 4)
            ((equal? x 'Friday) 5)
            ((equal? x 'Saturday) 6)
            ((equal? x 'Sunday) 7))))

(define (next-weekday day x)
  (if (or (not (is-valid-weekday? day)) (not (date? x)))
      'invalid-input
      (let ((idDay (get-day-id day))
            (idDate (get-day-id (weekday x))))
        (if (<= idDay idDate)
            (date-after-n-days (+ (- 7 idDate) idDay) x)
            (date-after-n-days (- idDay idDate) x)))
            ))

(define (make-event x string)
  (if (date? x)
      (cons x string)
      'invalid-input))

(define (equal-date? x y)
  (if (or (not (date? x)) (not (date? y)))
      'invalid-input
      (and (= (day x) (day y))
           (= (month x) (month y))
           (= (year x) (year y)))))

(define (filter p l)
      (if (null? l)
          l
          (if (p (car l))
              (cons (car l) (filter p (cdr l)))
              (filter p (cdr l)))))

(define (events-for-day x l)
  
  (if (not (date? x))
      'invalid-input
     (filter (lambda (el) (equal-date? x (car el))) l) ))

(define (event? l)
  (and (pair? l) (date? (car l)) (string? (cdr l))))

(define (get-full-list-events x l)
   (if (not (date? x))
      'invalid-input
     (cons x (map (lambda (el) (cdr el)) (filter (lambda (el) (and (event? el)(equal-date? x (car el)))) l)))
     ))

(define (get-keys l)
  (define keys  (map (lambda (el) (if (event? el) (car el) el))l ))
   (define (make-unique keys)
        (if (null? keys)
            '()
            (cons (car keys)
                  (make-unique
                   (filter (lambda (el) (not (equal-date? el (car keys)))) (cdr keys)))))
            )
  (sort (make-unique keys)))

(define (foldl combiner nv l)
        (if (null? l) nv
            (foldl combiner (combiner (car l) nv) (cdr l))))

(define (sort l)
  (define (insert x l)
    (cond ((null? l)
           (list x))
          ((or (equal-date? x (car l)) (date< x (car l) ))
           (cons x l))
          (else (cons (car l)
                      (insert x (cdr l))))))

  (foldl insert '() l))

(define (calendar l)
  (define keys (get-keys l))
  (define (for keys)
    (if (null? keys)
        '()
        (cons (get-full-list-events (car keys) l) (for (cdr keys)))))
  (for keys))