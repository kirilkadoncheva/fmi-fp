(load "/Documents/uni/2_курс/ФП/Scheme/check.rkt")
(define (pow x n)
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (pow x (- n))))
        (else (* x (pow x (- n 1))))))

(define (qpow x n)
  (define (sqr x) (* x x))
  (cond ((= n 0) 1)
        ((< n 0) (/ 1 (qpow x (- n))))
        ((even? n) (sqr (qpow x (quotient n 2))))
        (else (* x (qpow x (- n 1))))))

(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (qfib n)
  (define (iter i fi fi-1)
      (if (= i n)
          fi
          (iter (+ i 1) (+ fi fi-1) fi)))
      (if (= n 0)
          0
          (iter 1 1 0)))

(define (sum1 k)
   (if (> k 100) 0 (+ (* k k) (sum1 (+ k 1)))))

(define (sum2 a b f dx)
  (if (> a b) 0 (+ (* dx (f a)) (sum2 (+ a dx) b f dx))))

(define (sum3 x)
    (if (> x (expt 10 1000)) 0 (+ x (sum3 (exp x)))))

(define (sum a b term next)
  (if (> a b) 0 (+ (term a) (sum (next a) b term next))))

(define (prod a b term next)
  (if (> a b) 1 (* (term a) (prod (next a) b term next))))

(define (accumulate op nv a b term next)
    (if (> a b)
        nv
        (op (term a) (accumulate op nv (next a) b term next))))

(define (p n x)
   (define (term i) (* (- (+ n 1) i) (expt x i)))
   (define (next i) (+ i 1))
  (accumulate + 0 0 n term next))

(define (p1 n x)
   (define (ac1 a b y nv)
      (if (> a b)
        nv
        (+ (* (- (+ n 1) a) y) (ac1 (+ a 1) b (* y x) nv))))
  (ac1 0 n 1 0))


(define (fib-iter n)
  (define (for i x y)
     (if (= i n)
         y
         (for (+ i 1) y (+ x y))))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (for 2 1 1))))

(define (factorial-iter n)
  (define (iter i r)
       (if (> i n)
           r
           (iter (+ i 1) (* r i))))
  
      (iter 1 1))

(define (sum-iter s e)
  (sum s e (lambda (x) x) (lambda (x) (+ 1 x))))

(define (expt-iter x n)
  (define (iter i prod)
    (if (> i n)
        prod
        (iter (+ i 1) (* prod x))))
  
     ( if (>= n 0) (iter 1 1)
                     (/ 1 (expt-iter x (- n)))))


(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))))

(define (count-digits-iter n)
  (define (for i r)
           (if (< i 10)
               (+ 1 r)
               (for (quotient i 10) (+ r 1))))
  (for n 0))

(define (sum-digits n)
  (if (< n 10)
      n
      (+ (remainder n 10) (sum-digits (quotient n 10)))))

(define (sum-digits-iter n)
  (define (for i r)
           (if (< i 10)
               (+ i r)
               (for (quotient i 10) (+ r (remainder i 10)))))
  (for n 0))

(define (reverse-digits-iter n)
  (define (for i r)
           (if (< i 10)
               (+ i (* r 10))
               (for (quotient i 10) (+ (* 10 r) (remainder i 10)))))
  (for n 0))

(define (reverse-digits n)
  (if (< n 10)
      n
      (+ (* (expt 10 (- (count-digits n) 1)) (remainder n 10)) (reverse-digits (quotient n 10)))))

(define (count-divisors-iter n)
    (define (for i r)
      (if (> i n)
          r
          (if (= (remainder n i) 0)
              (for (+ i 1) (+ r 1))
              (for (+ i 1) r))))
  (for 1 0))

(define (count-divisors n)
    (define (for i)
      (if (> i n)
          0
          (if (= (remainder n i) 0)
              (+ 1 (for (+ i 1)))
              (for (+ i 1)))))
  (for 1))

(define (sum-divisors-iter n)
    (define (for i r)
      (if (> i n)
          r
          (if (= (remainder n i) 0)
              (for (+ i 1) (+ r i))
              (for (+ i 1) r))))
  (for 1 0))

(define (sum-divisors n)
    (define (for i)
      (if (> i n)
          0
          (if (= (remainder n i) 0)
              (+ i (for (+ i 1)))
              (for (+ i 1)))))
  (for 1))

(define (prime? n)
  (define (for i)
     (if (>= i n)
         #t
        
         (and (not (= 0 (remainder n i))) (for (+ i 1)))))
  (and (not (= n 1))(for 2)))

(define (square x) (* x x))


(define (fast-expt-iter x n)
  (define (iter product x n)
    (cond ((= n 0) product)
          ((even? n) (iter product (square x) (/ n 2)))
          (else (iter (* product x) x (- n 1)))))

  (if (< n 0)
      (/ 1 (fast-expt-iter x (- n)))
      (iter 1 x n)))

(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))

(define (fibonacci-iter n)
  (define (iter i x y)
    (cond 
        ((= i n) y)
        (else (iter (+ i 1) y (+ x y)))))
  (cond ((= n 0) 0)
        (else (iter 1 0 1)))
  )

(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(define (f-iter n)
  (define (iter i x y z)
    (cond 
        ((= i n) z)
        (else (iter (+ i 1) y  z (+ z (* 2 y) (* 3 x))))))
  (cond ((= n 0) 0)
        ((= n 1) 1)
        
        (else (iter 2 0 1 2)))
  )

(define (binomial-coefficient row index)
  (cond ((and (= row 1) (= index 1)) 1)
        ((or (= index 1) (= index row) ) 1)
        (else (+ (binomial-coefficient (- row 1) index) (binomial-coefficient (- row 1) (- index 1))))))

(define (accumulate combiner nv term a next b)
  (if (> a b)
      nv
      (combiner (term a) (accumulate combiner nv term (next a) next b))))

(define (accumulate-iter combiner nv term a next b)
  (if (> a b)
      nv
      (accumulate-iter combiner (combiner nv (term a))  term (next a) next b)))

(define (sum term a next b)
   (accumulate-iter + 0 term a next b))

(define (product term a next b)
   (accumulate-iter * 1 term a next b))

(define (count predicate a b)
    (sum (lambda (x) (if (predicate x) 1 0)) a (lambda (x) (+ x 1)) b))

(define (palindrome? n)
  (= n (reverse-digits n)))

(define (count-palindromes a b)
  (count palindrome? a b))

(define (exists? predicate a b)
    (if (> a b)
        #f
        (if (predicate a)
            #t
            (exists? predicate (+ a 1) b))))

(define (for-all? predicate a b)
  (or (> a b) (and (predicate a) (for-all? predicate (+ a 1) b))))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose-two f g)
 (lambda (x) (f (g x))) )

(define (identity x) x)

(define (repeated f n)
  (cond ((= n 0) identity)
        ((= n 1) f)
        (else (compose-two f (repeated f (- n 1) )))))

(define (accumulate-i op nv a b term next)
    (if (> a b)
        nv
        (accumulate-i op (op nv (term a)) (next a) b next term)))

(define (p n x)
  (define (op u v) (+ (* u x) v))
  (accumulate-i op 0 1 (+ n 1) identity (lambda (x) (+ x 1))))

(define (derive f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (derive-n f n dx)
  (if (= n 0) f (derive (derive-n f (- n 1) dx))))

(define (derive-n f n dx)
  ((repeated (lambda (f) (derive f dx)) n) f))

(define (derive-n f n dx)
  ((accumulate compose-two id 1 n (lambda (i) (lambda (f) (derive f dx))) (lambda (x) (+ x 1))) f))

(define lambda-true (lambda (x y) x))
(define lambda-false (lambda (x y) y))
(define (lambda-if b x y) ((b x y)))

(define (map-list f l)
  (if (null? l)
      '()
      (cons (f (car l)) (map-list f (cdr l)))))

(define (filter p l)
  (if (null? l)
      '()
      (if (p (car l))
          (cons (car l) (filter p (cdr l)))
          (filter p (cdr l)))))

(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l)
      nv
     (foldl op (op nv (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l))
      (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op nv l)
  (foldl op (car l) (cdr l)))

(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define (count-atoms l)
  (cond ((null? l) 0)
        ((atom? l) 1)
        (else (+ (count-atoms (car l)) (count-atoms (cdr l))))))

(define (flatten l)
  (cond ((null? l) '())
        ((atom? l) (list l))
        (else (append (flatten (car l)) (flatten (cdr l))))))

(define (deep-reverse l)
  (cond ((null? l) '())
        ((atom? l) l)
        (else (append (deep-reverse (cdr l))
                      (list (deep-reverse (car l)))))))

(define (deep-foldr nv term op l)
   (cond ((null? l) nv)
         ((atom? l) (term l))
         (else (op (deep-foldr nv term op (car l))
                   (deep-foldr nv term op (cdr l))))))

(define (count-atoms-foldr l)
  (deep-foldr 0 (lambda (x) 1) + l))
(define (flatten-foldr l)
  (deep-foldr '() list append l))
(define (snoc x l)
   (append l (list x)))

(define (deep-reverse-foldr l)
  (deep-foldr '() identity snoc l))

(define (branch p? f g)
  (lambda (x) (p? x) (f x) (g x)))

(define (deep-foldr-op nv term op l)
  (foldr op nv (map (branch atom? term (lambda (l) (deep-foldr nv term op l))) l)))

(define (deep-foldl nv term op l)
   (cond ((null? l) nv)
         ((atom? l) (term l))
         (else (op (deep-foldl nv term op (cdr l)) (deep-foldl nv term op (car l))
                   ))))

(define (my-append . l)
  (cond ((null? l) '())
        ((null? (car l)) (apply my-append (cdr l)))
        (else (cons (caar l)
                    (apply my-append (cons (cdar l) (cdr l)))))))

(define (evali x) (eval x (interaction-environment)))

(define (my-length l)
  (if (null? l)
      0
      (+ 1 (my-length (cdr l)))))

(define (sum l)
  (if (null? l)
      0
      (+ (car l) (sum (cdr l)))))

(define (member? x l)
  (if (null? l)
      #f
      (or (equal? (car l) x) (member? x (cdr l)))))

(define (last l)
  (if (null? l)
      '()
      (if (null? (cdr l))
          (car l)
          (last (cdr l)))))

(define (nth l n)
  (if (null? l)
        '()
      (if (= n 0)
      (car l)
      (nth (cdr l) (- n 1)))))

(define (my-map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))

(define (scale l x)
  (my-map (lambda (y) (* x y)) l))

(define (add-last l x)
  (if (null? l)
      (list x)
      
      (cons (car l) (add-last (cdr l) x))))

(define (my-reverse l)
  (if (null? l)
      '()
      (append (my-reverse (cdr l)) (list (car l)))))

(define (my-append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (my-append (cdr l1) l2))))

(define (reject p l)
  (filter (lambda (x) (not (p x))) l))

(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l)
      nv
      (foldl op (op nv (car l)) (cdr l))))

(define (map-foldl f l)
  (foldl (lambda (x y) (append x (list (f y)))) '() l))

(define (filter-foldl p l)
  (reverse (foldl (lambda (nv c) (if (p c) (cons c nv) nv)) '() l)))

(define (every? p l)
  (or (null? l) (and (p (car l)) (every? p (cdr l)))))

(define (any? p l)
  (and (not(null? l))(or (p (car l)) (any? p (cdr l)))))

(define (enumerate-interval from to)
  (if (> from to)
      '()
      (cons from (enumerate-interval (+ from 1) to))))

(define (take-while p l)
  (if (null? l)
      '()
      (if (p (car l))
          (cons (car l) (take-while p (cdr l)))
          '())))

(define (drop-while p l)
  (if (null? l)
      '()
      (if (p (car l))
           (drop-while p (cdr l))
          l)))

(define (remove x l)
  (define (!=x y)
    (not (= y x)))

  (if (not (member x l))
      l
      (append (take-while !=x l)
              (cdr (drop-while !=x l)))))

(define (quicksort l)
  (if (null? l)
      '()
      (let ((pivot (car l))
            (others (cdr l)))
        (append (quicksort (filter (lambda (x) (< x pivot)) others))
                (list pivot)
                (quicksort (filter (lambda (x) (>= x pivot)) others))))))
(define (flatmap f l)
  (foldr append '() (map f l)))

;(define (prime-sum-pairs n)
 ; (flatmap (lambda (j) (map (lambda (ii) (list ii j (+ ii j))) (filter (lambda (i)  (prime? (+ i j))) (enumerate-interval (+ j 1) n )))) (enumerate-interval 1 n)))



(define (selection-sort l)
  (if (null? l)
      '()
      (cons (apply min l) (selection-sort (remove (apply min l) l)))))

(define (prime-sum-pairs n)
  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

  (define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair)(cadr pair))))


  (define pairs
    (flatmap (lambda (i) (map (lambda (j) (list i j))
                              (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

  (map make-pair-sum (filter prime-sum? pairs))
  )


(define (compose . fns)
  (if (null? fns)
      identity
      (compose-two (car fns) (apply compose (cdr fns)))))

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

(define (zip-with* fn . ls)
  (if (or (null? ls)
          (any? null? ls))
      '()
      (cons (apply fn (map car ls))
            (apply zip-with* fn (map cdr ls)))))

(define (juxt . fns)
  (lambda args (map (lambda (f) (apply f args)) fns)))

(define (matrix? m)
  (and (list? m)
       (not (null? (car m)))
       (all? list? m)
       (all? (lambda (row) (= (length row)
                              (length (car m)))) m)))

(define get-rows length)
(define (get-columns m) (length (car m)))

(define get-first-row car)
(define (get-first-column m) (map car m))

(define del-first-row cdr)
(define (dek-first-column m) (map cdr m))

(define (get-row i m) (list-ref m i))

(define (get-column i m)
  (map (lambda (row) (list-ref row i)) m))

(define (transpose m)
  (if (null? (get-first-row m))
      '()
      (cons (get-first-col m)
            (transpose (del-first-col m)))))

(define (transpose-accumulate m)
  (accumulate cons '() 0 (- (get-columns m) 1) (lambda (i) (get-column i m)) (lambda (x) (+ x 1))))

(define (sum-vectors v1 v2)
  (map + v1 v2))

(define (sum-matrices m1 m2) (map sum-vectors m1 m2))

(define (mult-vectors v1 v2) (apply + (map * v1 v2)))

(define (mult-matrices m1 m2)
  (let ((m2t (transpose m2)))
        (map (lambda (row)
               (map (lambda (column) (mult-vectors row column)) m2t)) m1)))

(define (make-rat n d)
  (cons 'rat (if (= d 0) (cons n 1)
      (let* ((g (gcd n d))
             (ng (quotient n g))
             (dg (quotient d g)))
        (if (> dg 0) (cons ng dg)
                     (cons (- ng) (- dg))))
      )))


(define (*rat p q)
  (make-rat
     (* (get-numer p) (get-numer q))
     (* (get-denom p) (get-denom q))))

(define (+rat p q)
    (make-rat
       (+ (* (get-numer p) (get-denom q))
          (* (get-denom p) (get-numer q)))
       (* (get-denom p) (get-denom q))))

(define (<rat p q)
         (< (* (get-numer p) (get-denom q))
            (* (get-numer q) (get-denom p))))

(define (my-exp x n)
  (accumulate +rat (make-rat 0 1) 0 n (lambda (i) (make-rat (pow x i) (fact i))) (lambda (i) (+ i 1))))

(define (make-rat n d)
   (let* ((g (gcd n d))
         (numer (quotient n g))
         (denom (quotient d g)))
    (lambda (prop)
          (case prop ('get-numer numer)
                     ('get-denom denom)
                     ('print (cons numer denom))
                     (else 'unknown-prop)))))


(define (rat? p)
  (and (pair? p) (eqv? (car p) 'rat)
       (pair? (cdr p))
       (integer? (cadr p))
       (positive? (cddr p))
       (= 1 (gcd (cadr p) (cddr p)))))

(define (check-rat f)
  (lambda (p)
    (if (rat? p) (f p) 'error)))

(define get-numer (check-rat cadr))
(define get-denom (check-rat cddr))

(define (make-rat n d)
        (let* ((g (gcd n d))
              (numer (quotient n g))
              (denom (quotient d g)))
        (define (self prop . params)
              (case prop  ('get-numer numer)
                          ('get-denom denom)
                          ('print (cons numer denom))
                          ('* (let ((r (car params)))
                                  (make-rat (* (self ’get-numer) (r 'get-numer))
                                            (* (self ’get-denom) (r 'get-denom)))))
                          (else 'unknown-prop)))
         self) )


(define (tree? t)
  (or (null? t)
      (and (list t) (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define empty-tree '())
(define (make-tree root left right) (list root left right))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define (depth-tree t)
  (if (empty-tree? t)
      0
      (+ 1 (max (depth-tree (left-tree t))
                (depth-tree (right-tree t))))))

(define (memv-tree x t)
  (cond ((empty-tree? t) #f)
        ((eqv? x (root-tree t)) t)
        (else (or (memv-tree x (left-tree t))
                  (memv-tree x (right-tree t))))))

(define (cons#f h t) (and t (cons h t)))

(define (path-tree x t)
  (cond ((empty-tree? t) #f)
        ((eqv? x (root-tree t)) (list x))
        (else (cons#f (root-tree t)
                      (or (path-tree x (left-tree t))
                          (path-tree x (right-tree t)))))))

(define (make-alist f keys)
  (map (lambda (x) (cons x (f x))) keys))

(define (keys alist) (map car alist))
(define (vvalues alist) (map cdr alist))

(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (add-assoc key value alist)
  (cons (cons key value) (del-assoc key alist)))

(define (add-key-value key value alist)
         (let ((new-kv (cons key value)))
              (cond ((null? alist) (list new-kv))
                    ((eqv? (caar alist) key) (cons new-kv (cdr alist)))
                    (else (cons (car alist)
                                (add-key-value key value (cdr alist)))))))

(define (add-key-value-s key value alist)
  (let ((kv (cons key value)))
       (if (assq key alist)
           (map (lambda (kvv) (if (eq? (car kv) key) kvv )) alist)
           (cons kv alist))))

(define (search p l)
  (and (not (null? l))
       (or (p (car l)) (search p (cdr l)))))

(define (my-assq key al)
  (search (lambda (kv) (and (eq? (car kv) key) kv)) al))

(define (al-map f l)
  (map (lambda (kv) (cons (car kv) (f (cdr kv)))) l))

(define (al-filter p l)
  (filter (lambda (kv) (p (cdr kv))) l))

(define (al-all? p l)
  (not (search (lambda (x) (not (p x))) l)))

(define vertices keys)

(define (children v g)
  (cdr (assv v g)))

(define (edge? u v g)
  (memv v (children u g)))

(define (map-children v f g)
  (map f (children v g)))

(define (search-child v f g)
  (search f (children v g)))

(define (make-graph g)
  (define (self prop . params)
             (case prop
                  ('print g)
                  ('vertices (keys g))
                  ('children (let ((v (car params)))
                                  (cdr (assv v g))))
                  ('edge? (let ((u (car params)) (v (cadr params)))
                               (memv v (self 'children u)))) 

                  ('map-children (let ((v (car params))
                                      (f (cadr params)))
                                      (map f (self 'children v))))
                  ('search-child (let ((v (car params))
                                      (f (cadr params))) 
                                 (search f (self 'children v))))))
  self)

(define (childless g)
    (filter (lambda (v) (null? (children v g))) (vertices g)))

(define (parents v g)
  (filter (lambda (u) (edge? u v g)) (vertices g)))

(define (symmetric? g)
   (every? (lambda (u) (every? (lambda (v) (edge? v u g)) (children u g)))  (vertices g)))

(define (dfs-path u v g)
        (define (dfs-search path)
            (let ((current (car path)))
                 (cond ((eqv? current v) (reverse path))
                       ((memv current (cdr path)) #f)
                       (else (search-child current
                                           (lambda (w) dfs-search (cons w path)) g)))))
        (dfs-search (list u)))



(define (dimensions matrix)
  (if (null? matrix) (cons 0 0) (cons (length matrix) (length (car matrix)))))

(define (reverse-columns m)
  (map (lambda (column) (reverse column)) m))

(define (nth-column m n)
   (map (lambda (row) (list-ref row n)) m))

(define (main-diagonal m)
  (map (lambda (row i) (list-ref row i)) m (enumerate-interval 0 (length m))))

(define (transpose m)
  (map (lambda (i) (nth-column m i)) (enumerate-interval 0 (- (cdr (dimensions m)) 1))))

(define (for-all-columns? p m)
   (every? p (transpose m)))

(define (odd-exists? l)
  (any? odd? l))

(define (prime-in-each-column? matrix)
  (for-all-columns? (lambda (l) (any? prime? l)) matrix))

(define (mult-vector v1 v2)
  (apply + (map * v1 v2)))

(define (multiply a b)
  (map (lambda (row) (map (lambda (col) (mult-vector row col)) (transpose b))) a))

(define (subset? row col)
  (every? (lambda (el) (member el row)) col))

(define (ok-vector? v m)
  (any? (lambda (row) (subset? row v)) m))

(define (count-columns m)
  (length (filter (lambda (col) (ok-vector? col m)) (transpose m))))

(define (tree? t)
    (or (null? t)
        (and (list? t)
             (= (length t) 3)
             (tree? (cadr t))
             (tree? (caddr t)))))

(define empty-tree '())

(define (make-tree root left right)
    (list root left right))

(define (leaf root)
  (make-tree root empty-tree empty-tree))

(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define (leaf? tree)
     (and (not (empty-tree? tree))
          (empty-tree? (left-tree tree))
          (empty-tree? (right-tree tree))))

(define (pre-order tree)
   (if (empty-tree? tree)
       '()
       (append (list (root-tree tree))
               (pre-order (left-tree tree))
               (pre-order (right-tree tree)))))

(define (in-order tree)
   (if (empty-tree? tree)
       '()
       (append (in-order (left-tree tree))
               (list (root-tree tree))
               (in-order (right-tree tree)))))

(define (post-order tree)
   (if (empty-tree? tree)
       '()
       (append (post-order (left-tree tree))
               (post-order (right-tree tree))
               (list (root-tree tree))
               )))

(define tree
  (make-tree 1
             (make-tree 2
                        (leaf 4)
                        (leaf 5))
             (leaf 3)))

(define (level n tree)
   (if (empty-tree? tree)
       '()
       (if (= n 0)
           (list (root-tree tree))
           (append (level (- n 1) (left-tree tree))
                   (level (- n 1) (right-tree tree))))))

(define (count-leaves tree)
    (if (empty-tree? tree)
        0
        (if (leaf? tree)
            1
            (+ (count-leaves (left-tree tree))
               (count-leaves (right-tree tree))))))

(define (map-tree fn tree)
     (if (empty-tree? tree)
         empty-tree
         (make-tree (fn (root-tree tree))
                    (map-tree fn (left-tree tree))
                    (map-tree fn (right-tree tree)))))

(define (binary-heap? tree)
     (if (empty-tree? tree)
         #t
         (and (or (empty-tree? (left-tree tree)) (< (root-tree tree) (root-tree (left-tree tree))))
              (or (empty-tree? (right-tree tree)) (< (root-tree tree) (root-tree (right-tree tree))))
              (binary-heap? (left-tree tree))
              (binary-heap? (right-tree tree)))))

(define (height tree)
    (if (empty-tree? tree)
        0
        (+ 1 (max (height (left-tree tree)) (height (right-tree tree))))))

(define (balanced? tree)
   (if (empty-tree? tree)
       #t
       (and (<= (abs (- (height (left-tree tree)) (height (right-tree tree)))) 1)
            (balanced? (left-tree tree))
            (balanced? (right-tree tree)))))

(define (make-alist f keys)
  (map (lambda (key) (cons key (f key))) keys))

(define (keys alist)
  (map car alist))

(define (vvalues alist)
  (map cdr alist))

(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? key (car kv)))) alist))

(define (add-assoc key value alist)
    (cons (cons key value)
          (del-assoc key alist)))

(define (run-length-encode l)
     (if (null? l)
         '()
         (cons (cons (car l) (length (take-while (lambda (x) (equal? x (car l))) l)))
               (run-length-encode (drop-while (lambda (x) (equal? x (car l))) l)))))

(define (make x n)
  (if (= n 0)
      '()
      (cons x (make x (- n 1)))))

(define (run-length-decode l)
     (if (null? l)
         '()
         (append (make (caar l) (cdr (car l)))
                 (run-length-decode (cdr l)))))

(define (remove x l)
   (filter (lambda (i) (not (equal? x i))) l))

(define (count x l)
    (length (filter (lambda (i) (equal? x i)) l)))

(define (histogram l)
    (if (null? l)
        '()
        (cons (cons (car l) (count (car l) l))
              (histogram (remove (car l) l)))))

(define (make-new-list kv alist)
     (append (list (car kv)) (map cdr (filter (lambda (x) (equal? (car x) (car kv))) alist))))

(define (group-by f l)
   (define alist (map (lambda (x) (cons (f x) x)) l))
   (if (null? alist)
       '()
       (cons (make-new-list (car alist) alist)
               (group-by f (map cdr (del-assoc (caar alist ) alist)))))
 )

(define empty-graph '())

(define (make-graph vs)
  (make-alist (lambda (_) '())
              vs))

(define vertices keys)
(define (children v g)
  (cdr (assoc v g)))
(define (edge? u v g)
  (member v (children u g)))

(define (map-children v f g)
  (map f (children v g)))

(define (any? p l)
  (and (not (null? l))
       (or (p (car l))
           (any? p (cdr l)))))

(define (search-child v p g)
  (any? p (children v g)))

(define (add-vertex v g)
  (if (assoc v g)
      g
      (add-assoc v '() g)))

(define (add-if-missing x l)
  (if (member x l)
      l
      (cons x l)))

(define (add-edge u v g)
  (let ((g-with-u-v (add-vertex v (add-vertex u g))))
    (add-assoc u
               (add-if-missing v (children u g-with-u-v))
               g-with-u-v)))

(define (remove-edge u v g)
  (add-assoc u
             (filter (lambda (u)
                       (not (equal? u v)))
                     (children u g))
             g))

(define (degree v g)
   (+ (length (children v g)) (length (filter (lambda (u) (member v (children u g) )) (vertices g)))))

(define (edges g)
   (flatmap (lambda (v) (map (lambda (u) (list v u)) (children v g))) (vertices g)))

 (define g (make-graph '(1 2 3 4 5)))
 (define gg (add-edge 3 5(add-edge 1 2 (add-edge 1 3 g))))

 (define (symmetric? g)
   (every? (lambda (u) (every? (lambda (v) (edge? v u g)) (children u g))) (vertices g)))

 (define (invert g)
     (foldl
      (lambda (inverted edge) (add-edge (cdr edge) (car edge) inverted))
      (make-graph (vertices g))
      (edges g))
     )