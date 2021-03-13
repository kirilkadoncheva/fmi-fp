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

(define (search t x)
   (if (empty-tree? t)
       #f
       (or (= x (root-tree t))
           (search (left-tree t) x)
           (search (right-tree t) x))))

(define (findCode t x)
   (if (not (search t x))
       0
       (cond ((= (root-tree t) x) '())
             ((search (left-tree t) x)  (cons 0 (findCode (left-tree t) x)))
             (else (cons 1 (findCode (right-tree t) x))))
           ))

(define (toDecimal l)
  
   (define (iter i x ll)
       (if (> i (- (length l) 1))
           0
           (+ (* x (car ll)) (iter (+ i 1) (* x 2) (cdr ll)))))
     (iter 0 1 (reverse l)))

(define (getCode t x)
   (if (number? (findCode t x))
       0
       (toDecimal (cons 1 (findCode t x)))))


(define (in-order tree)
   (if (empty-tree? tree)
       '()
       (append (in-order (left-tree tree))
               (list (root-tree tree))
               (in-order (right-tree tree)))))

(define (filter p l)
   (if (null? l)
       '()
       (if (p (car l))
           (cons (car l) (filter p (cdr l)))
           (filter p (cdr l)))))

(define (sameAsCode t)
    (define ll (filter (lambda (n) (= n (getCode t n)) ) (in-order t)))
  (if (null? ll)
      0
      (car ll)))

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
(define (minList l)
    (foldr min (car l) (cdr l)))

(define (intervalTree t)
     (if (empty-tree? t)
         empty-tree
         (cond ((leaf? t) (leaf (cons root-tree root-tree)))
               (else (let ((l (intervalTree left-tree))
                           (r (intervalTree right-tree))
                           )
                     (make-tree
                          (cons (min (car (root-tree l)) (car (root-tree r)))
                           (max (cdr (root-tree l)) (cdr (root-tree r))))
                           l r))))))

(define t (make-tree 5 (make-tree 3 (make-tree 1 empty-tree (leaf 2)) (leaf 4)) (leaf 6)))