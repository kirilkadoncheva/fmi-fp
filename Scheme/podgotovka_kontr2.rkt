;2016 - вариант А -----------------------------------------------------

; - Задача 1 -
(define (drop-while p l)
  (if (null? l)
      l
      (if (p (car l))
          (drop-while p (cdr l))
          l)))

(define (setUnion l1 l2)
   (cond 
         ((null? l1) l2)
         ((null? l2) l1)
         (else (if (< (car l1) (car l2))
             (cons (car l1) (setUnion (drop-while (lambda (x) (equal? x (car l1))) l1)
                                      (drop-while (lambda (x) (equal? x (car l1))) l2)))
             (cons (car l2) (setUnion (drop-while (lambda (x) (equal? x (car l2))) l1)
                                      (drop-while (lambda (x) (equal? x (car l2))) l2)))))))

(define (setIntersect l1 l2)
  (if (or (null? l1) (null? l2))
        '()
        (if (<= (car l1) (car l2))
             (if (member (car l1) l2)(cons (car l1) (setIntersect (drop-while (lambda (x) (equal? x (car l1))) l1)
                                      (drop-while (lambda (x) (equal? x (car l1))) l2)))
                 
                                      (setIntersect (drop-while (lambda (x) (equal? x (car l1))) l1)
                                      l2))
             (if (member (car l2) l1)(cons (car l2) (setIntersect (drop-while (lambda (x) (equal? x (car l2))) l1)
                                      (drop-while (lambda (x) (equal? x (car l2))) l2)))
                                      (setIntersect  l1
                                      (drop-while (lambda (x) (equal? x (car l2))) l2))))))

(define (setDiff l1 l2)
  (cond ((null? l1) '())
        ((null? l2) l1)
        (else (if (member (car l1) l2)
            (setDiff (cdr l1) l2)
            (cons (car l1) (setDiff (cdr l1) l2))))
      ))

(define (filter p l)
   (if (null? l)
       '()
       (if (p (car l))
           (cons (car l) (filter p (cdr l)))
           (filter p (cdr l)))))

(define (sort l)
   (if (null? l)
       '()
       (append (sort (filter (lambda (x) (<= x (car l))) (cdr l))) (list (car l))
               (sort (filter (lambda (x) (> x (car l))) (cdr l)))))
  )

(define (setSymDiff l1 l2)
     (if (or (null? l1) (null? l2))
         '()
         (sort (append (setDiff l1 l2) (setDiff l2 l1)))))

; - Задача 2 -

(define (getLengths l)
  (if (null? l)
      '()
      (cons  (length (car l)) (getLengths (cdr l)))))

(define (repeats x l)
  (if (null? l)
      0
      (if (= x (car l))
          (+ 1 (repeats x (cdr l)))
          (repeats x (cdr l)))))


(define (histogram l)
    (if (null? l)
        '()
        (cons (cons (car l) (repeats (car l) l)) (histogram (filter (lambda (x) (not (= x (car l))))  l)))))
(define (sortByLengths l)
    (if (null? l)
        '()
        (append (sortByLengths (filter (lambda (x) (<= (cdr x) (cdr (car l)))) (cdr l))) (list (car (car l)))
                (sortByLengths (filter (lambda (x) (> (cdr x) (cdr (car l)))) (cdr l))))))


(define (ifSort l)
  (define lengths (sortByLengths (histogram (getLengths l))))
  (define (for s)
    (if (null? s)
        '()
        (append (filter (lambda (x) (= (car s) (length x))) l) (for (cdr s)))))
  (for lengths))

(define l '((a b c) (d e) (f g h) (f p) (i j k l) (m n) (o)))

; - Задача 4 -

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

(define (prune t)
  (if (empty-tree? t)
      empty-tree
      (if (not (leaf? t))
          (make-tree (root-tree t) (prune (left-tree t)) (prune (right-tree t)))
          empty-tree)))

(define tree
  (make-tree 1
             (make-tree 2
                        (leaf 4)
                        (leaf 5))
             (leaf 3)))
(define (in-order tree)
  (if (empty-tree? tree)
      empty-tree
      (append (in-order (left-tree tree))
              (list (root-tree tree))
              (in-order (right-tree tree)))))

; 2018 - Вариант А ----------------------------------------------------------

; - Задача 2 -
; - A -
(define (make-leaf x)
  (make-tree x empty-tree empty-tree))

(define (grow t x)
    (if (empty-tree? t)
        empty-tree
        (if (leaf? t)
            (make-tree (root-tree t) (make-leaf x) (make-leaf x))
            (make-tree (root-tree t) (grow (left-tree t) x) (grow (right-tree t) x)))))
; - B -
(define (level n tree) 
    (cond((empty-tree? tree) empty-tree)
          ((= n 0) (list( root-tree tree)))
           (else (append (level (- n 1) (left-tree tree)) (level (- n 1) (right-tree tree)))
                 )))

(define empty-stream '())

(define-syntax cons-stream
  (syntax-rules ()
                ((cons-stream h t)
                 (cons h (delay t)))))

(define (empty-stream? s)
  (equal? s empty-stream))

(define head car)

(define (tail s)
  (force (cdr s)))

(define (make-tree-of-height n s)
 
  (if (= n 0)
      empty-tree
      (make-tree s (make-tree-of-height (- n 1) (+ s 1)) (make-tree-of-height (- n 1) (+ s 1)))))

(define (for i)
      (cons-stream (make-tree-of-height i 0) (for (+ 1 i))))
 
(define growingTrees
  
  (for 0))

(define (take-stream-list n s)
  (if (or (= n 0) (empty-stream? s))
      '()
      (cons (car s) (take-stream-list (- n 1) (tail s)))))

; - Задача 3 -
(define (make-show str h m long)
    (list str (list h m) long))

(define (show? s)
    (and (list? s)
         (string? (car s))
         (list? (cadr s))
         (= 2 (length (cadr s)))
         (number? (caddr s))))

(define getName car)
(define (getEnd show) (+ (+ (* 60 (car (cadr show))) (cadr (cadr show))) (caddr show)))

(define (getEnds shows)
   (map (lambda (x) (cons x (getEnd x))) shows))
(define (foldr combiner nv l)
        (if (null? l) nv
            (combiner (car l) (foldr combiner nv (cdr l)))))

(define (lastShow shows)
  (define maxL (foldr max 0 (map (lambda (x) (cdr x)) (getEnds shows))))
  (getName (car (car (filter (lambda (x) (= (cdr x) maxL)) (getEnds shows))))))

(define shows (list (make-show "A" 11 0 120) (make-show "B" 12 0 70) (make-show "A" 10 30 90)))

(define (longestProgram shows)
  (define maxL (foldr max 0 (map (lambda (x) (caddr x)) shows)))
  (getName (car (filter (lambda (x) (= (caddr x) maxL)) shows))))