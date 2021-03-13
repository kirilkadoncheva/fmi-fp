(load "/Documents/uni/2_курс/ФП/Scheme/check.rkt")

;TREES --------------------------------------------------------------------
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
     (if(empty-tree? tree)
        '()
        (cons (root-tree tree) (append (pre-order (left-tree tree)) (pre-order (right-tree tree))))))

(define (in-order tree)
     (if(empty-tree? tree)
        '()
        (append (in-order (left-tree tree))
                (list (root-tree tree))
                (in-order (right-tree tree)))))

(define (post-order tree)
     (if(empty-tree? tree)
        '()
        (append 
               
                (post-order (left-tree tree))
                (post-order (right-tree tree))
                 (list (root-tree tree)))))

(define (level n tree)
     
        
     (cond((empty-tree? tree) empty-tree)
          ((= n 0) (list( root-tree tree)))
           (else (append (level (- n 1) (left-tree tree)) (level (- n 1) (right-tree tree))))))


(define (count-leaves tree)
    (if (empty-tree? tree)
        0
        (if (leaf? tree)
            1
            (+ (count-leaves (left-tree tree)) (count-leaves (right-tree tree))))))

(define (map-tree fn tree)
     (if (empty-tree? tree)
         empty-tree
         (make-tree (fn (root-tree tree))
                    (map-tree fn (left-tree tree))
                    (map-tree fn (right-tree tree)))))

(define (binary-heap? tree)
    (if (empty-tree? tree)
        #t
        (and (or (empty-tree? (left-tree tree))(< (root-tree tree) (root-tree (left-tree tree))) )
             (or (empty-tree? (right-tree tree))(< (root-tree tree) (root-tree (right-tree tree))))
             (binary-heap? (left-tree tree))
             (binary-heap? (right-tree tree))
             )))

(define (height tree)
   (if (empty-tree? tree)
       0
       (+ 1 (max (height (left-tree tree)) (height (right-tree tree))))))

(define (my-abs a)
  (if (< a 0)
      (- a)
      a))
(define (balanced? tree)
     (if (empty-tree? tree)
         #t
         (and (balanced? (left-tree tree))
              (balanced? (right-tree tree))
              (< (my-abs (- (height (left-tree tree)) (height (right-tree tree)))) 2))))

; ASSOCIATION LISTS -------------------------------------------------------------------

(define (make-alist f keys)
  (map (lambda (key)
         (cons key (f key)))
       keys))

(define (keys alist)
  (map car alist))

(define (my-values alist)
  (map cdr alist))
; (assoc key alist), (assv key alist), (assq key alist)

(define (del-assoc key alist)
  (filter (lambda (kv)
            (not (equal? (car kv) key)))
          alist))

(define (add-assoc key value alist)
  (cons (cons key value)
        (del-assoc key alist)))

(define (drop-while p l)
    (if (null? l)
        l
        (if (p (car l))
            (drop-while p (cdr l))
            l)))

(define (take-while p l)
    (if (null? l)
        l
        (if (p (car l))
            (cons (car l)(take-while p (cdr l)))
           '())))

(define (run-length-encode l)
  (if (null? l)
      l
      (cons (cons (car l) (length (take-while (lambda (x) (= x (car l))) l)))
              (run-length-encode (drop-while (lambda (x) (= x (car l))) l)))
  ))

(define (make-repeated-list a n)
      (if (= n 0)
          '()
          (cons a (make-repeated-list a (- n 1)))))


(define (run-length-decode l)
     (apply append(map (lambda (x) (make-repeated-list (car x) (cdr x))) l)))

(define (counter x l)
  (if (null? l)
      0
      (if (= x (car l))
          (+ 1 (counter x (cdr l)))
          (counter x (cdr l)))))

(define (filter p l)
   (if (null? l)
       l
       (if (p (car l))
           (cons (car l) (filter p (cdr l)))
           (filter p (cdr l)))))

(define (histogram l)
       (if (null? l)
           l
           (cons
            (cons (car l) (counter (car l) l))
            (histogram (filter (lambda(x) (not (= x (car l)))) l)))))


(define (apply-f f l)
  (map (lambda (x) (cons  (f x) x)) l))

(define (make-list-for-key key l)
    (cons key (map (lambda (x) (cdr x)) (filter (lambda (x) (= (car x) key)) l))))

(define (clean-list key l)
  (filter (lambda (x) (not(= (car x) key))) l))

(define (group-by f l)
     (define new-l (apply-f f l))
     (define (make-the-list l)
             (if (null? l)
                 l
                 (cons (make-list-for-key (car (car l)) l)
                       (make-the-list (clean-list (car (car l)) l)))))
  (make-the-list new-l))