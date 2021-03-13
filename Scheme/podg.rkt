(load "/Documents/uni/2_курс/ФП/Scheme/check.rkt")

(define (two-compose f g)
  (lambda (x) (f (g x))))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (double x) (* 2 x))
(define (square x) (* x x))

(define (compose . fns)
  (if (null? fns)
      identity
      (two-compose (car fns) (apply compose (cdr fns)))))

(define (flip fn)
  (lambda args (apply fn (reverse args))))

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (list (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))


(define (zip-with fn l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (fn (car l1) (car l2)) (zip-with fn (cdr l1) (cdr l2)))))
(define (every? p l)
  (if(null? l)
     #t
     (and (p (car l)) (every? p (cdr l)))))

(define (any? p l)
  (not (every? (lambda (x) (not (p x))) l)))

(define (zip-with* fn . ls)
  (if (or (null? ls) (any? null? ls))
      '()
      (cons (apply fn (map car ls)) (apply zip-with* fn (map cdr ls)))))

(define (juxt . fns)
  (lambda args (map (lambda (fn)
                      (apply fn args)) fns)))

(define (dimensions matrix)
   (cons (length matrix) (length (car matrix))))

(define (reverse-columns matrix)
  (map reverse matrix))

(define (nth-column matrix n)
  (map (lambda (row) (list-ref row n)) matrix))

(define (enumerate-interval a b)
  (if (> a b)
      '()
      (cons a (enumerate-interval (+ a 1) b))))

(define (main-diagonal matrix)
  (map (lambda (el) (list-ref (car el) (car (cdr el)))) (zip matrix (enumerate-interval 0 (- (length (car matrix)) 1)))))

(define (transpose matrix)
 (map (lambda (el) (nth-column matrix el))  (enumerate-interval 0 (- (length (car matrix)) 1))))

(define (for-all-columns? p matrix)
  (every? p (transpose matrix)))

(define (odd-exists? l)
  (any? odd? l))


(define (divides? k n)
  (= (remainder n k) 0))

(define (prime? n)
  (define (iter k)
    (or (= k n)
        (and (not (divides? k n))
             (iter (+ k 1)))))

  (and (not (= n 1)) (iter 2)))

(define (prime-in-each-column? matrix)
  (every? (lambda (x) (any? prime? x)) (transpose matrix)))

(define (multiply-vectors a b)
  (apply + (map * a b)))

(define (multiply a b)
   (map (lambda (row) (map (lambda (column) (multiply-vectors row column)) (transpose b))) a))

(define (subset? a b)
   (every? (lambda (x) (member x b)) a))

(define (subset-of-row? column matrix)
  (any? (lambda (row) (subset? column row)) matrix))

(define (filter p l)
  (if (null? l)
      '()
      (if (p (car l))
          (cons (car l) (filter p (cdr l)))
          (filter p (cdr l)))))

(define (count-columns matrix)
  (length (filter (lambda (x) (subset-of-row? x matrix)) (transpose matrix))))

;Trees ----------------------------------------------------------------

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
      (append
      
              (post-order (left-tree tree))
              (post-order (right-tree tree))
               (list (root-tree tree)))))
(define (level n tree)
  (cond ((empty-tree? tree) '())
        ((= n 0) (list (root-tree tree)))
        (else (append (level (- n 1) (left-tree tree)) (level (- n 1) (right-tree tree))
                      ))))

(define tree
  (make-tree 1
             (make-tree 2
                        (leaf 4)
                        (leaf 5))
             (leaf 3)))

(define (count-leaves tree)
  (if (empty-tree? tree)
      0
      (if (leaf? tree)
          (+ 1 (count-leaves (left-tree tree))  (count-leaves (right-tree tree)))
          (+ (count-leaves (left-tree tree))  (count-leaves (right-tree tree))))))

(define (map-tree fn tree)
    (if (empty-tree? tree)
      empty-tree
      (make-tree  (fn (root-tree tree))
                  (map-tree fn (left-tree tree))
             
              
              (map-tree fn (right-tree tree)))))

(define (binary-heap? tree)
  (if (empty-tree? tree)
       #t
       (and (binary-heap? (left-tree tree))
            (binary-heap? (right-tree tree))
            (or (empty-tree? (left-tree tree))
                 (< (root-tree tree) (root-tree (left-tree tree))))
            (or (empty-tree? (right-tree tree))
                 (< (root-tree tree) (root-tree (right-tree tree))))
            )))

(define (square x) (* x x))
(define (cube x) (* x x x))



(define squared-tree
  (make-tree 1
             (make-tree 4
                        (leaf 16)
                        (leaf 25))
             (leaf 9)))

(define cubed-tree
  (make-tree 1
             (make-tree 8
                        (leaf 64)
                        (leaf 125))
             (leaf 27)))


(define (height tree)
  (if (empty-tree? tree)
      0
      (+ 1 (max (height (left-tree tree)) (height (right-tree tree))))))



(define (balanced? tree)
  (if (empty-tree? tree)
      #t
      (and (balanced? (left-tree tree))
           (balanced? (right-tree tree))
           (<= (abs (- (height (left-tree tree)) (height (right-tree tree)))) 1))))

; Association lists ----------------------------------

(define (make-alist f keys)
  (map (lambda (key) (cons key (f key))) keys))

(define (akeys alist)
  (map car alist))

(define (avalues alist)
  (map cdr alist))

(define (del-assoc key alist)
  (filter (lambda (kv) (not (equal? (car kv) key))) alist))

(define (add-assoc key value alist)
  (cons (cons key value) (del-assoc key alist)))

(define (count-repeats x l)
  (if (null? l)
      0
      (if (equal? x (car l))
          (+ 1 (count-repeats x (cdr l)))
          0)))

(define (drop-while p l)
  (if (null? l)
      '()
      (if (p (car l))
          (drop-while p (cdr l))
          l)))

(define (run-length-encode l)
 (if (null? l)
     '()
     (cons (cons (car l) (count-repeats (car l) l))
           (run-length-encode (drop-while (lambda (x) (equal? x (car l))) (cdr l))))))

(define (make-part-list kv)
  (define (for i)
    (if (> i (cdr kv))
        '()
        (cons (car kv) (for (+ i 1)))))
  (for 1))

(define (run-length-decode l)
    (if (null? l)
        '()
        (append (make-part-list (car l)) (run-length-decode (cdr l)))))

(define (count-repeats x l)
  (if (null? l)
      0
      (if (equal? x (car l))
          (+ 1 (count-repeats x (cdr l)))
          (count-repeats x (cdr l)))))

(define (histogram l)
  
  (if (null? l)
      '()
      (cons (cons (car l) (count-repeats (car l) l)) (histogram (filter (lambda (x) (not (equal? x (car l)))) l))))
  )

(define (unique l)
  (if (null? l)
      '()
      (cons (car l)
            (unique (filter (lambda (x)
                              (not (equal? x (car l))))
                            l)))))
     
  (define (group-by f l)
  (make-alist (lambda (key)
                (filter (lambda (x)
                          (equal? (f x) key))
                        l))
              (unique (map f l))))

; Graphs -------------------------------------------
  (define empty-graph '())

  (define (make-graph vs)
  (make-alist (lambda (_) '())
              vs))

  (define vertices akeys)
  
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
  (+ (length chidren v) (length (filter (lambda (vertex) (edge? vertex v g)) (vertices g)))))

(define (flatmap f l)
  (apply append (map f l)))

(define (edges g)
  (flatmap (lambda (u)
             (map-children u (lambda (v) (cons u v)) g)) (vertices g)))

(define (symmetric? g)
  (every? (lambda (edge) (edge? (cdr edge) (car edge) g)) (edges g)))


(define (foldl combiner nv l)
        (if (null? l) nv
            (foldl combiner (combiner (car l) nv) (cdr l))))

(define (invert g)
  (foldl (lambda (inverted edge)
           (add-edge (cdr edge) (car edge) inverted))
         (make-graph (vertices g))
    (edges g)))

(define (path? u v g)
  (define (dfs path)
    (let ((current (car path)))
      (or (equal? current v)
          (and (not (member current (cdr path)))
               (search-child current (lambda (child) (dfs (cons child path)) g))))))
  (dfs (list u)))

(define (acyclic? g)
  )