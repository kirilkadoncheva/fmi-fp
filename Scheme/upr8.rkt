
;ГРАФИ ----------------------------------------------------------
(define empty-graph '())

  
(define (make-alist f keys)
  (map (lambda (key)
         (cons key (f key)))
       keys))

(define (make-graph vs)
  (make-alist (lambda (_) '())
              vs))

(define (keys l)
  (map (lambda(x) (car x)) l))

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

(define (degree- v g)
    (length (children v g)))

(define (filter p l)
      (if (null? l)
          l
          (if (p (car l))
              (cons (car l) (filter p (cdr l)))
              (filter p (cdr l)))))

(define (degree+ v g)
     (length (filter (lambda (u) (edge? u v g)) (vertices g))))

(define (degree v g)
  (+ (degree- v g) (degree+ v g)))

(define g '((1 2 3) (2 3) (3 4) (4 5 ) (5 ) (6)))

(define (edges g)
  (apply append (map (lambda (v)
                       (map-children v (lambda(child)
                                           (cons v child)) g))  (vertices g))))
(define (every? p l)
   (or (null? l) (and (p (car l)) (every? p (cdr l)))))

(define (symmetric? g)
 (every? (lambda (edge)
          (edge? (cdr edge) (car edge) g) )
         (edges g)))




(define (foldl combiner nv l)
        (if (null? l) nv
            (foldl combiner (combiner (car l) nv) (cdr l))))

(define (invert g)
  (foldl (lambda (inverted edge)
           (add-edge (cdr edge) (car edge) inverted))
         (make-graph (vertices g))
         (edges g))); ne stava

(define (path? u v g)
   (define (dfs visited)
          (or (equal? v (car visited))
              (search-child (car visited)
                            (lambda (child) (and (not (member child visited)) (dfs (cons child visited))))
                            g)) )
  (dfs (list u)))

;(define (acyclic? g)
;    ); ?????

;ПОТОЦИ ----------------------------------------------------

(define empty-stream '())

(define (take-stream-list n s)
  (if (or (= n 0) (empty-stream? s))
      '()
      (cons (car s) (take-stream-list (- n 1) (tail s)))))

(define-syntax cons-stream
  (syntax-rules ()
                ((cons-stream h t)
                 (cons h (delay t)))))

(define (take-stream n s)
  (if (or (= n 0) (empty-stream? s))
      empty-stream
      (cons-stream (car s) (take-stream (- n 1) (tail s)))))



(define (empty-stream? s)
  (equal? s empty-stream))

(define head car)

(define (tail s)
  (force (cdr s)))

(define (repeat value)
  (define (construct str value)
      (cons-stream value (construct str value)))
  (construct empty-stream value))

(define (append-str stream l)
   (if (null? l)
       stream
       (cons-stream (car l) (append-str  stream (cdr l)) )))

(define (cycle l)
  (cons-stream (car l) (cycle (append (cdr l) (list (car l)))))
  )


(define (iterate f x)
  (cons-stream x (iterate f (f x))))

(define (square x) (* x x))







(define (integers-from n)
   (cons-stream n (integers-from (+ n 1)))
  )



(define (flatmap f l)
  (apply append (map f l)))

(define (range-stream from to)
    (take-stream (+ to (- from) 1) (integers-from from))
  )

(define (append-stream s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1) (append-stream (tail s1) s2)))
  )

(define (concat-streams ss)
  (cond ((empty-stream? ss) empty-stream)
        ((empty-stream? (head ss)) (concat-streams (tail ss)))
        (else (cons-stream (head (head ss))
                         (append-stream (tail (head ss))
                                        (concat-streams (tail ss)))))))
                    
(define (flatmap-stream f s)
     (concat-streams (map-stream f s))
  )

(define (map-stream f s)
    (if (empty-stream? s)
        empty-stream
        (cons-stream (f (car s)) (map-stream f (tail s))))
  )

(define triples
   (flatmap-stream (lambda (c)
                 (flatmap-stream (lambda (b)
                               (map-stream (lambda (a)
                                             (list a b c)
                                             ) (range-stream 1 b))) (range-stream 1 c))
                 )
               (integers-from 1))
  )
(define (filter-stream p s)
      (if (empty-stream? s)
          empty-stream
          (if (p (head s))
              (cons-stream (head s) (filter-stream p (tail s)))
              (filter-stream p (tail s)))))

(define pythagorean-triples
  (filter-stream
   (lambda (triple) (= (+ (square (car triple)) (square (car (cdr triple))))
                       (square (car (cdr (cdr triple)))))) triples))