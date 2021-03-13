(load "/Documents/uni/2_курс/ФП/Scheme/check.rkt")

(define (make-alist f keys)
  (map (lambda (key)
         (cons key (f key)))
       keys))

(define (keys alist)
  (map car alist))
(define (vvalues alist)
  (map cdr alist))
; (assoc key alist), (assv key alist), (assq key alist)

(define (filter p l)
   (if (null? l)
       '()
       (if (p (car l))
           (cons (car l) (filter p (cdr l)))
           (filter p (cdr l)))))

(define (del-assoc key alist)
  (filter (lambda (kv)
            (not (equal? (car kv) key)))
          alist))

(define (add-assoc key value alist)
  (cons (cons key value)
        (del-assoc key alist)))


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

(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l)
      nv
      (foldl op (op nv (car l)) (cdr l))))

(define g (add-edge 3 5(add-edge 1 2 (add-edge 1 3 (make-graph '(1 2 3 4 5 6))))))

(define (flatmap f l)
  (apply append (map f l)))

(define (edges g)
  (flatmap (lambda (u) (map (lambda (v) (list u v)) (children u g))) (vertices g)))

(define (invert g)
  (foldl (lambda (inverted edge)
           (add-edge (cadr edge) (car edge) inverted))
         (make-graph (vertices g))
         (edges g)))

(define (path? u v g)
    (define (dfs path)
       (let ((current (car path)))
             (or (equal? current v)
                 (and (not (member current (cdr path)))
                      (search-child current (lambda (child)
                                              (dfs (cons child path))) g))
                 )))
  (dfs (list u)))

(define (path u v g)
     (define (dfs path)
         (let ((current (car path)))
              (cond ((equal? v current) (reverse path))
                    ((memv current (cdr path)) #f)
                    (else (search-child current (lambda (child) (dfs (cons child path))) g)))))
  (dfs (list u)))



(define (extend path)
     (map-children (car path) (lambda (w) (cons w path)) g))

(define (remains-acyclic? path)
   (not (mamv (car path) (cdr path))))

(define (extend-acyclic path)
    (filter remains-acyclic? (extend path)))

(define (dfs-all-paths u v g)
      (define (dfs-search path)
           (let ((last (car path)))
             (cond ((eqv? v last) (list (reverse path)))
                   ((memv last (cdr path)) '())
                   (else (apply append
                                (map-children last (lambda (w) (dfs-search (cons w path)))
                                              g))))))
  (dfs-search (list u)))

(define (bfs-path? u v g)
     (define (bfs-level l)
         (and (not (null? l))
              (or (and (memv v l) #t)
                  (bfs-level (apply append (map (lambda (w) (children w g)) l))))))
  (bfs-level (list u)))

(define (bfs-path u v g)
  ;за първия връх в пътя прави списък от пътища, състоящи се от пътя и децата на първия връь
   (define (extend path)
       (map-children (car path) (lambda (v) (cons v path)) g))
  ;проверява дали първия връх в пъта прави цикъл
   (define (acyclic? path)
       (not (memv (car path) (cdr path))))
  ;прави същото като extend, но маха цикличните пътища
   (define (extend-acyclic path)
       (filter acyclic? (extend path)))
  ;за всеки връх на това ниво прави списък от новото ниво
   (define (extend-level l)
       (apply append (map extend-acyclic l)))

  ;проверява дали path стига то v
  (define (target-path? path)
     (eqv? (car path) v))

  
(define (exists? p? l)
  ;; неконструктивно съществуване
  (not (null? (filter p? l))))

  (define (bfs-level l)
    (cond
        ;ако нивото е празно, няма път
          ((null? l) #f)
        ;ако в нивото сме стигнали до целевия връх: връщаме първия път
          ((exists? target-path? l)
              (reverse (car (filter target-path? l))))
        ;иначе разширяваме нивото до следващото ниво
          (else (bfs-level (extend-level l))))
    )
 (bfs-level (list (list u)))
  )

(define (bfs-all-paths u v g)
    (define (extend path)
      (map-children (car path) (lambda (child) (cons child path)) g))
    (define (not-acyclic? p)
       (not (memv (car p) (cdr p))))
    (define (extend-acyclic p)
      (filter not-acyclic? (extend p)))
    (define (extend-level l)
      (apply append (map extend-acyclic l)))

    (define (target-path? p)
       (eqv? (car p) v))

  (define (bfs-level l)
     (if
        ;ако нивото е празно - няма пътища
        (null? l) '()
        ;взимаме всички целеви пътища в текущото ниво + пътищата от следващото ниво
        (append (map reverse (filter target-path? l))
                (bfs-level (extend-level l)))))

  (bfs-level (list (list u)))
  )


(define (bfs-acyclic g)
   (define (extend path)
      (map-children (car path) (lambda (child) (cons child path)) g))
    (define (not-acyclic? p)
       (not (memv (car p) (cdr p))))
  (define (acyclic? p)
      (memv (car p) (cdr p)))
   
    (define (extend-level l)
      (apply append (map extend l)))

  (define (bfs-level l)
      (cond
         ((null? l) #f)
         ((any? acyclic? l) #t)
         (else (bfs-level (extend-level l)))))
  (any? (lambda (v) (bfs-level (list (list v)))) (vertices g)))

(define (bfs-all-cyclic-paths u v g)
    (define (extend path)
      (map-children (car path) (lambda (child) (cons child path)) g))
    (define (not-acyclic-target? p)
       (and (not (memv (car p) (cdr p)))
            (not (target-path? p))))
  (define (not-acyclic? p)
        (or (< (length p) 3)
            (not (memv (car p) (cdr p))))
            )
    (define (extend-acyclic-target p)
      (filter not-acyclic-target? (extend p)))
  
    (define (extend-level l)
      (apply append (map extend-acyclic-target l)))

    (define (target-path? p)
       (and (eqv? (car p) v)
             (>= 3 (length p))))

  (define (bfs-level l)
     (if
        ;ако нивото е празно - няма пътища
        (null? l) '()
        ;взимаме всички целеви пътища в текущото ниво + пътищата от следващото ниво
        (append (map reverse (filter target-path? l))
                (bfs-level (extend-level (filter not-acyclic? l))))))

  (bfs-level (list (list u)))
  )

(define (maxCycle g u)
    (bfs-all-cyclic-paths u u g))

(define gg (add-edge 4 2 (add-edge 3 4 (add-edge 3 1 (add-edge 2 3 (add-edge 1 2(make-graph '(1 2 3 4))))))))