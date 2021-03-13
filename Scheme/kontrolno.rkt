(define (foldr combiner nv l)
        (if (null? l) nv
            (combiner (car l) (foldr combiner nv (cdr l)))))

(define (foldl combiner nv l)
        (if (null? l) nv
            (foldl combiner (combiner (car l) nv) (cdr l))))

(define (accumulate op nv term a next b)
  (if (> a b) nv
      (op (term a) (accumulate op nv term (next a) next b))))

(define (filter p l)
      (if (null? l)
          l
          (if (p (car l))
              (cons (car l) (filter p (cdr l)))
              (filter p (cdr l)))))