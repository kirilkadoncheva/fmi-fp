(load "/Documents/uni/2_курс/ФП/Scheme/check.rkt")

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

(define (take-stream n s)
    (if (or (= n 0)
            (empty-stream? s))
        empty-stream
        (cons-stream (head s)
                     (take-stream (- n 1) (tail s)))))

(define (cycle l)
    (if (null? l)
        empty-stream
        (cons-stream (car l)
                     (cycle (append (cdr l) (list (car l)))))))

(define (stream->list s)
    (if (empty-stream? s)
        '()
        (cons (head s)
              (stream->list (tail s)))))

(define (repeat value)
   (cons-stream value (repeat value)))

(define (iterate f x)
    (define (iter last)
         (cons-stream last (iter (f last))))
  (iter x))

(define (square x) (* x x))

(define (integers-from n)
   (cons-stream n (integers-from (+ n 1))))

(define (range-stream from to)
   (take-stream (+ to (- from) 1)
                (integers-from from)))

(define (map-stream f s)
    (if (empty-stream? s)
        empty-stream
        (cons-stream (f (head s))
                     (map-stream f (tail s)))))

(define (filter-stream p s)
   (if (empty-stream? s)
        empty-stream
        (if (p (head s))
            (cons-stream (head s) (filter-stream p (tail s)))
            (filter-stream p (tail s)))
        ))

(define (append-stream s1 s2)
     (if (empty-stream? s1)
         s2
         (cons-stream (head s1)
                      (append-stream (tail s1) s2))))

(define (concat-streams ss)
      (cond ((empty-stream? ss) empty-stream)
            ((empty-stream? (head ss)) (concat-streams (tail ss)))
            (else (cons-stream (head (head ss))
                               (append-stream (tail (head ss))
                                              (concat-streams (tail ss)))))))

(define (flatmap-stream f s)
  (concat-streams (map-stream f s)))

(define triples
    (flatmap-stream (lambda (c)
                       (flatmap-stream (lambda (b)
                                          (map-stream (lambda (a)
                                                         (list a b c)) (range-stream 1 b))) (range-stream 1 c)))
         (integers-from 1)))

(define pythagorean-triples
     (filter-stream
                 (lambda (triple) (= (+ (square (car triple))
                                        (square (cadr triple)))
                                     (square (caddr triple))))
                  triples))

(define (generateExponents k l)
  (define (iter x y)
          (cond ((< (expt x k) (expt y l)) (cons-stream (expt x k) (iter (+ 1 x) y)))
                ((> (expt x k) (expt y l)) (cons-stream (expt y l) (iter x (+ 1 y))))
                (else (cons-stream (expt y l) (iter (+ 1 x) (+ 1 y))))))
  (iter 1 1))