(define (foldr f x xs)
  (if (null? xs)
    x
    (f (car xs) (foldr f x (cdr xs)))))

(define (foldl f x xs)
  (if (null? xs)
    x
    (foldl f (f x (car xs)) (cdr xs))))

(define (map f xs)
  (foldr (lambda (x ys) (cons (f x) ys)) '() xs))

(define (filter f xs)
  (foldr (lambda (x xs) (if (f x) (cons x xs) xs)) '() xs))

(define (flip f)
  (lambda (a b) (f b a)))

(define (length xs)
  (foldl (lambda (x y) (+ 1 x)) 0 xs))

(define (reverse xs)
  (foldl (flip cons) '() xs))

(define (id x) x)
