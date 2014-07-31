(define (foldr f x xs)
  (if (null? xs)
    x
    (f (car xs) (foldr f x (cdr xs)))))

(define (map f xs)
  (foldr (lambda (x ys) (cons (f x) ys)) '() xs))
