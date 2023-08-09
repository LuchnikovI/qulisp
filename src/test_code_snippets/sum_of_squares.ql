;; this is comment 1
(define (square x)
    (* x x)
)
;; this is comment 2
(define (sum-of-squares x y)
    (+ (square x) (square y))
)
(sum-of-squares 3.14e-3 40)