(define (square x)
    (* x x)
)
(define (sum-of-squares x y)
    (+ (square x) (square y))
)
(sum-of-squares 3.14e-3 400000000000000000000000000000000000000000000000000000000000)