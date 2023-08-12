pub const SUM_OF_SQUARES: &str = "
    ;; this is comment 1
    (define (square x)
        (* x x)
    )
    ;; this is comment 2
    (define (sum-of-squares x y)
        (+ (square x) (square y))
    )
    (sum-of-squares 3.14e-3 40)
";

pub const TOO_BIG_INT: &str = "
    (define (square x)
        (* x x)
    )
    (define (sum-of-squares x y)
        (+ (square x) (square y))
    )
    (sum-of-squares 3.14e-3 400000000000000000000000000000000000000000000000000000000000)
";

pub const PRINT_MSG: &str = "
    (define print_msg (msg src dst)
        (print \"Message from \" src \" to \" dst \" : \" msg)
    )

    (print_msg \"hello world\" \"me\" \"you\")
";

pub const PRINT_QUOTED: &str = "
    (print `(* 42 24))
";