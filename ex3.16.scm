#lang scheme
(define (count-pairs x)
  (let ((memory '()))
    (define (iter x)
      (if (or (not (pair? x)) (memq x memory)) ; if it is not a pair or inside the memory we don't count
          0
          (begin (set! memory (cons x memory)) ;store the pair if it is not in memeory
          (+ (iter (car x))
             (iter (cdr x))
             1)))
      )
    (iter x)
    ))



;set-cdr may have been removed in the latest version of scheme.
(define zi '(a b c))
(set-cdr! (cddr zi) zi)
(count-pairs zi)