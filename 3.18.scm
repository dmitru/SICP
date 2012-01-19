#lang scheme

(require racket/mpair)

(define (make-cycle ml)
  (define (last-pair ml)
    (if (null? (mcdr ml))
        ml
        (last-pair (mcdr ml))))
  (set-mcdr! (last-pair ml) ml)
  ml)

(define l (mlist 'x 'y 'z))
(make-cycle l)

(define (is-cycle? ml)
  (let ((seen '()))
    (define (helper ml)
      (if (null? ml)
          #f
          (if (member (mcar ml) seen)
              #t
              (begin 
                (set! seen (cons (mcar ml) seen))
                (helper (mcdr ml))))))
    (helper ml)))

; Tests

(is-cycle? (mlist 1 2 3))   ; #f
(is-cycle? l)               ; #t