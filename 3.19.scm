#lang scheme

(require racket/mpair)

(define (make-cycle ml)
  (define (last-pair ml)
    (if (null? (mcdr ml))
        ml
        (last-pair (mcdr ml))))
  (set-mcdr! (last-pair ml) ml)
  ml)

(define (mcddr x) (mcdr (mcdr x)))

(define (is-cycle? ml)
  (define (iter slow fast initial?)
    (if (and (not initial?) (eq? (mcar slow) (mcar fast)))
        #t
        (if (or (null? (mcdr fast)) (null? (mcddr fast)))
            #f
            (iter (mcdr slow) (mcddr fast) #f))))
  (if (null? ml)
      #f
      (iter ml ml #t)))

; Tests

(define l (mlist 'x 'y 'z))
(make-cycle l)

(define l2 (mlist 'x 'y))
(make-cycle l2)

(define l3 (mlist 'x))
(make-cycle l3)

(is-cycle? (mlist 1 2 3))
(is-cycle? l)
(is-cycle? l2)
(is-cycle? l3)