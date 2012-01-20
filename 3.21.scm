#lang racket

(require racket/mpair)

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (make-queue) (mcons '() '()))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (queue-front queue)
  (if (empty-queue? queue)
      (error "QUEUE-FRONT -- called on empty queue" queue)
      (mcar (front-ptr queue))))

(define (queue-insert! queue item)
  (let ((new-entry (mcons item '())))
    (if (empty-queue? queue)
        (begin (set-front-ptr! queue new-entry)
               (set-rear-ptr! queue new-entry))
        (begin (set-mcdr! (rear-ptr queue) new-entry)
               (set-rear-ptr! queue new-entry)))
    queue))

(define (queue-delete! queue)
  (if (empty-queue? queue)
      (error "QUEUE-DELETE! -- called on empty queue" queue)
      (set-front-ptr! queue (mcdr (front-ptr queue))))
  queue)

(define (queue-print queue)
  (define (iter ptr)
    (if (eq? ptr (rear-ptr queue))
        (display (mcar ptr))
        (begin (display (mcar ptr))
               (display " ")
               (iter (mcdr ptr)))))
  (display "#<queue>(")
  (if (empty-queue? queue)
      'pass
      (iter (front-ptr queue)))
  (display ")")
  (newline))

; Tests
(define q (make-queue))
(queue-print q)
(for-each (lambda (x) (queue-insert! q x)) '(a b c d e))
(queue-print q)
(empty-queue? q)
(queue-front q)
(queue-delete! q)
(queue-delete! q)
(queue-front q)
(queue-print q)
(queue-delete! q)
(queue-delete! q)
(queue-delete! q)
(empty-queue? q)
(queue-print q)