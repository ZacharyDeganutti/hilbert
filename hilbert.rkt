#lang racket

(require racket/gui/base)

;; Draws a curve and outputs it to a file called hilbert.png
(define (draw-curve iterations)
  (define scale 10)
  (define size (* scale (expt 2 (+ 1 iterations))))
  (define target (make-bitmap size size))
  (define dc (new bitmap-dc% [bitmap target]))
  (define (scale-curve curve scale)
    (map (lambda (pair) (map (lambda (val) (* val scale)) pair)) curve))
  (define curve (scale-curve (indexize (flatten (hilbert-curve iterations))) scale))
  (define curve-path
    (let ([p (new dc-path%)])
      (send p move-to 0 0)
      (map (lambda (pt) (send p line-to (first pt) (second pt))) curve)
      p))
  (send dc set-pen "Black" 5 'solid)
  (send dc set-brush "White" 'transparent)
  (send dc draw-path curve-path)
  (send target save-file "hilbert.png" 'png))

;; Defines a symbolic representation of a hilbert curve of some iteration count
(define (hilbert-curve iterations)
  ;; Rotates an entire curve right
  (define (rotate-right curve)
    (if [symbol? curve]
        (cond
          [(eq? curve 'up) 'left]
          [(eq? curve 'left) 'up]
          [(eq? curve 'down) 'right]
          [(eq? curve 'right) 'down])
        (map rotate-right curve)))

  ;; Rotates an entire curve left
  (define (rotate-left curve)
    (if [symbol? curve]
        (cond
          [(eq? curve 'up) 'right]
          [(eq? curve 'left) 'down]
          [(eq? curve 'down) 'left]
          [(eq? curve 'right) 'up])
        (map rotate-left curve)))

  ;; Generates the curve
  (define (make-curve iterations)
    (define next (- iterations 1))
    (if [eq? iterations 0]
        (list 'up 'right 'down)
        (list
         (rotate-left (make-curve next))
         'up
         (make-curve next)
         'right
         (make-curve next)
         'down
         (rotate-right (make-curve next)))))

  (make-curve iterations))

;; Takes a symbolic curve and returns a curve defined in terms
;; of discrete pairs of integers representing positions
(define (indexize curve)
  (match-define-values (_ a)
                       (for/fold ([acc-index '(0 0)]
                                  [result '((0 0))])
                                 ([i curve])
                         (let ([curr-index (cond
                                             [(eq? i 'up) (map + acc-index '(0 1))]
                                             [(eq? i 'down) (map - acc-index '(0 1))]
                                             [(eq? i 'right) (map + acc-index '(1 0))]
                                             [(eq? i 'left) (map - acc-index '(1 0))])])
                           (values curr-index (cons curr-index result)))))
  (reverse a))

