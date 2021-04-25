#lang racket

(require racket/draw)

(define width 1024)
(define margin 64)
(define target (make-bitmap width width))
(define dc (new bitmap-dc% [bitmap target]))
(send dc set-smoothing 'aligned)

(define circle (new region%))
(let ([p (new dc-path%)]
      [r (- width (* 2 margin))])
  (send p ellipse margin margin r r)
  (send circle set-path p))
(send dc set-clipping-region circle)

(send dc set-pen "white" 1 'transparent)

(send dc set-brush "white" 'solid)
(send dc draw-rectangle 0 0 width width)

(let* ([k-width (/ width 8)]
       [left-rec-width (* k-width 3)]
       [k-slop-w (* k-width (sqrt 2))])
  (send dc set-brush "red" 'solid)
  (send dc draw-rectangle 0 0 left-rec-width width)
  (send dc draw-polygon
        (list (cons (+ (/ width 2) k-slop-w) (/ width 2))
              (cons width k-slop-w)
              (cons width (- width k-slop-w))))
  (send dc set-brush "blue" 'solid)
  (send dc draw-polygon
        (list (cons (/ width 2) 0)
              (cons width 0)
              (cons (/ width 2) (/ width 2))))
  (send dc draw-polygon
        (list (cons (/ width 2) width)
              (cons width width)
              (cons (/ width 2) (/ width 2)))))


(send target save-file "kbf-icon-1024.png" 'png)
