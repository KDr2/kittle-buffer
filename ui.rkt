#lang racket

(require racket/gui/base)
(require racket/draw)

(define (draw-data-buffer canvas dc)
  (send dc set-scale 3 3)
  (send dc set-text-foreground "blue")
  (send dc draw-text "Data Buffer" 0 0))

(define (draw-pointer-stack canvas dc)
  (send dc set-scale (+ 1(random 3)) 3)
  (send dc set-text-foreground "blue")
  (send dc draw-text "Pointer Stack" 0 0))


(let* ([main-window (new frame%
                          [label "KBF - Kittle-Buffer"]
                          [width 800]
                          [height 600])]
       [canvas-buffer (new canvas%
                           [parent main-window]
                           [style '(border)]
                           [stretchable-height #f]
                           [min-height 100]
                           [paint-callback draw-data-buffer])]
       [canvas-pointer-stack (new canvas%
                                  [parent main-window]
                                  [style '(border)]
                                  [stretchable-height #f]
                                  [min-height 100]
                                  [paint-callback draw-pointer-stack])]
       [panel-buttons (new horizontal-panel%
                           [parent main-window]
                           [stretchable-height #f])]
       [panel-code (new horizontal-panel% [parent main-window]
                        [stretchable-height #t])]
       [tf-code (new text-field%
                     [parent panel-code]
                     [label "Code:"]
                     [style '(multiple vertical-label)])]
       [tf-output (new text-field%
                       [parent panel-code]
                       [label "Output:"]
                       [style '(multiple vertical-label)]
                       [enabled #f])]
       [btn-run (new button%
                     [parent panel-buttons]
                     [label "Run"]
                     [stretchable-width #t]
                     [callback (lambda (button event)
                                 (send tf-code set-value "Run click"))])]
       [btn-step (new button%
                      [parent panel-buttons]
                      [label "Step"]
                      [stretchable-width #t]
                      [callback (lambda (button event)
                                  (send tf-code set-value "Step click"))])]
       [btn-continue (new button%
                          [parent panel-buttons]
                          [label "Continue"]
                          [stretchable-width #t]
                          [callback (lambda (button event)
                                      (send tf-code set-value "Continue click"))])]
       [btn-stop (new button%
                      [parent panel-buttons]
                      [label "Stop"]
                      [stretchable-width #t]
                      [callback (lambda (button event)
                                  (send canvas-pointer-stack refresh))])])
  (send main-window show #t))




