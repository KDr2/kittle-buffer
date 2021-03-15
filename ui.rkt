#lang racket

(require racket/gui/base)
(require racket/draw)

(require kittle-buffer/kbf)

(define (draw-pointer dc x y id)
  (let ([ps (list (cons x y) (cons (- x 15) (+ y 30)) (cons (+ x 15) (+ y 30)))]
        [text (format "~a" id)])
    (send dc draw-polygon ps)
    (send dc draw-text text (- x 5) (+ y 12))))

(define (draw-data-buffer canvas dc)
  (send dc draw-text "Data Buffer" 0 0)
  (send dc draw-rectangle 40 40 80 80)
  (send dc draw-line 40 80 119 80)
  (send dc draw-text "Index" 50 50)
  (send dc draw-text "Value" 50 90)
  (for ([i (in-range (vector-length BUFFER))]
        [v BUFFER])
    (send dc draw-rectangle (* (+ i 3) 42) 40 40 80)
    (send dc draw-line (* (+ i 3) 42) 80 (+ 39 (* (+ i 3) 42)) 80)
    (send dc draw-text (format "~a" i) (+ 4 (* (+ i 3) 42)) 52)
    (send dc draw-text (format "~a" v) (+ 4 (* (+ i 3) 42)) 92))
  (for ([(k v) POINTERS])
    (draw-pointer dc (+ 20 (* (+ v 3) 42)) 120 k)))

(define (draw-pointer-stack canvas dc)
  (send dc draw-text "Pointer Stack" 0 0)
  (send dc draw-rectangle 40 40 80 80)
  (send dc draw-line 40 80 119 80)
  (send dc draw-text "Pointer" 50 50)
  (send dc draw-text "Position" 50 90)
  (for ([i (in-range (length POINTER-STACK))]
        [p POINTER-STACK])
    (send dc draw-rectangle (* (+ i 3) 42) 40 40 80)
    (send dc draw-line (* (+ i 3) 42) 80 (+ 39 (* (+ i 3) 42)) 80)
    (send dc draw-text (format "~a" p) (+ 12 (* (+ i 3) 42)) 52)
    (send dc draw-text (format "~a" (hash-ref POINTERS p)) (+ 12 (* (+ i 3) 42)) 92)))



(define style-delta-red (let ([delta (make-object style-delta%)])
                          (send delta set-delta-foreground "red")
                          delta))
(define style-delta-black (let ([delta (make-object style-delta%)])
                            (send delta set-delta-foreground "black")
                            delta))

(define kbf-win%
  (class object%

    (super-new)
    (set-char-outputer (lambda (x)
                         (send tf-output set-value
                               (string-append (send tf-output get-value)
                                              (string x)))))

    (define running #f)
    (define instructions '())

    (define main-window (new frame%
                             [label "KBF - Kittle-Buffer"]
                             [width 800]
                             [height 600]))
    (define canvas-buffer (new canvas%
                               [parent main-window]
                               [style '(border hscroll)]
                               [stretchable-height #f]
                               [min-height 180]
                               [paint-callback draw-data-buffer]))
    (define canvas-pointer-stack (new canvas%
                                      [parent main-window]
                                      [style '(border hscroll)]
                                      [stretchable-height #f]
                                      [min-height 140]
                                      [paint-callback draw-pointer-stack]))
    (define  panel-buttons (new horizontal-panel%
                                [parent main-window]
                                [stretchable-height #f]))
    (define  btn-run (new button%
                          [parent panel-buttons]
                          [label "Run"]
                          [stretchable-width #t]
                          [callback (lambda (button event)
                                      (enabled-buttons #f btn-step btn-continue btn-stop)
                                      (send tf-output set-value "")
                                      (run (send tf-code get-value))
                                      (refresh)
                                      (enabled-buttons #t btn-step))]))
    (define btn-step (new button%
                          [parent panel-buttons]
                          [label "Step"]
                          [stretchable-width #t]
                          [callback (lambda (button event)
                                      (when (not running)
                                        (set! instructions (parse (send tf-code get-value)))
                                        (set! running #t))
                                      (let ([vals (step instructions)])
                                        (set! instructions (cdr vals))
                                        (send (send tf-code get-editor)
                                              change-style style-delta-black 0 'end)
                                        (send (send tf-code get-editor)
                                              change-style style-delta-red
                                              (car (vector-ref (car vals) 3))
                                              (+ 1 (cdr (vector-ref (car vals) 3))))
                                        (refresh)))]))
    (define btn-continue (new button%
                              [parent panel-buttons]
                              [label "Continue"]
                              [enabled #f]
                              [stretchable-width #t]
                              [callback (lambda (button event)
                                          (send tf-code set-value "Continue click"))]))
    (define btn-stop (new button%
                          [parent panel-buttons]
                          [label "Stop"]
                          [enabled #f]
                          [stretchable-width #t]
                          [callback (lambda (button event)
                                      (send canvas-pointer-stack refresh))]))
    (define  panel-code (new horizontal-panel%
                             [parent main-window]
                             [stretchable-height #t]))
    (define tf-code (new text-field%
                         [parent panel-code]
                         [label "Code:"]
                         [style '(multiple vertical-label)]
                         [init-value "^0^1>+21^2>>+50^3>>>+50@2^0."]))
    (define tf-output (new text-field%
                           [parent panel-code]
                           [label "Output:"]
                           [style '(multiple vertical-label)]
                           [enabled #f]))

    (define/public (show) (send main-window show #t))

    (define/public (refresh)
      (send canvas-buffer refresh)
      (send canvas-buffer
            init-auto-scrollbars (* 42 (+ 4 (vector-length BUFFER))) #f 0 0)
      (send canvas-pointer-stack refresh)
      (send canvas-pointer-stack
            init-auto-scrollbars (* 42 (+ 4 (length POINTER-STACK))) #f 0 0))

    (define/public (enabled-buttons flag . btns)
      (map (lambda (b) (send b enable flag)) btns))
    ))

; entrance
(let* ([kbf-mw (new kbf-win%)])
  (send kbf-mw show))
