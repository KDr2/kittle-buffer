#lang racket

(require racket/gui/base)
(require racket/draw)
(require framework)

(require kittle-buffer/interpreter)

(provide wmain)

(define init-code "^0^1>+21^2>>+50^3>>>+22?@2^0.+29.+7..+3.-79.+43.-9.+4.-37.")

(define cur-arity 0)
(define (update-arity ins)
  (if (char=? #\@ (vector-ref ins 0))
      (set! cur-arity (vector-ref ins 1))
      (set! cur-arity 0)))

(define no-brush (new brush% [style 'transparent]))
(define orange-brush (new brush% [color "orange"]))
(define yellow-brush (new brush% [color "yellow"]))
(define green-brush (new brush% [color "green"]))
(define red-brush (new brush% [color "red"]))

(define (brush-for-data idx)
  (if (= cur-arity 0)
      no-brush
      (cond
       [(member idx (map pointer (stream->list (in-range cur-arity)))) yellow-brush] ; args
       [(= idx (pointer cur-arity)) orange-brush] ; function
       [(= idx (pointer (+ 1 cur-arity))) green-brush] ; return value
       [else no-brush])))

(define (brush-for-ptr idx)
  (if (= cur-arity 0)
      no-brush
      (cond
       [(< idx cur-arity) yellow-brush] ; args
       [(= idx cur-arity) orange-brush] ; function
       [(= idx (+ 1 cur-arity)) green-brush] ; return value
       [else no-brush])))

(define (draw-pointer dc x y id)
  (let ([ps (list (cons x y) (cons (- x 15) (+ y 30)) (cons (+ x 15) (+ y 30)))]
        [text (format "~a" id)])
    (send dc set-pen (if (= id (car POINTER-STACK)) "red" "black") 1 'solid)
    (send dc set-brush (if (= id (car POINTER-STACK)) red-brush no-brush))
    (send dc draw-polygon ps)
    (send dc draw-text text (- x 5) (+ y 12))
    (send dc set-pen "black" 1 'solid)))

(define (draw-data-buffer canvas dc)
  (send dc set-smoothing 'aligned)
  (send dc draw-text "Data Buffer" 0 0)
  (send dc set-brush no-brush)
  (send dc draw-rectangle 40 40 80 80)
  (send dc draw-line 40 80 119 80)
  (send dc draw-text "Index" 50 50)
  (send dc draw-text "Value" 50 90)
  (for ([i (in-range (vector-length BUFFER))]
        [v BUFFER])
    (send dc set-brush (brush-for-data i))
    (send dc draw-rectangle (* (+ i 3) 42) 40 40 80)
    (send dc draw-line (* (+ i 3) 42) 80 (+ 39 (* (+ i 3) 42)) 80)
    (send dc draw-text (format "~a" i) (+ 4 (* (+ i 3) 42)) 52)
    (send dc draw-text (format "~a" v) (+ 4 (* (+ i 3) 42)) 92))
  (send dc set-brush no-brush)
  (for ([(k v) POINTERS])
    (draw-pointer dc (+ 20 (* (+ v 3) 42)) 120 k)))

(define (draw-pointer-stack canvas dc)
  (send dc set-smoothing 'aligned)
  (send dc draw-text "Pointer Stack" 0 0)
  (send dc set-brush no-brush)
  (send dc draw-rectangle 40 30 80 80)
  (send dc draw-line 40 70 119 70)
  (send dc draw-text "Pointer" 50 40)
  (send dc draw-text "Position" 50 80)
  (for ([i (in-range (length POINTER-STACK))]
        [p POINTER-STACK])
    (send dc set-pen (if (= i 0) "red" "black") 1 'solid)
    (send dc set-brush (brush-for-ptr i))
    (send dc draw-rectangle (* (+ i 3) 42) 30 40 80)
    (send dc draw-line (* (+ i 3) 42) 70 (+ 39 (* (+ i 3) 42)) 70)
    (send dc draw-text (format "~a" p) (+ 12 (* (+ i 3) 42)) 42)
    (send dc draw-text (format "~a" (hash-ref POINTERS p)) (+ 12 (* (+ i 3) 42)) 82)
    (send dc set-pen "black" 1 'solid)))



(define style-delta-red (let ([delta (make-object style-delta%)])
                          (send delta set-delta-foreground "red")
                          delta))
(define style-delta-black (let ([delta (make-object style-delta%)])
                            (send delta set-delta-foreground "black")
                            delta))

(define kbf-win%
  (class object%

    (super-new)

    (define running #f)
    (define paused #f)
    (define instructions '())

    (define main-window (new frame%
                             [label "KBF - Kittle-Buffer v0.2"]
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
                          [callback (lambda (btn evt) (action-run btn evt))]))
    (define btn-step (new button%
                          [parent panel-buttons]
                          [label "Step"]
                          [stretchable-width #t]
                          [callback (lambda (button event)
                                      (when (not running)
                                        (reset)
                                        (set! instructions (parse (send tf-code get-value)))
                                        (set! running #t))
                                      (let ([vals (step instructions)])
                                        (when (> (vector-length (car vals)) 0)
                                          (update-arity (car vals))
                                          (set! instructions (cdr vals))
                                          (send (send tf-code get-editor)
                                                change-style style-delta-black 0 'end)
                                          (send (send tf-code get-editor)
                                                change-style style-delta-red
                                                (car (vector-ref (car vals) 3))
                                                (+ 1 (cdr (vector-ref (car vals) 3))))
                                          (refresh))
                                        (if (eq? instructions '())
                                            (begin
                                              (set! running #f)
                                              (enable-buttons #t btn-run btn-step)
                                              (enable-buttons #f btn-continue btn-stop btn-pause))
                                            (begin
                                              (enable-buttons #t btn-continue btn-step btn-stop)
                                              (enable-buttons #f btn-run btn-pause)))))]))
    (define  btn-pause (new button%
                            [parent panel-buttons]
                            [label "Pause"]
                            [enabled #f]
                            [stretchable-width #t]
                            [callback (lambda (button event)
                                        (set! paused #t))]))
    (define btn-continue (new button%
                              [parent panel-buttons]
                              [label "Continue"]
                              [enabled #f]
                              [stretchable-width #t]
                              [callback (lambda (button event)
                                          (set! paused #f)
                                          (action-run button event))]))
    (define btn-stop (new button%
                          [parent panel-buttons]
                          [label "Stop"]
                          [enabled #f]
                          [stretchable-width #t]
                          [callback (lambda (button event)
                                      (reset)
                                      (enable-buttons #t btn-run btn-step)
                                      (enable-buttons #f btn-continue btn-stop btn-pause))]))

    (define slider-delay (new slider%
                              (label "Delay")
                              (parent panel-buttons)
                              (min-value 0)
                              (max-value 100)
                              (init-value 50)))

    (define chb-pause-at-q (new check-box%
                              (label "Pause at ?")
                              (parent panel-buttons)
                              (value #f)))

    (define  panel-code (new panel:horizontal-dragable% ; horizontal-panel%
                             [parent main-window]
                             [stretchable-height #t]))
    (define tf-code (new text-field%
                         [parent panel-code]
                         [label "Code:"]
                         [style '(multiple vertical-label)]
                         ;; [init-value init-code]
                         [font (make-object font% 14 'default)]))
    (define tf-output (new text-field%
                           [parent panel-code]
                           [label "Output:"]
                           [style '(multiple vertical-label)]
                           [enabled #f]
                           [font (make-object font% 14 'default)]))

    (define/public (show) (send main-window show #t))

    (define/public (refresh)
      (send canvas-buffer refresh)
      (send canvas-buffer
            init-auto-scrollbars (* 42 (+ 4 (vector-length BUFFER))) #f 0 0)
      (send canvas-pointer-stack refresh)
      (send canvas-pointer-stack
            init-auto-scrollbars (* 42 (+ 4 (length POINTER-STACK))) #f 0 0))


    (define/public (enable-buttons flag . btns)
      (map (lambda (b) (send b enable flag)) btns))

    (define/public (reset)
      (reset-kbf)
      (set! cur-arity 0)
      (set! instructions '())
      (set! running #f)
      (set! paused #f)
      (send tf-output set-value "")
      (refresh)
      (set-char-inputer (lambda ()
                          (let ([val (get-text-from-user
                                      "KBF" "Please enter a character" main-window
                                      #:validate (lambda (x) (= 1 (string-length x))))])
                            (if (string? val)
                                (string-ref val 0)
                                (integer->char 0)))))
      (set-char-outputer (lambda (x)
                           (send tf-output set-value
                                 (string-append (send tf-output get-value)
                                                (string x)))))
      (set-error-handler (lambda (ex)
                           (message-box "Error" (format "~a" ex))
                           (raise ex))))

    (define/public (action-run button event)
      (enable-buttons #f btn-run btn-step btn-continue)
      (enable-buttons #t btn-stop btn-pause)
      (run-code)
      (if paused
          (begin ;; paused
            (enable-buttons #f btn-run btn-pause)
            (enable-buttons #t btn-step btn-continue btn-stop))
          (begin ;; stoped or finished
            (set! running #f)
            (enable-buttons #t btn-run btn-step)
            (enable-buttons #f btn-continue btn-stop btn-pause))))

    (define/public (set-code code)
      (send tf-code set-value init-code))

    (define/public (run-code)
      (when (not running)
        (reset)
        (set! instructions (parse (send tf-code get-value)))
        (set! running #t))
      (let loop ([vals (step instructions)])
        (set! instructions (cdr vals))
        (when (eq? instructions '())
          (set! running #f))
        (when (> (vector-length (car vals)) 0)
          (update-arity (car vals))
          (when (and
                 (send chb-pause-at-q get-value)
                 (char=? #\? (vector-ref (car vals) 0)))
            (set! paused #t))
          (send (send tf-code get-editor)
                change-style style-delta-black 0 'end)
          (send (send tf-code get-editor)
                change-style style-delta-red
                (car (vector-ref (car vals) 3))
                (+ 1 (cdr (vector-ref (car vals) 3))))
          (refresh)
          (sleep/yield (/ (send slider-delay get-value) 100.0)))
        (when (and running (not paused))
          (loop (step instructions)))))))

;; entrance

(define (wmain)
  (let* ([kbf-mw (new kbf-win%)])
    (send kbf-mw set-code init-code)
    (send kbf-mw show)))
