#lang racket

(require (only-in 2htdp/image color-list->bitmap))
(require racket/gui/base)
(require (for-syntax syntax/parse))
(require syntax-spec-v3)

(provide
 generate-etfractal
 render
 simple-color
 simple-color-ratio)

;;Represents the state of the rendering of the escape time fractal
(define-struct world-state [image width height et-fractal max-iterations escape-bounds bounds pixels-computed total-pixels] #:mutable)
;;A WorldState is a (make-world-state bitmap% Natural Natural ETFractal Natural (Tuple (Tuple Natural) (Tuple Natural)) Natural Natural)
;; and represent the state of the given escape time fractal
;; image - the state of the rendering of the fractal
;; width - The width of the window drawn to
;; height - The height of the window drawn to
;; et-fractal - The fractal information used for the state of the fractal
;; max-iterations - The max number of iterations that can be computed for a single point
;; bounds - The upper and lower bounds on the complex plane
;; pixels-computed - The number of pixels that have been computed so far
;; total-pixels - The total number of pixels to compute

(define-struct et-fractal [updater])
;;An ETFractal represents an escape time fractal with some updater which computes the value for each
;;position

;;Finds the number of steps it takes to escape to infinity given some updater
;;steps-to-inf: (-> Complex Complex Natural) Natural Natural WorldState
(define (steps-to-inf et-fractal-update x y world)
  (letrec
      ([width (world-state-width world)]
       [height (world-state-height world)]
       [h-bounds (first (world-state-bounds world))]
       [v-bounds (second (world-state-bounds world))]
       [x-lower (first h-bounds)]
       [x-upper (second h-bounds)]
       [y-lower (first v-bounds)]
       [y-upper (second v-bounds)]
       [complex-posn 
        (+ (* (/ x width) 
              (- x-upper x-lower))
           x-lower
           (* 0+1i (+ y-lower
                      (* (/ y height) 
                         (- y-upper y-lower)))))]
       [find-steps (λ (z iteration)
                     (if (or (>= iteration (world-state-max-iterations world))
                             (> (magnitude z) (world-state-escape-bounds world)))
                         iteration
                         (find-steps (et-fractal-update z complex-posn) (add1 iteration))))])
    (find-steps 0+0i 0)))


;;Maps iteration count to color. The function returned will
;;return the color that is assosicated with the given number of steps to infinity 
;;simple-color: Natural (-> Natural Color)
(define (simple-color max-iters)
  (λ (steps)
    (if (= steps max-iters)
        (make-color 0 0 0)
        (make-color (min 255 (* 10 steps))
                    (min 255 (* 5 steps))
                    (min 255 (* 15 steps))))))

;;Will create coloring rules based on the ratios the user supplies. The function returned will
;;return the color that is assosicated with the given number of steps to infinity. All of the
;;ratios of each of the colors is relative to the other color values
;;simple-color-ratio: Natural Natural Natural Natural (-> Natural Color)
(define (simple-color-ratio r-ratio g-ratio b-ratio max-iters)
  (let*
      ([ratio-sum (+ r-ratio g-ratio b-ratio)]
       [red-scalar (/ r-ratio ratio-sum)]
       [green-scalar (/ g-ratio ratio-sum)]
       [blue-scalar (/ b-ratio ratio-sum)])
    (λ (steps)
      (let
          ([red (ceiling (* steps red-scalar 255))]
           [green (ceiling (* steps green-scalar 255))]
           [blue (ceiling (* steps blue-scalar 255))])
        (if (= steps max-iters)
            (make-color 0 0 0)
            (make-color (min 255 (ceiling red))
                        (min 255 (ceiling green))
                        (min 255 (ceiling blue))))))))

;; Generates the image of the given fractal of the given state
;;draw-fractal: (-> (-> Complex Complex Integer)) (-> Natural (-> Natural Color)) WorldState
(define (draw-fractal etf-updater color-func world)
  (letrec
      ([width (world-state-width world)]
       [height (world-state-width world)]
       [bitmap (make-object bitmap% width height)]
       [bytes (generate-pixels etf-updater color-func width height world)])
    (send bitmap set-argb-pixels 0 0 width height bytes)
    (set-world-state-image! world bitmap)
    bitmap))

;; Creates a loading screen bitmap with progress information
;;create-loading-screen: Natural Natural WorldState -> bitmap%
(define (create-loading-screen width height world)
  (define bitmap (make-object bitmap% width height))
  (define dc (make-object bitmap-dc% bitmap))
  
  (send dc set-pen "black" 1 'solid)
  (send dc set-brush "black" 'solid)
  (send dc draw-rectangle 0 0 width height)
  
  (draw-loading dc width height world)
  bitmap)

;;Draws the loading screen to satisfy low attention spanned users
;;draw-loading: bitmap-dc% Natural Natural WorldState -> Void
(define (draw-loading dc width height state)
  (let* ([pixels-computed (world-state-pixels-computed state)]
         [total-pixels (world-state-total-pixels state)]
         [percentage (if (= total-pixels 0)
                         0
                         (floor (* 100 (/ pixels-computed total-pixels))))]
         [loading-text (format "Computing fractal... ~a/~a pixels (~a%)" 
                               pixels-computed 
                               total-pixels
                               percentage)])
    (send dc set-font (make-object font% 20 'default 'normal 'bold))
    
    (define-values (text-width text-height _ __) 
      (send dc get-text-extent loading-text))
    
    (send dc set-text-foreground "white")
    (send dc draw-text loading-text 
          (/ (- width text-width) 2)
          (/ (- height text-height) 2))
    
    (draw-progress-bar dc width height text-width text-height state)))

;;Draws the progress bar based on the number of pixels to be computed and the computed so far
;;draw-progress-bar: bitmap-dc% Natural Natural Natural Natural WorldState -> Void
(define (draw-progress-bar dc width height text-width text-height state)
  (let* ([pixels-computed (world-state-pixels-computed state)]
         [total-pixels (world-state-total-pixels state)]
         [progress-width (max 10 (- width 100))]
         [progress-height 20]
         [progress-x (/ (- width progress-width) 2)]
         [center-y (/ height 2)]
         [progress-y (+ center-y (/ text-height 2) 20)])
    
    (send dc set-pen "white" 2 'solid)
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle progress-x progress-y progress-width progress-height)
    
    (send dc set-pen "blue" 1 'solid)
    (send dc set-brush "blue" 'solid)
    (define filled-width (inexact->exact
                          (floor (* (/ pixels-computed total-pixels) progress-width))))
    (when (> filled-width 0)
      (send dc draw-rectangle
            (+ progress-x 1)
            (+ progress-y 1)
            (max 1 (- filled-width 2))
            (max 1 (- progress-height 2))))))

;;Creates a color for each of the pixels in the fractal image
;;generate-pixels: (-> (-> Complex Complex Integer)) (-> Natural (-> Natural Color)) Natural
;;                 Natural WorldState
(define (generate-pixels etf-updater color-func width height world)
  (define bytes (make-bytes (* 4 width height)))
  
  ;;reset pixels-computed counter
  (set-world-state-pixels-computed! world 0)
  
  (for*/list ([x (range width)]
              [y (range height)])
    (let*
        ([num-steps (steps-to-inf etf-updater x y world)]
         [color (color-func num-steps)]
         [starting-idx (+ (* x 4) (* y 4 width))]
         [red (send color red)]
         [green (send color green)]
         [blue (send color blue)])
      (set-world-state-pixels-computed! world (add1 (world-state-pixels-computed world)))
      
      ;;pixel color in the bytes array
      (bytes-set! bytes starting-idx 0)
      (bytes-set! bytes (+ starting-idx 1) red)
      (bytes-set! bytes (+ starting-idx 2) green)
      (bytes-set! bytes (+ starting-idx 3) blue)))
  bytes)

;;A convienence function to make a new et-fractal
;;generate-etfractal: (-> (-> Complex Complex Integer)) ETFractal)
(define (generate-etfractal updater)
  (make-et-fractal updater))

;;Will render the current state
;;render-state: (-> Natural (-> Natural Color)) Image
(define (render-state color-func world)
  (let ([updater (et-fractal-updater (world-state-et-fractal world))]
        [max-iters (world-state-max-iterations world)])
    (draw-fractal updater
                  (color-func max-iters)
                  world)))

;;Creates a new frame from the given color function and world state
;; create-frame : (-> Natural (-> Natural Color)) WorldState)
(define (create-frame color world)
  (let* ([width (world-state-width world)]
         [height (world-state-height world)]
         [title "Fractalang"]
         
         [display-frame (new frame%
                             [label title]
                             [width width]
                             [height height])]
         [is-calculating? #t]
         [loading-bitmap (create-loading-screen width height world)]
         
         [paint-callback (lambda (canvas dc)
                           (if is-calculating?
                               ;; Show loading screen while calculating
                               (begin
                                 (set! loading-bitmap (create-loading-screen width height world))
                                 (send dc draw-bitmap loading-bitmap 0 0))
                               (send dc draw-bitmap (world-state-image world) 0 0)))]
         
         [display-canvas
          (new canvas%
               [parent display-frame]
               [paint-callback paint-callback])])
    
    (send display-frame show #t)
    (send display-canvas refresh-now)
    
    (define refresh-timer
      (new timer%
           [notify-callback
            (lambda ()
              (when is-calculating?
                (send display-canvas refresh-now)))]
           [interval 100]))
    
    ;;separate thread to compute the fractal
    (thread
     (lambda ()
       (define img (render-state color world))
       
       (set! is-calculating? #f)
       (send refresh-timer stop)
       (send display-canvas refresh-now)))
    display-frame))


(define-syntax-rule (render etf color-func
                           #:max-iterations max-iter
                           #:escape-bounds escape-bounds
                           #:horizontal-bounds (x-lower x-upper)
                           #:vertical-bounds (y-lower y-upper)
                           #:window-width width
                           #:window-height height)
  (begin
    (for-each (λ (val name)
                (unless (complex? val)
                  (error 'render "~a must be a complex number, got: ~v" name val)))
              (list x-lower x-upper y-lower y-upper)
              '("x-lower" "x-upper" "y-lower" "y-upper"))
    (create-frame color-func (make-world-state
                              (make-object bitmap% width height)
                              width height etf max-iter escape-bounds
                              (list (list x-lower x-upper)
                                    (list y-lower y-upper))
                              0
                              (* width height)))))


