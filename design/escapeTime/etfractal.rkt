#lang racket

(require (only-in 2htdp/image color-list->bitmap))
(require racket/gui/base)
(require (for-syntax syntax/parse))

(provide
  generate-etfractal
  render
  simple-color
  simple-color-ratio)

;;Represents the state of the rendering of the escape time fractal
(define-struct world-state [image width height et-fractal max-iterations bounds] #:mutable)
;;A WorldState is a (make-world-state bitmap% Natural Natural ETFractal Natural (Tuple (Tuple Natural)))
;; and represent the state of the given escape time fractal
;; image - the state of the rendering of the fractal
;; width - The width of the window drawn to
;; height - The height of the window drawn to
;; et-fractal - The fractal information used for the state of the fractal
;; max-iterations - The max number of iterations that can be computed for a single point
;; bounds - The upper and lower bounds on the complex plane

(define-struct et-fractal [updater dimension])
;;An ETFractal represents an escape time fractal with some updater which computes the value for each
;;position and the dimension that the fractal will be represented in.
;;
;;NOTE: Currently the dimension field is ignored

;;Dimension is one of:
;; - 2D
;; - 3D

;;Finds the number of steps it takes to escape to infinity given some updater
;;steps-to-inf: (-> Complex Complex Natural) Natural Natural WorldState
(define (steps-to-inf et-fractal-update x y world)
  (letrec
      ([width (world-state-width world)]
       [height (world-state-height world)]
       [z-bounds (second (world-state-bounds world))]
       [upper-z (second z-bounds)]
       [lower-z (first z-bounds)]
       [complex-posn 
        (+ (+ (real-part lower-z) 
              (* (/ x width) 
                 (- (real-part upper-z) (real-part lower-z)))
              (* 0+1i (+ (imag-part lower-z) 
                         (* (/ y height) 
                            (- (imag-part upper-z) (imag-part lower-z)))))))]
       [find-steps (λ (z iteration)
                     (if (or (>= iteration (world-state-max-iterations world))
                             (> (magnitude z) 2))
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
        (make-color (min 255 (* 10 steps)) ;; 2/6
                    (min 255 (* 5 steps))  ;; 1/6
                    (min 255 (* 15 steps))))));3/6

;;Will create coloring rules based on the ratios the user supplies. The function returned will
;;return the color that is assosicated with the given number of steps to infinity 
;;simple-color-ratio: Decimal Decimal Decimal Natural (-> Natural Color)
(define (simple-color-ratio r-ratio g-ratio b-ratio max-iters)
  (if (> (+ r-ratio b-ratio g-ratio) 1)
      ;;TODO: MAKE THE BELOW ERROR A COMPILE TIME ERR
      (error "The given ratios of red green and blue values cannot be greater than 1!"
             r-ratio g-ratio b-ratio)
      (λ (steps)
        (let
            ([red (ceiling (* steps r-ratio 255))]
             [green (ceiling (* steps g-ratio 255))]
             [blue (ceiling (* steps b-ratio 255))])
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

;;Creates a color for each of the pixels in the fractal image
;;generate-pixels: (-> (-> Complex Complex Integer)) (-> Natural (-> Natural Color)) Natural
;;                 Natural WorldState
(define (generate-pixels etf-updater color-func width height world)
  (define bytes (make-bytes (* 4 width height)))
  (for*/list ([x (range width)]
              [y (range height)])
    (let*
        ([num-steps (steps-to-inf etf-updater x y world)]
         [color (color-func num-steps)]
         [starting-idx (+ (* x 4) (* y 4 width))]
         [red (send color red)]
         [green (send color green)]
         [blue (send color blue)])
      (bytes-set! bytes starting-idx 0)
      (bytes-set! bytes (+ starting-idx 1) red)
      (bytes-set! bytes (+ starting-idx 2) green)
      (bytes-set! bytes (+ starting-idx 3) blue)))
  bytes)

;;A convienence function to make a new et-fractal
;;generate-etfractal: (-> (-> Complex Complex Integer)) Dimension)
(define (generate-etfractal updater dimension)
  (make-et-fractal updater dimension))

;;Will render the current state
;;render-state: (-> Natural (-> Natural Color)) Image
(define (render-state color-func world)
  (let ([updater (et-fractal-updater (world-state-et-fractal world))]
        [max-iters (world-state-max-iterations world)])
    (draw-fractal updater
                  (color-func max-iters)
                  world)))

;;Creates a new frame from the given color function and world state
;; create-frame : (-> Natural (-> Natural Color)) WorldState
(define (create-frame color world)
  (let* ([width (world-state-width world)]
         [height (world-state-height world)]
         [title "Fractalang"]
         
         [display-frame (new frame%
                             [label title]
                             [width width]
                             [height height])]
         [paint-callback (lambda (canvas dc)
                           (send dc draw-bitmap (world-state-image world) 0 0)
                           (define img (render-state color world))
                           (send dc draw-bitmap img 0 0))]
         [display-canvas
          (new canvas%
               [parent display-frame]
               [paint-callback paint-callback])])
    (send display-canvas refresh-now)	
    (send display-frame show #t)))

;; (render mandelbrot mand-color max-iterations [(bounds -2 2) (bounds -2i 2i)] 500 500)
(define-syntax render
  (λ (stx)
    (syntax-parse stx
      [(render et color max-iter [(_ x-lower x-upper)
                                  (_ y-lower y-upper)] width height)
       #'(create-frame color (make-world-state
                              (make-object bitmap% width height)
                              width
                              height
                              et
                              max-iter
                              (list
                               (list x-lower x-upper)
                               (list y-lower y-upper))))])))












