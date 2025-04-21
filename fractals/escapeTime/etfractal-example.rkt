#lang racket
(require "etfractal.rkt")

;;EXAMPLES

;;The updater for a Mandelbrot fractal
(define mand-upd (λ (z c) (+ (* z z) c)))

(define mand-etf (generate-etfractal mand-upd))

(render mand-etf simple-color
        #:max-iterations 100
        #:escape-bounds 2
        #:horizontal-bounds (1+0.5i -1-0.5i)
        #:vertical-bounds (-1-0.5i 1+0.5i)
        #:window-width 600 #:window-height 600)

;;The updater for a Burning Ship fractal
(define burn-upd (λ (z c)
                   (+
                    (* (make-rectangular (abs (real-part z)) (abs (imag-part z))) 
                       (make-rectangular (abs (real-part z)) (abs (imag-part z)))) 
                    c)))

(define burn-etf (generate-etfractal burn-upd))

#;(render burn-etf simple-color
        #:max-iterations 100
        #:escape-bounds 2
        #:horizontal-bounds -1-0.5i 1
        #:vertical-bounds -1-0.5i 1
        #:window-width 600 #:window-height 600)

;;The updater for a Julia fractal
(define julia-upd (λ (k) (λ (z _) (+ (* z z) k))))
