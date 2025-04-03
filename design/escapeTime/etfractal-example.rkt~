#lang racket
(require "etfractal.rkt")

;;EXAMPLES

;;The updater for a Mandelbrot fractal
(define mand-upd (λ (z c) (+ (* z z) c)))

(define mand-etf (generate-etfractal mand-upd '2D))

(render mand-etf simple-color 100 [(bounds -2 2) (bounds -1.5+1.5i 1.5-1.5i)] 500 500)

;;The updater for a Burning Ship fractal
(define burn-upd (λ (z c)
                   (+
                    (* (make-rectangular (abs (real-part z)) (abs (imag-part z))) 
                       (make-rectangular (abs (real-part z)) (abs (imag-part z)))) 
                    c)))

;;The updater for a Julia fractal
(define julia-upd (λ (k) (λ (z _) (+ (* z z) k))))

