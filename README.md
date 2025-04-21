# fractal-lang

## Purpose

Fractalang is a langauge allows users to easily represent and draw fractals. The langauge supports both iterative(LINK) and escape time(LINK) fractals enabling users to explore math in a more approachable setting and to see their creations come to life. The interactive nature of the language was intentional so that users can explore the fractals they create.

    Tell potential users why they might be interested in your DSL, i.e. a purpose statement for the DSL and description or links to relevant background information necessary to understand the domain.

## Example



Iterative Fractal
```scheme
;; The Sierpinski Approximation
(define sierpinski-fractal (generate-ifractal 
            ;;Definitions
            [([A : (draw 1 "black")]
              [B : (draw 1 "black")]
              [+ : (turn 60)]
              [- : (turn -60)])
            ;; Initial State
            [B-A-B]
            ;; Transformations       
            ([A -> B-A-B]
              [B -> A+B+A])]))

;; Draw your creation!
(render/interactive sierpinski-fractal 600 600)
```

Escape Time Fractal
```scheme

;; The updater for the Mandelbrot fractal
(define mand-upd (λ (z c) (+ (* z z) c)))

;; The definition of the Mandelbrot fractal
(define mand-etf (generate-etfractal mand-upd))

;; Render the fractal!
(render mand-etf simple-color
        #:max-iterations 100
        #:escape-bounds 2
        #:horizontal-bounds (1+0.5i -1-0.5i)
        #:vertical-bounds (-1-0.5i 1+0.5i)
        #:window-width 400 #:window-height 400)
```

See more examples in the respective folders under the "fractals" directory.

    Whet their appetite with a small but compelling example. Point out how the benefits of your DSL show up in the example.

    Provide instructions for installing the DSL and accessing the full documentation
