# Fractalang

## Purpose

Fractalang is a langauge allows users to easily represent and draw [fractals](https://en.wikipedia.org/wiki/Fractal). The langauge supports both iterative and escape-time fractals enabling users to explore math in a more approachable setting and to see their creations come to life. The interactive nature of the language was intentional so that users can explore the fractals they create.
## Example



Iterative Fractal
```scheme
;; The Koch Snowflake
(define koch (generate-ifractal [([F : (combine (draw 10 "black") (draw 10 "red"))]
                                  [+ : (turn 60)] [- : (turn -120)])
                                 [F-F-F]
                                 ([F -> F+F-F+F])]))

;; Draw your creation!
(render/interactive koch 600 600)
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

## Installation Guide

Change directory into the repository and run 


```
git clone (https://github.com/nikhil-khot/fractal-lang)[https://github.com/nikhil-khot/fractal-lang]
cd fractal-lang/
raco pkg install
```

Then import it into your project as 

```
(require Fractalang)
```
## Full Documentation

The full documentation for this langauge can be found in the scribble page or in [here](/fractals/design.md)

Developer docs can be found [here](/private/README.md)
