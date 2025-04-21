#lang scribble/manual

@(require scribble/eval
          (for-label racket
                     racket/gui
                     pict))

@title{Fractalang}

@section{Overview}

This library provides tools for generating and rendering both escape-time fractals and iterative fractals in Racket. Through its simple syntax and low barrier to entry,
it acts as an approachable way by which computer science students can learn more about math.

@section{Escape-Time Fractals}

Escape-time fractals are created by iterating a function on complex numbers and determining whether the result escapes to infinity.

@defproc[(generate-etfractal [func (-> Complex Complex Complex)])
         ETFractal]{
 Creates an escape-time fractal based on a function of two complex
 values. The arguments are typically named @racket[z] and @racket[c] in the math, where @racket[z] is the value of the last iteration of the
 function and @racket[c] is the point being tested.
}

; point-in-set?: (-> ETFractal Complex Natural Natural Boolean)
; Will determine if the given complex point is in the set of points of
; the escape-time fractal.

; steps-to-escape: (-> ETFractal Complex Natural Natural (Maybe Natural))
; Will determine the number of steps to escape for some point on the
; complex plane. Errors if the given point does not escape.

@defproc[(point-in-set? [etf ETFractal] [point Complex] [max-iter Natural] [escape-bound Natural])
         Boolean]{
Determines if the given complex point is in the set of points of the escape-time fractal given the
bound which defines the magnitude which is consider to be infinity.
}

@defproc[(steps-to-escape [etf ETFractal] [point Complex] [max-iter Natural] [escape-bound Natural])
         (Maybe Natural)]{
Determines the number of steps to escape for some point on the complex plane. Will return
false if the point does not escape in the maximum number of steps given.
}

@defproc[(render [etf ETFractal]
                [#:color-func color-function (-> Natural Color)]
                [#:escape-bounds escape-bounds Natural]
                [#:horizontal-bounds h-bounds Bounds]
                [#:vertical-bounds v-bounds Bounds]
                [#:window-height height PosInt]
                [#:window-width width PosInt])
         Frame%]{
 Renders an escape time fractal, using the provided means of
 coloring, within the given complex bounds and window size. Returns the 
 frame object of the window.
}

@deftech{Bounds} is defined as:
@racketblock[
([min-bound Complex] [max-bound Complex])
]

If the max-bound is larger than the min-bound then the image will be reflected on the axis
in which it is defined for. This allows users to rotate the fractal to some arbitrary rotation
as well as focus on particular regions of the fractal.

@section{Iterative Fractals}

Iterative fractals are created by repeatedly applying transformations to a geometric pattern.

@defproc[(generate-ifractal [l-system L-System])
         IFractal]{
 Creates an iterative fractal based on a provided L-System.
}

@deftech{L-System} is defined as:
@racketblock[
((<Binding> ...+) [<State>] (<Transformation> ...))
]

An L-system is used to represent iterative fractals. The bindings
define all possible commands for this L-System. These bindings are
then used to define the initial state of the system, and the
transformations to apply at each iteration. If a binding has no defined
transformation, the default transformation simply preserves it
within the State.

@deftech{Binding} is defined as:
@racketblock[[<id>: <command>]]

A @racket[<command>] is one of or a composition of commands from a defined list of actions.
Currently including: moving, drawing in color, rotating, doing nothing,
saving current position and rotation, returning to saved position and rotation.

@deftech{State} is defined as:
@racketblock[
<id>
]
or
@racketblock[
<id><State>
]

The state is represented by a contiguous set of identifiers that have
no space between them. These identifiers are associated with some
transformation of the graphical representation of the fractal. If
there is an identifier that does not exist, then an error
will be raised.

@deftech{Transformation} is defined as:
@racketblock[
[<id> <State>]
]

A transformation is some identifier to some new state. These
transformations update the state of the fractal and the state of the
fractal determines its graphical representation. All transformations
must be represented in terms of other bindings @racket[<id>]s.

@defproc[(iterate [ifr IFractal] [iterations Natural])
         IFractal]{
 Applies transformations on the current state of the fractal
 to create a new state for the specified number of iterations.
}

@defproc[(render [ifr IFractal] 
                [iterations Natural]
                [width PosInt]
                [height PosInt])
         Pict]{
 Renders the provided iterative fractal with the provided number of iterations applied
 and scaled to fit the provided window bounds.
}

@section{Examples}

@subsection{Creating a Mandelbrot Set}

@racketblock[
(define mandelbrot 
  (generate-etfractal (λ (z c) (+ (* z z) c))))

(render mandelbrot
        #:color-func some-color-function
        #:escape-bounds 100
        #:window-width 800
        #:window-height 600)
]

@subsection{Creating a Koch Snowflake}

@racketblock[
(define koch-system
  (([F: (draw 1 "blue")]
     [+: (rotate 60)]
     [-: (rotate -60)])
    [F]
    ([F-> F+F--F+F])))

(define koch
  (generate-ifractal koch-system))

(define iterated-koch
  (iterate koch 4))

(render iterated-koch 4 800 600)
]