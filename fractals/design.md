## Purpose
The purpose of this language is to easily generate graphical representations of fractals with low mathematical overhead. Through this, it allows users to be able to understand the behaviour of different types of fractals in a simple yet familiar form factor. Users will be able to define fractals in simple syntax that is similar to the commonly used notation for escape-time and iterative fractals. They will also have control over the coloring method of the fractal, and once rendered will be able to move around, zoom in, and alter the degree of precision/level of iteration that the fractal is currently being rendered at.
## Concepts
There are two kinds of fractals that we will be representing within our language - escape-time fractals and iterative fractals. Both kinds of fractals have static properties that can be measured, such as the Hausdorff dimension, a specific measure of fractal dimension. Other measurements include the packing dimension, information dimension, and correlation dimension. Many of these measures are equivalent for most fractals, so which ones are to be included will be finalized by the first milestone. \
Escape-time fractals are those where a mathematical function is repeatedly applied to every point within a certain bounds, to test whether the value of the function “escapes” the bounds. Points for which the repeated application of the function does not escape are included within the fractal. Escape-time fractals are usually generated on the complex plane, allowing for simpler equations using complex numbers instead of points. \
Iterative fractals are any fractals that are created by repeatedly iterating a pattern. While escape-time fractals fall under this definition, most iterative patterns are better represented by a set of states and transformations, leading to a different grammar within our language that utilizes L-Systems. L-Systems are a type of grammar that allows us to define an alphabet of bindings to commands, and transformations to apply to each variable in the state at each iteration.
## Language Grammar
```lisp
; generate-etfractal :
; (-> (-> Complex Complex Complex) ETFractal)
; Creates an escape-time fractal based on a function of two complex
; values (z and c, where z is the value of the last iteration of the
; function and c is the point being tested)

; bounds: (Complex Complex)
; The bounds that the fractal will be generated upon. Used to limit the
; region for which escape-time fractals are calculated and rendered upon.

; point-in-set?: (-> ETFractal Complex Natural Natural Boolean)
; Will determine if the given complex point is in the set of points of
; the escape-time fractal.

; steps-to-escape: (-> ETFractal Complex Natural Natural (Maybe Natural))
; Will determine the number of steps to escape for some point on the
; complex plane. Errors if the given point does not escape.

; <Color-ETF> := #:color-func (-> Natural (-> Natural Color))
;              | #:color-func (-> Natural Natural Natural Natural (-> Natural Color))

; render : (-> ETFractal 
               <Color-ETF>
               #:escape-bounds Natural
               #:horizontal-bounds [Bounds]
               #:vertical-bounds [Bounds]
               #:window-height PosInt
               #:window-width PosInt)

; Will render an escape time fractal, using the provided means of
; coloring (either a function that determines the color of each point,
; or a list of colors to use as a gradient depending on the speed of
; escape), within the given bounds and window size. Returns the final
; bounds being used to view the fractal.

; generate-ifractal : (-> <L-System> IFractal)
; Creates an iterative fractal based on a provided L-System,

; <L-System> := ((<Binding> ...+) [<State>] (<Transformation> ...))
; An L-system is used to represent iterative fractals. The bindings
; define all possible commands for this L-System. These bindings are
; then used to define the initial state of the system, and the
; transformations to apply at each iteration. If a binding has no defined
; transformation, the default transformation simply preserves it
; within the State.

; <Binding> := [<id>: <command>]
; A <command> is one of or a composition of commands from a defined list of actions.
; Currently including: moving, drawing in color, rotating, doing nothing,
; saving current position and rotation, returning to saved position and rotation

; <State> := <id>
;          | <id><State>
; The state is represented by a contiguous set of identifiers that have
; no space between them. These identifiers are associated with some
; transformation of the graphical representation of the fractal. If
; there is an identifier that does not exist, then an error
; will be raised.

; <Transformation> := [<id> -> <State>]
; A transformation is some identifier to some new state. These 
; transformations update the state of the fractal and the state of the
; fractal determines its graphical representation. All transformations
; must be represented in terms of other bindings <id>s.

; iterate : (-> IFractal Natural IFractal)
; Iterate will apply transformations on the current state of the fractal
; to create a new state. 

; render/pict : (-> IFractal Natural PosInt PosInt Pict)
; Will render the provided iterative fractal with the provided number of iterations applied
; and scaled to fit the provided window bounds

; render/interactive : (-> IFractal PosInt PosInt Ifractal)
; Will render the provided iterative fractal in a window of the specified size. Users
; can iterate the fractal backwards and forwards with the left and right arrow keys,
; and the current level of iteration will be displayed
```

## Milestones
Below are the milestones for this project:
- 02/16/25: Solidify the syntax and semantics of our DSL \
    - While we have determined our goal and the concepts that lay within it, certain implementation details still need to be finalized. This includes which other properties besides the Hausdorff dimension to include (since many are mostly equivalent), whether our L-Systems should be context-free (transformations only operate on a single binding at a time vs allowing a pattern of bindings to have a unique transformation), and commands to include for our L-Systems.
- 02/23/25: Implement the runtime of the functionality of escape time fractals
- 03/02/25: Implement the runtime of the functionality of iterative fractals
- 03/09/25: Add the compile time macros of the DSL to enable for two dimensional fractals to be generated using standard notations.
- 03/20/25: Ensure reasonable error messages for the language and language smoothly extends to three dimensions
    - If needed, this is the point by which we will make sure the grammar of the language is easily translatable into three dimensions. The main feature that we may need to edit is our assumptions on Space, as while the complex plane is standard to use in two dimensions, there is no analagous representation in three dimensions.
- 04/06/25: Add runtime support for three dimensional rendering of fractals for both escape time and iterative fractals
- 04/13/25: Extend the language syntax to allow for three dimensional fractal generation using standard notations


## Example Programs

Example escape-time fractal program to generate the mandelbrot set:
```lisp
(define mand-upd (λ (z c) (+ (* z z) c)))

(define mand-etf (generate-etfractal mand-upd))

(render mand-etf simple-color
        #:max-iterations 100
        #:escape-bounds 2
        #:horizontal-bounds (1+0.5i -1-0.5i)
        #:vertical-bounds (-1-0.5i 1+0.5i)
        #:window-width 600 #:window-height 600)
```

Example iterative fractal program to draw the Koch Snowflake using alternating red and black lines:
```lisp
(define koch (generate-ifractal [([F : (combine (draw 10 "black") (draw 10 "red"))]
                               [+ : (turn 60)] [- : (turn -120)])
                              [F-F-F]
                              ([F -> F+F-F+F])]))

(render koch 2 500 500)

(render/interactive koch 600 600)
```