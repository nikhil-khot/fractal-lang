#lang racket

(require "iterative.rkt")

; Sierpinski approximation
#;(define f (generate-ifractal [([A : (draw 1 "black")] [B : (draw 1 "black")] [+ : (turn 60)] [- : (turn -60)])
                              [B-A-B]
                              ([A -> B-A-B] [B -> A+B+A])]))

; Dragon curve
#;(define f (generate-ifractal [([A : (draw 50 "red")] [B : (draw 50 "blue")] [+ : (turn 90)] [- : (turn -90)])
             [A]
             ([A -> A+B] [B -> A-B])]))

#;(define f (generate-ifractal [([F : (draw 1 "black")] [G : (draw 1 "black")] [+ : (turn 120)] [- : (turn -120)])
                              [F-G-G]
                              ([F -> F-G+F+G-F] [G -> GG])]))

#;(define f (generate-ifractal [([X : (none)] [F : (draw 1 "black")]
                               [+ : (turn 25)] [- : (turn -25)] [* : (turn 90)]
                               [s : (save)] [r : (return)])
                              [*-X]
                              ([X -> F+ssXr-Xr-Fs-FXr+X] [F -> FF])]))

#;(define f (generate-ifractal [([F : (combine (draw 10 "black") (draw 10 "red"))]
                               [+ : (turn 60)] [- : (turn -120)])
                              [F-F-F]
                              ([F -> F+F-F+F])]))

#;(define f (generate-ifractal [([A : (none)] [B : (none)] [F : (draw 10 "black")]
                               [+ : (turn 90)] [- : (turn -90)])
                              [A]
                              ([A -> +BF-AFA-FB+] [B -> -AF+BFB+FA-])]))

#;(define f (generate-ifractal [([T : (draw 1 "brown")] [B : (draw 0.9 "green")]
                               [s : (save)] [r : (return)]
                               [+ : (turn 25)] [- : (turn -55)] [* : (turn 90)])
                              [*B]
                              ([T -> TT] [B -> Ts+Br-B])]))

(define f (generate-ifractal [([F : (draw 10 "black")] [B : (move -5)] [G : (draw 10 "black")]
                               [s : (save)] [r : (return)]
                               [+ : (turn 90)] [- : (turn -90)] [- : (turn 0)])
                              [D]
                              ([F -> -BFB+G-BFB+] [G -> G])]))


(bang f)

