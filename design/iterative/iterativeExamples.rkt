#lang racket

(require "iterative.rkt")

; Sierpinski approximation
#;(define f (generate-ifractal [([A : (draw 1)] [B : (draw 1)] [+ : (turn 60)] [- : (turn -60)])
             [A]
             ([A -> B - A - B] [B -> A + B + A])]))

; Dragon curve
(define f (generate-ifractal [([A : (draw 1)] [B : (draw 1)] [+ : (turn 90)] [- : (turn -90)])
             [A]
             ([A -> A + B] [B -> A - B])]))

(bang f)