#lang racket

(require "fractals/iterative/iterative.rkt")
(require "fractals/escapeTime/etfractal.rkt")

(provide generate-ifractal
         render/pict
         iterate
         render/interactive
         generate-etfractal
         render
         simple-color
         simple-color-ratio
         point-in-set?
         steps-to-escape)