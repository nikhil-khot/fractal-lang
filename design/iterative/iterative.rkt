#lang racket

(require (for-syntax syntax/parse))
(require graphics/value-turtles)
(require 2htdp/universe)
(require pict)
(require (prefix-in img- 2htdp/image))

; Iterative Fractal

(define-struct ifractal [bindings state transformations is-2D hausdorff])
; (ifractal [HashOf Bindings] (ListOf <State>) [HashOf <Transformation>] Boolean Number)

; generate-ifractal : -> <L-System> <Space> IFractal
; <L-System> := [(<Binding> ...+) [<State>] (<Transformation> ...)]



(define-syntax generate-ifractal
  (lambda (stx)
    (define-syntax-class binding
      #:description "binding pair"
      (pattern [x:id (~datum :) f]))
    (define-syntax-class transform
      #:description "transformation"
      (pattern [x:id (~datum ->) y:id ...+]))
    (syntax-parse stx
      [(_ [(binds:binding ...+) [state:id ...+] (transforms:transform ...)])
       #'(ifractal (parse-bindings binds ...)
                   (parse-state state ...)
                   (parse-transforms transforms ... binds.x ...)
                   '2D 0)])))

; <Binding> := [<id>: <command>]
; A <command> is one of or a composition of commands from a yet to be defined list of actions,
; including moving forward, drawing forward, rotating,
; saving the current location, returning to a location,
; and more as we define the need for them

(define-syntax parse-bindings
  (lambda (stx)
    (define-syntax-class command
      #:description "command"
      (pattern (~or ((~datum draw) x)
                    ((~datum turn) x))))
    (define-syntax-class binding
      #:description "binding pair"
      (pattern [x:id (~datum :) f:command]))
    (syntax-parse stx
      [(_ binds:binding ...)
       #'(make-immutable-hash
          (for/list ([i (list 'binds.x ...)]
                     [b (list (parse-commands binds.f) ...)])
            (cons i b)))])))

(define-syntax parse-commands
  (lambda (stx)
    (define-syntax-class command
      #:description "command"
      (pattern (~or ((~datum draw) x)
                    ((~datum turn) x))))
    (syntax-parse stx
      [(_ ((~datum draw) x)) #'(lambda (t) (draw x t))]
      [(_ ((~datum turn) x)) #'(lambda (t) (turn x t))])))

; <State> := <id>
;          | <id><State>

(define-syntax parse-state
  (lambda (stx)
    (syntax-parse stx
      [(_ x:id ...) #'(list 'x ...)])))

; <Transformation> := [<id> -> <State>]

(define-syntax parse-transforms
  (lambda (stx)
    (define-syntax-class transform
      #:description "transformation"
      (pattern [x:id (~datum ->) y:id ...+]))
    (syntax-parse stx
      [(_ transforms:transform ... alphabet:id ...+)
       #'(let ([defined-xs (list 'transforms.x ...)]
               [defined-ys (list (list 'transforms.y ...) ...)])
           (make-immutable-hash
            (for/list ([ltr (list 'alphabet ...)])
              (if (member ltr defined-xs)
                          (cons ltr (list-ref defined-ys
                                              (index-of defined-xs ltr)))
                          (cons ltr ltr)))))])))

; render : (-> IFractal Natural PosInt PosInt Natural)
(define (render ifractal iters height width)
  (let ([frac (iterate ifractal iters)]
        [turtle (set-pen-color (turtles width height) "black")])
    (draw-state (ifractal-state frac) (ifractal-bindings frac) turtle)))

(define (draw-state states bindings turtle)
  (if (empty? states)
      (turtles-pict turtle)
      (draw-state (rest states)
                  bindings
                  ((hash-ref bindings (first states)) turtle))))

; hausdorff-dim : (-> IFractal Number)

; iterate : (-> IFractal Natural IFractal)
(define (iterate frac iters)
  (ifractal (ifractal-bindings frac)
            (update-state (ifractal-state frac) (ifractal-transformations frac) iters)
            (ifractal-transformations frac)
            (ifractal-is-2D frac)
            (ifractal-hausdorff frac)))

; update-state : (-> [ListOf Symbol] [HashOf Symbol [ListOf Symbol]] Natural [ListOf Symbol])
(define (update-state state-list transforms iters)
  (if (zero? iters)
      state-list
      (update-state
       (flatten (for/list ([state state-list])
                 (hash-ref transforms state)))
       transforms (sub1 iters))))


; Sierpinski approximation
#;(define f (generate-ifractal [([A : (draw 1)] [B : (draw 1)] [+ : (turn 60)] [- : (turn -60)])
             [A]
             ([A -> B - A - B] [B -> A + B + A])]))

; Dragon curve
(define f (generate-ifractal [([A : (draw 1)] [B : (draw 1)] [+ : (turn 90)] [- : (turn -90)])
             [A]
             ([A -> A + B] [B -> A - B])]))


(define (render-frac f)
  (pict->bitmap (render f 0 500 500)))


(define (BANG)
  (big-bang f
    (on-draw render-frac)
    (display-mode 'fullscreen)
    (on-key (λ (frac _) (iterate frac 1)))))

(BANG)
      