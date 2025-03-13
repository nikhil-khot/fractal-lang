#lang racket

(require (for-syntax syntax/parse))
(require graphics/value-turtles)
(require 2htdp/universe)
(require pict)

(provide generate-ifractal
         render
         iterate
         bang)
         

; Iterative Fractal

(define-struct ifractal [bindings state transformations is-2D hausdorff])
; (ifractal [HashOf Bindings] (ListOf <State>) [HashOf <Transformation>] Boolean Number)

; generate-ifractal : -> <L-System> <Space> IFractal
; <L-System> := [(<Binding> ...+) [<State>] (<Transformation> ...)]
(begin-for-syntax
  (define-syntax-class binding
      #:description "binding pair"
      (pattern [x:id (~datum :) f:command]))
  (define-syntax-class transform
      #:description "transformation"
      (pattern [x:id (~datum ->) y:id ...+]))
  (define-syntax-class command
      #:description "command"
      (pattern (~or ((~datum draw) x)
                    ((~datum turn) x)))))


(define-syntax generate-ifractal
  (lambda (stx)
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
    (syntax-parse stx
      [(_ binds:binding ...)
       #'(make-immutable-hash
          (for/list ([i (list 'binds.x ...)]
                     [b (list (parse-commands binds.f) ...)])
            (cons i b)))])))

(define-syntax parse-commands
  (lambda (stx)
    (syntax-parse stx
      [(_ ((~datum draw) x)) #'(lambda (t) (draw x t))]
      [(_ ((~datum turn) x)) #'(lambda (t) (turn x t))])))

; <State> := <id>
;          | <id><State>

(define-syntax parse-state
  (lambda (stx)
    (syntax-parse stx
      [(_ x:id) #'(for/list ([char (string->list (symbol->string 'x))])
                      (string->symbol (string char)))]
      [(_) #'()])))
  
; <Transformation> := [<id> -> <State>]

(define-syntax parse-transforms
  (lambda (stx)
    (syntax-parse stx
      [(_ transforms:transform ... alphabet:id ...+)
       #'(let ([defined-xs (list 'transforms.x ...)]
               [defined-ys (list (parse-state transforms.y ...) ...)])
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


(define-struct fract [fractal iter imgs])


(define (render-frac f)
  (pict->bitmap
   (scale-to-fit
    (render f 0 500 500)
    500 500)
   ))

(define (draw-handler f)
  (list-ref (fract-imgs f) (- (length (fract-imgs f)) (add1 (fract-iter f)))))


(define (key-handler f ke)
  (cond [(key=? ke "left") (fract (fract-fractal f) (sub1 (fract-iter f)) (fract-imgs f))]
        [(key=? ke "right") (if (= (length (fract-imgs f)) (add1 (fract-iter f)))
                                (let ([new-frac (iterate (fract-fractal f) 1)])
                                  (fract new-frac (add1 (fract-iter f)) (cons (render-frac new-frac) (fract-imgs f))))
                                (fract (fract-fractal f) (add1 (fract-iter f)) (fract-imgs f)))]
        [else f]))

#;(define (bang f)
  (big-bang f
    (on-draw render-frac)
    (display-mode 'fullscreen)
    (on-key (λ (frac _) (iterate frac 1)))))
(define (bang f)
  (big-bang (fract f 0 (cons (render-frac f) '()))
    (on-draw draw-handler)
    (display-mode 'fullscreen)
    (on-key key-handler)))




      