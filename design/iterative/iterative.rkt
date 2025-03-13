#lang racket

(require (for-syntax syntax/parse))
(require graphics/value-turtles)
(require 2htdp/universe)
(require pict)
(require rackunit)

(provide generate-ifractal
         render
         iterate
         bang)
         
; Iterative Fractals

(define-struct ifractal [bindings state transformations is-2D])
; (ifractal [HashOf Bindings] (ListOf <State>) [HashOf <Transformation>] Boolean)

; <L-System> := [(<Binding> ...+) [<State>] (<Transformation> ...)]
(begin-for-syntax
  (define-syntax-class binding
      #:description "binding pair"
      (pattern [x:id (~datum :) f:command]))
  (define-syntax-class transform
      #:description "transformation"
      (pattern [x:id (~datum ->) y:id]))
  (define-syntax-class command
      #:description "command"
      (pattern (~or ((~datum draw) x color:string)
                    ((~datum move) x)
                    ((~datum turn) x)))))

; generate-ifractal : (-> <L-System> <Space> IFractal)
; Creates an IFractal
(define-syntax generate-ifractal
  (lambda (stx)
    (syntax-parse stx
      [(_ [(binds:binding ...+) [state:id] (transforms:transform ...)])
       #'(let ([parsed-binds (parse-bindings binds ...)]
               [parsed-state (parse-state state)]
               [parsed-transforms (parse-transforms transforms ... binds.x ...)])

           (andmap (λ (l) (unless (member l (hash-keys parsed-binds))
                            (error "Undefined identifier in initial state:" l)))
                   parsed-state)
           (andmap (λ (l) (unless (member l (hash-keys parsed-binds))
                            (error "Undefined identifier in transforms - keys:" l)))
                   (list 'transforms.x ...))
           (andmap (λ (los)
                     (andmap (λ (l) (unless (member l (hash-keys parsed-binds))
                                      (error "Undefined identifier in transforms - values:" l)))
                             los)) (hash-values parsed-transforms))
             
           (ifractal parsed-binds parsed-state parsed-transforms '2D))])))

; <Binding> := [<id>: <command>]
; A <command> is one of or a composition of commands from a defined list of actions.
; Currently including: moving, drawing in color, and rotating
; TODO: Saving location, returning to location

; parse-bindings: (-> <Binding> ...+ [HashOf Symbol [ListOf <command>]])
(define-syntax parse-bindings
  (lambda (stx)
    (syntax-parse stx
      [(_ binds:binding ...)
       #'(make-immutable-hash
          (for/list ([i (list 'binds.x ...)]
                     [b (list (parse-commands binds.f) ...)])
            (unless (= 1 (string-length (symbol->string i)))
              (error "Can only use bindings of length 1:" i))
            (cons i b)))])))

(define-syntax parse-commands
  (lambda (stx)
    (syntax-parse stx
      [(_ ((~datum draw) x color:string)) #'(lambda (t) (draw x (set-pen-color t color)))]
      [(_ ((~datum move) x)) #'(lambda (t) (move x t))]
      [(_ ((~datum turn) x)) #'(lambda (t) (turn x t))])))

; <State> := <id>
;          | <id><State>

; parse-state : (-> <State> [ListOf Symbol])
(define-syntax parse-state
  (lambda (stx)
    (syntax-parse stx
      [(_ x:id) #'(for/list ([char (string->list (symbol->string 'x))])
                      (string->symbol (string char)))]
      [(_) #'()])))
  
; <Transformation> := [<id> -> <State>]
; parse-transforms: (-> <Transformation> ... id ...+ [HashOf Symbol [ListOf Symbol]])
(define-syntax parse-transforms
  (lambda (stx)
    (syntax-parse stx
      [(_ transforms:transform ... alphabet:id ...+)
       #'(let ([defined-xs (list 'transforms.x ...)]
               [defined-ys (list (parse-state transforms.y) ...)])
           (make-immutable-hash
            (for/list ([ltr (list 'alphabet ...)])
              (if (member ltr defined-xs)
                          (cons ltr (list-ref defined-ys
                                              (index-of defined-xs ltr)))
                          (cons ltr (list ltr))))))])))


; iterate : (-> IFractal Natural IFractal)
; Iterates the fractal the provided number of times
(define (iterate frac iters)
  (if (< iters 0)
      (error "Cannot iterate backwards")
      (ifractal (ifractal-bindings frac)
            (update-state (ifractal-state frac) (ifractal-transformations frac) iters)
            (ifractal-transformations frac)
            (ifractal-is-2D frac))))

; update-state : (-> [ListOf Symbol] [HashOf Symbol [ListOf Symbol]] Natural [ListOf Symbol])
; Updates the state iters times, using the provided transforms
(define (update-state state-list transforms iters)
  (if (zero? iters)
      state-list
      (update-state
       (flatten (for/list ([state state-list])
                 (hash-ref transforms state)))
       transforms (sub1 iters))))


(module+ test
  (check-exn #rx"Can only use bindings of length 1: 'AB"
             (lambda () (generate-ifractal [([AB : (draw 1 "white")])
             [AB]
             ([AB -> ABAB])])))
  (check-exn #rx"Undefined identifier in initial state: 'C"
             (lambda () (generate-ifractal [([A : (draw 1 "white")] [B : (turn 90)])
             [C]
             ([A -> AB] [B -> AB])])))
  (check-exn #rx"Undefined identifier in transforms - keys: 'C"
             (lambda () (generate-ifractal [([A : (draw 1 "white")] [B : (turn 90)])
             [A]
             ([C -> AB] [B -> AB])])))
  (check-exn #rx"Undefined identifier in transforms - values: 'C"
             (lambda () (generate-ifractal [([A : (draw 1 "white")] [B : (turn 90)])
             [B]
             ([A -> AB] [B -> CB])])))
  (check-equal? '(A B C B C) (parse-state ABCBC))
  (check-equal? '(A B) (hash-ref (parse-transforms [A -> AB] A B) 'A))
  (check-equal? '(B) (hash-ref (parse-transforms [A -> AB] A B) 'B))
  (check-not-false (andmap (λ (i)
                             (member i (hash-keys (parse-bindings [A : (move 10)] [B : (turn 90)]))))
                           '(A B)))
  (define f (generate-ifractal [([A : (draw 1 "black")] [B : (draw 1 "black")] [+ : (turn 60)] [- : (turn -60)])
                              [A]
                              ([A -> B-A-B] [B -> A+B+A])]))
  (check-equal? (parse-state B-A-B) (ifractal-state (iterate f 1)))
  (check-equal? (parse-state A+B+A-B-A-B-A+B+A) (ifractal-state (iterate f 2))))


; render : (-> IFractal Natural PosInt PosInt Pict)
; Renders the provided IFractal (iterated iters times) as a Pict
(define (render ifractal iters height width)
  (let ([frac (iterate ifractal iters)]
        [turtle (turtles width height)])
    (draw-state (ifractal-state frac) (ifractal-bindings frac) turtle)))

; draw-state: (-> (ListOf <State>) [HashOf <Bindings>] Turtle Pict)
; D=Recursively executes the turtle commands bound to each state value
(define (draw-state states bindings turtle)
  (if (empty? states)
      (turtles-pict turtle)
      (draw-state (rest states)
                  bindings
                  ((hash-ref bindings (first states)) turtle))))


; Struct to hold the fractal, current iteration, and previous iterations
(define-struct fract [fractal iter imgs])

; Renders the fractal, scales it, and turns it into a bitmap for big-bang to use
(define (render-frac f)
  (let ([img (render f 0 500 500)])
    (pict->bitmap
     (scale-to-fit
      img
      (min 500 (pict-width img)) (min 500 (pict-height img)))
     )))

; Accesses the corresponding bitmap for the current fractal iteration
(define (draw-handler f)
  (list-ref (fract-imgs f) (- (length (fract-imgs f)) (add1 (fract-iter f)))))

(define (key-handler f ke)
  (cond [(key=? ke "left") (fract (fract-fractal f) (sub1 (fract-iter f)) (fract-imgs f))]
        [(key=? ke "right") (if (= (length (fract-imgs f)) (add1 (fract-iter f)))
                                (let ([new-frac (iterate (fract-fractal f) 1)])
                                  (fract new-frac (add1 (fract-iter f)) (cons (render-frac new-frac) (fract-imgs f))))
                                (fract (fract-fractal f) (add1 (fract-iter f)) (fract-imgs f)))]
        [else f]))

(define (bang f)
  (fract-fractal (big-bang (fract f 0 (cons (render-frac f) '()))
    (on-draw draw-handler)
    (display-mode 'fullscreen)
    (on-key key-handler))))




      