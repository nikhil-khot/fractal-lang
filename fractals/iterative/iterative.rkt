#lang racket

(require (for-syntax syntax/parse))
(require graphics/value-turtles)
(require 2htdp/universe)
(require pict)
(require rackunit)
(require syntax/macro-testing)

(provide generate-ifractal
         render/pict
         iterate
         render/interactive)
         
; Iterative Fractals

(define-struct ifractal [bindings state transformations])
; (ifractal [HashOf Bindings] (ListOf <State>) [HashOf <Transformation>])

; <L-System> := [(<Binding> ...+) [<State>] (<Transformation> ...)]
(begin-for-syntax
  (define-syntax-class binding
      #:description "binding pair"
      (pattern [x:id (~datum :) f:command]
        #:fail-unless (= 1 (string-length (symbol->string (syntax-e #'x))))
        "Can only use binding identifiers of length 1"))
  (define-syntax-class transform
      #:description "transformation"
      (pattern [x:id (~datum ->) y:id]
        #:fail-unless (= 1 (string-length (symbol->string (syntax-e #'x))))
        "Can only transform identifiers of length 1"))
  (define-syntax-class command
      #:description "command"
      (pattern (~or ((~datum draw) x:number color:string)
                    ((~datum move) x:number)
                    ((~datum turn) x:number)
                    ((~datum save))
                    ((~datum return))
                    ((~datum none))
                    ((~datum combine) c:command ...+)))))

; generate-ifractal : (-> <L-System> IFractal)
; Creates an IFractal
(define-syntax generate-ifractal
  (lambda (stx)
    (syntax-parse stx
      [(_ [(binds:binding ...+) [state:id] (transforms:transform ...)])
       (check-ids #'(binds.x ...) #'state #'(transforms.x ...)  #'(transforms.y ...))
       #'(let ([parsed-binds (parse-bindings binds ...)]
               [parsed-state (parse-state state)]
               [parsed-transforms (parse-transforms transforms ... binds.x ...)])
           (ifractal parsed-binds parsed-state parsed-transforms))])))

(define-for-syntax (check-ids binds state t-keys t-vals)
  (let ([binds (map syntax-e (syntax-e binds))]
        [state (id->symb-list state)]
        [t-keys (map syntax-e (syntax-e t-keys))]
        [t-vals (map id->symb-list (syntax-e t-vals))])
    (andmap (λ (l) (unless (member l binds)
                   (error "Undefined identifier in initial state:" l)))
          state)
    (andmap (λ (l) (unless (member l binds)
                     (error "Undefined identifier in transforms - keys:" l)))
            t-keys)
    (andmap (λ (los)
            (andmap (λ (l) (unless (member l binds)
                             (error "Undefined identifier in transforms - values:" l)))
                    los)) t-vals)
    (andmap (λ (l1) (unless (= 1 (length (filter (λ (l2) (equal? l1 l2)) binds)))
                    (error "Duplicate binding:" l1))) binds)
    (andmap (λ (l1) (unless (= 1 (length (filter (λ (l2) (equal? l1 l2)) t-keys)))
                    (error "Duplicate transformation key:" l1))) t-keys)))

(define-for-syntax (id->symb-list x)
  (let ([sym (syntax-e x)])
    (unless (symbol? sym) (error "Not a symbol:" sym))
  (for/list ([char (string->list (symbol->string sym))])
    (string->symbol (string char)))))
  

; <Binding> := [<id>: <command>]
; A <command> is one of or a composition of commands from a defined list of actions.
; Currently including: moving, drawing in color, rotating, doing nothing,
; saving current position and rotation, returning to saved position and rotation

; parse-bindings: (-> <Binding> ...+ [HashOf Symbol <command>])
(define-syntax parse-bindings
  (lambda (stx)
    (syntax-parse stx
      [(_ binds:binding ...)
       #'(make-immutable-hash
          (for/list ([i (list 'binds.x ...)]
                     [b (list (parse-commands binds.f) ...)])
            (cons i b)))])))

(define-struct turtle-window [turtle stack])
; (turtle-window Turtle [ListOf [VectorOf Real Real Real]]) 

; parse-commands: (-> <command> (-> TurtleWindow TurtleWindow))
(define-syntax parse-commands
  (lambda (stx)
    (syntax-parse stx
      [(_ ((~datum draw) x:number color:string))
       #'(draw/window x color)]
      [(_ ((~datum move) x:number))
       #'(move/window x)]
      [(_ ((~datum turn) x:number))
       #'(turn/window x)]
      [(_ ((~datum save)))
       #'(save/window)]
      [(_ ((~datum return)))
       #'(return/window)]
      [(_ ((~datum none))) #'(λ (t) t)]
      [(_ ((~datum combine) c:command ...+))
       #'(let ([commands (list (parse-commands c) ...)])
           (λ (t) (foldl (λ (command t) (command t)) t commands)))])))

; draw/window: (-> Number Color (-> TurtleWindow TurtleWindow))
(define (draw/window num color)
  (λ (t)
    (turtle-window
     (draw num (set-pen-color (turtle-window-turtle t) color))
     (turtle-window-stack t))))

; move/window: (-> Number (-> TurtleWindow TurtleWindow))
(define (move/window num)
  (λ (t)
    (turtle-window
     (move num (turtle-window-turtle t))
     (turtle-window-stack t))))

; turn/window: (-> Number (-> TurtleWindow TurtleWindow))
(define (turn/window num)
  (λ (t)
    (turtle-window
     (turn num (turtle-window-turtle t))
     (turtle-window-stack t))))

; draw/window: (-> (-> TurtleWindow TurtleWindow))
(define (save/window)
  (λ (t) (turtle-window
                 (turtle-window-turtle t)
                 (cons (turtle-state (turtle-window-turtle t)) (turtle-window-stack t)))))

; draw/window: (-> (-> TurtleWindow TurtleWindow))
(define (return/window)
  (λ (t) (if (cons? (turtle-window-stack t))
                    (turtle-window
                     (restore-turtle-state (turtle-window-turtle t) (first (turtle-window-stack t)))
                     (rest (turtle-window-stack t)))
                    (error "No saved turtles"))))

; <State> := <id>
;          | <id><State>

; parse-state : (-> <State> [ListOf Symbol])
(define-syntax parse-state
  (lambda (stx)
    (syntax-parse stx
      [(_ x:id) (let ([state (id->symb-list #'x)])
                  #'(for/list ([char (string->list (symbol->string 'x))])
                      (string->symbol (string char))))]
      [(_) #'()])))
  
; <Transformation> := [<id> -> <State>]
; parse-transforms: (-> <Transformation> ... <id> ...+ [HashOf Symbol [ListOf Symbol]])
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
                (ifractal-transformations frac))))

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
  (check-exn #rx"Can only use binding identifiers of length 1"
             (lambda ()
               (convert-compile-time-error
                (generate-ifractal [([AB : (draw 1 "white")])
                                    [AB]
                                    ([AB -> ABAB])]))))
  (check-exn #rx"Can only transform identifiers of length 1"
             (lambda ()
               (convert-compile-time-error
                (generate-ifractal [([A : (draw 1 "white")] [B : (draw 1 "white")])
                                    [A]
                                    ([AB -> ABAB])]))))
  (check-exn #rx"Undefined identifier in initial state: 'C"
             (lambda ()
               (convert-compile-time-error
                (generate-ifractal [([A : (draw 1 "white")] [B : (turn 90)])
                                    [C]
                                    ([A -> AB] [B -> AB])]))))
  (check-exn #rx"Undefined identifier in transforms - keys: 'C"
             (lambda ()
               (convert-compile-time-error
                (generate-ifractal [([A : (draw 1 "white")] [B : (turn 90)])
                                    [A]
                                    ([C -> AB] [B -> AB])]))))
  (check-exn #rx"Undefined identifier in transforms - values: 'C"
             (lambda ()
               (convert-compile-time-error
                (generate-ifractal [([A : (draw 1 "white")] [B : (turn 90)])
                                    [B]
                                    ([A -> AB] [B -> CB])]))))
  (check-exn #rx"Duplicate binding: 'B"
             (lambda ()
               (convert-compile-time-error
                (generate-ifractal [([A : (draw 1 "white")] [B : (turn 90)] [B : (turn 90)])
                                    [B]
                                    ([A -> AB] [B -> BB])]))))
  (check-exn #rx"Duplicate transformation key: 'A"
             (lambda ()
               (convert-compile-time-error
                (generate-ifractal [([A : (draw 1 "white")] [B : (turn 90)])
                                    [B]
                                    ([A -> AB] [B -> BB] [A -> AB])]))))
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
(define (render/pict ifractal iters height width)
  (let* ([frac (iterate ifractal iters)]
         [turtle (turtle-window (turtles width height) '())]
         [img (draw-state (ifractal-state frac) (ifractal-bindings frac) turtle)])
    (cc-superimpose
     (scale-to-fit img
                   (min width (* 10 (pict-width img))) (min height (* 10 (pict-height img))))
     (blank width height))))

; draw-state: (-> (ListOf <State>) [HashOf <Bindings>] TurtleWindow Pict)
; D=Recursively executes the turtle commands bound to each state value
(define (draw-state states bindings turtle)
  (if (empty? states)
      (turtles-pict (turtle-window-turtle turtle))
      (draw-state (rest states)
                  bindings
                  ((hash-ref bindings (first states)) turtle))))


; Represents the state of rendering an iterative fractal
(define-struct world-state [fractal iter imgs width height])
; (world-state IFractal PosInt [ListOf Bitmap] PosInt PosInt)

; Renders the fractal, shows the iteration number, and turns it into a bitmap for big-bang to use
; (-> IFractal PosInt PosInt PosInt Bitmap)
(define (render-frac f iter width height)
  (let ([img (render/pict f 0 width height)])
    (pict->bitmap
     (vc-append 20
                (text (string-append "Iteration: " (number->string iter)) null (/ height 10))
                img))))

; Accesses the corresponding bitmap for the current fractal iteration
; (-> WorldState Bitmap)
(define (draw-handler w)
  (list-ref (world-state-imgs w) (- (length (world-state-imgs w))
                                    (add1 (world-state-iter w)))))

; Iterates fractals forwards/backwards
; (-> WorldState WorldState)
(define (key-handler w ke)
  (cond [(key=? ke "left")
         (if (zero? (world-state-iter w))
             w
             (world-state (world-state-fractal w) (sub1 (world-state-iter w)) (world-state-imgs w)
                          (world-state-width w) (world-state-height w)))]
        [(key=? ke "right")
         (if (= (length (world-state-imgs w)) (add1 (world-state-iter w)))
             (let ([new-frac (iterate (world-state-fractal w) 1)])
               (world-state new-frac (add1 (world-state-iter w))
                            (cons (render-frac new-frac (add1 (world-state-iter w))
                                               (world-state-width w) (world-state-height w))
                                  (world-state-imgs w))
                            (world-state-width w) (world-state-height w)))
             (world-state (world-state-fractal w) (add1 (world-state-iter w)) (world-state-imgs w)
                          (world-state-width w) (world-state-height w)))]
        [else w]))


; Displays the fractal in an interactive window of the provided size,
; allowing the fractal to be iterated forwards and backwards
; (-> IFractal PosInt PosInt IFractal)
(define (render/interactive f w h)
  (world-state-fractal
   (big-bang (world-state f 0 (cons (render-frac f 0 w h) '()) w h)
    (on-draw draw-handler)
    (on-key key-handler))))

