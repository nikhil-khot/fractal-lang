#lang racket

(require (for-syntax syntax/parse))
(require graphics/value-turtles)
(require 2htdp/universe)
(require (prefix-in img: 2htdp/image))
(require pict)
(require rackunit)

(provide generate-ifractal
         render
         iterate
         bang)
         
; Iterative Fractals

(define-struct ifractal [bindings state transformations is-2D])
; (ifractal [HashOf Bindings] (ListOf <State>) [HashOf <Transformation>] Boolean)

(define-struct turt [turtle stack])

; <L-System> := [(<Binding> ...+) [<State>] (<Transformation> ...)]
(begin-for-syntax
  (define-syntax-class parameter-binding
      #:description "parameter definition"
      (pattern [p:id (~datum =) x:number]))
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
  (define-syntax-class parameter-transform
      #:description "parameter transformation"
      (pattern [p:id (~or (~datum +=)
                          (~datum -=)
                          (~datum *=)
                          (~datum /=)) x:number]))
  (define-syntax-class command
      #:description "command"
      (pattern (~or ((~datum draw) x:number color:string)
                    ((~datum move) x:number)
                    ((~datum turn) x:number)
                    ((~datum save))
                    ((~datum return))
                    ((~datum none))
                    ((~datum combine) c:command ...+)))))

; generate-ifractal : (-> <L-System> <Space> IFractal)
; Creates an IFractal
(define-syntax generate-ifractal
  (lambda (stx)
    (syntax-parse stx
      [(_ [(binds:binding ...+) [state:id] (transforms:transform ...)])
       #'(let ([parsed-binds (parse-bindings binds ...)]
               [parsed-state (parse-state state)]
               [parsed-transforms (parse-transforms transforms ... binds.x ...)])
           (check-bindings parsed-binds parsed-state (list 'transforms.x ...) (hash-values parsed-transforms))
           (ifractal parsed-binds parsed-state parsed-transforms '2D))])))

(define (check-bindings binds state t-keys t-vals)
  (andmap (λ (l) (unless (member l (hash-keys binds))
                   (error "Undefined identifier in initial state:" l)))
          state)
  (andmap (λ (l) (unless (member l (hash-keys binds))
                   (error "Undefined identifier in transforms - keys:" l)))
          t-keys)
  (andmap (λ (los)
            (andmap (λ (l) (unless (member l (hash-keys binds))
                             (error "Undefined identifier in transforms - values:" l)))
                    los)) t-vals))
  

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
            (cons i b)))])))

(define-syntax parse-commands
  (lambda (stx)
    (syntax-parse stx
      [(_ ((~datum draw) x color:string)) #'(λ (t) (turt (draw x (set-pen-color (turt-turtle t) color))
                                                              (turt-stack t)))]
      [(_ ((~datum move) x:number)) #'(λ (t) (turt (move x (turt-turtle t))
                                                 (turt-stack t)))]
      [(_ ((~datum turn) x:number)) #'(λ (t) (turt (turn x (turt-turtle t))
                                                 (turt-stack t)))]
      [(_ ((~datum save))) #'(λ (t) (turt (turt-turtle t)
                                               (cons (turtle-state (turt-turtle t)) (turt-stack t))))]
      [(_ ((~datum return))) #'(λ (t) (if (cons? (turt-stack t))
                                               (turt (restore-turtle-state (turt-turtle t) (first (turt-stack t)))
                                                 (rest (turt-stack t)))
                                               (error "No saved turtles")))]
      [(_ ((~datum none))) #'(λ (t) t)]
      [(_ ((~datum combine) c:command ...+)) #'(let ([commands (list (parse-commands c) ...)])
                                                 (λ (t) (foldl (λ (command t) (command t)) t commands)))])))

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
  #;(check-exn #rx"Can only use bindings of length 1: 'AB"
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
  (let* ([frac (iterate ifractal iters)]
         [turtle (turt (turtles width height) '())]
         [img (draw-state (ifractal-state frac) (ifractal-bindings frac) turtle)])
    (cc-superimpose
     (scale-to-fit img
                   (min width (* 10 (pict-width img))) (min height (* 10 (pict-height img))))
     (blank width height))))

; draw-state: (-> (ListOf <State>) [HashOf <Bindings>] Turt Pict)
; D=Recursively executes the turtle commands bound to each state value
(define (draw-state states bindings turtle)
  (if (empty? states)
      (turtles-pict (turt-turtle turtle))
      (draw-state (rest states)
                  bindings
                  ((hash-ref bindings (first states)) turtle))))


; Struct to hold the fractal, current iteration, and previous iterations
(define-struct world-state [fractal iter imgs])

; Renders the fractal, scales it, and turns it into a bitmap for big-bang to use
(define (render-frac f iter)
  (let ([img (render f 0 500 500)])
    (pict->bitmap
     (vc-append 20
                (text (string-append "Iteration: " (number->string iter)) null 50)
                img)
     )))

; Accesses the corresponding bitmap for the current fractal iteration
(define (draw-handler f)
  (list-ref (world-state-imgs f) (- (length (world-state-imgs f)) (add1 (world-state-iter f))))
         )

(define (key-handler f ke)
  (cond [(key=? ke "left")
         (if (zero? (world-state-iter f))
             f
             (world-state (world-state-fractal f) (sub1 (world-state-iter f)) (world-state-imgs f)))]
        [(key=? ke "right") (if (= (length (world-state-imgs f)) (add1 (world-state-iter f)))
                                (let ([new-frac (iterate (world-state-fractal f) 1)])
                                  (world-state new-frac (add1 (world-state-iter f))
                                               (cons (render-frac new-frac (add1 (world-state-iter f)))
                                                     (world-state-imgs f))))
                                (world-state (world-state-fractal f) (add1 (world-state-iter f)) (world-state-imgs f)))]
        [else f]))

(define (bang f)
  (world-state-fractal (big-bang (world-state f 0 (cons (render-frac f 0) '()))
    (on-draw draw-handler)
    (on-key key-handler))))

