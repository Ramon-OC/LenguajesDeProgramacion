#lang plai

(require "grammars.rkt")

;; Falta crear el parser de condicionales
(define (parse s-exp)

  (match s-exp
    [(? number?) (numS s-exp)]
    [(? symbol?) (parse-id s-exp)]
    [(? boolean?) (boolS s-exp)]
    [(? string?) (strS s-exp)]

    ;; parser en el caso de operadores
    [(cons x xs)
     (case x
       [(+ - * / < <= > >=  = not or and) (parse-op (cons x xs))]
       [(modulo expt)
        (if (equal? (length s-exp) 3)
            (parse-op (cons x xs))
            (error 'parse "Error en el número de parametros para binario"))]
       [(add1 sub1 not zero?)
        (if (equal? (length s-exp) 2)
            (parse-op (cons x xs))
            (error 'parse "Error en el número de parametros para unario"))])]

    [(list 'with bindings body) 
     (withS (parse-binding bindings) (parse body))]
    [(list 'with* bindings body) 
     (with*S  (parse-binding bindings) (parse body))]
    

    ;; Funciones.
    [(list 'fun params body) (parse-fun params body)]
    [(list (list 'fun params body) args) 
     (appS (parse-fun params body) (map parse args))]
    [(list 'f args) (appS (parse 'f) (map parse args))]

    [(list 'app fun args) (appS (parse fun) (map parse args))]
   
    ;; Falta crear el parser de condicionales
    ))


;; Declaración de funciones auxiliares

;; Dada una s-exp, se encarga de parsear un identificador
;; de tipo idS, si es falso se asigna el bool #f, #t en caso
;; contrario.
(define (parse-id s-exp)
  (cond
    [(equal? s-exp 'false) (boolS #f)]
    [(equal? s-exp 'true) (boolS #t)]
    [else (idS s-exp)]))

;; Parser en el caso de un operador
(define (parse-op sexp)
  (opS (operator (car sexp)) (map parse (cdr sexp))))

;; Falta agregar más casos
(define (operator sexp)
  (case sexp
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(=) =]
    [(<) <]
    [(<=) <=]
    [(>) >]
    [(>=) >=]
    [(modulo) modulo]
    [(expt) expt]
    [(add1) add1]
    [(sub1) sub1]
    [(not) not]
    [(and) (λ args (foldr (λ (x y) (and x y)) #t args))]
    [(or) (λ args (foldr (λ (x y) (or x y)) #f args))]
    [(zero?) zero?]))

;; Parse bindings
(define (parse-binding xs)
  (if (dob-elem? (bidings xs))
      (error (~a "Presencia de elementos duplicados"))
      (map (λ (b) (binding (car b) (parse (cadr b)))) xs)))

;; Extrae los identificadores (bidings) del with
(define (bidings with-list)
  (if (empty? with-list)
      '()
      (cons (caar with-list) (bidings (cdr with-list)))))

;; Parse fun
(define (parse-fun params body)
  (if (dob-elem? params)
      (error (~a "parser: parámetro definido dos veces: " (car params)))
      (funS params (parse body))))

;; Verifica que una lista xs no contenga elementos repetidos
;; devuelve verdadero si los hay, si no falso (la lista estuvo vacía
;; después del recorrido completo)
(define (dob-elem? xs)
  (cond
    [(member (car xs) (cdr xs)) #t]
    [(empty? xs) #f]
    [else (dob-elem? (cdr xs))]))