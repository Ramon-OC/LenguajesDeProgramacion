#lang plai

(require "grammars.rkt")

(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp)
     (cond
       [(eq? s-exp 'true) (bool #t)]
       [(eq? s-exp 'false) (bool #f)]
       [(or (eq? s-exp 'num?) (eq? s-exp 'str?) (eq? s-exp 'bool?)) (op (parse-list (list (parse (cadr s-exp)))))]
       [(or (eq? s-exp '+) (eq? s-exp '-) (eq? s-exp '*)
            (eq? s-exp '/) (eq? s-exp 'modulo) (eq? s-exp 'min)
            (eq? s-exp 'max) (eq? s-exp 'expt) (eq? s-exp 'sqrt)
            (eq? s-exp '<) (eq? s-exp '>)
            (eq? s-exp '<=) (eq? s-exp '>=) (eq? s-exp '=)
            (eq? s-exp 'not) (eq? s-exp 'and) (eq? s-exp 'or)) (op (parse-list (cdr s-exp)))]
       [(eq? s-exp 'sub1) (op (parse (list 'sub1 (cadr s-exp))))]
       [(eq? s-exp 'num?) (op (parse (list 'num? (cadr s-exp))))]
       [else (id s-exp)])]
    [(boolean? s-exp) (bool s-exp)]
    [(string? s-exp) (str s-exp)]
    [(list? s-exp)
     (let ([keyword (car s-exp)]
           [args (cdr s-exp)])
       (cond
         [(eq? keyword 'op) (op (parse-list args))]
         [(eq? keyword 'fun) (fun (parse-list (car args)) (parse (cadr args)))]
         [(eq? keyword 'app) (app (parse (car args)) (parse-list (cdr args)))]
         [(eq? keyword 'iF) (iF (parse (car args)) (parse (cadr args)) (parse (caddr args)))]
         [else (error 'parse "Expresión inválida")]))]
    [else (error 'parse "Expresión inválida")]))

(define (parse-list lst)
  (map parse lst))