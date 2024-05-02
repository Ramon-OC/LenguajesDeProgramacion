#lang plai

(require "grammars.rkt")

(define (parse s-exp)
  (cond
    [(number? s-exp) (num s-exp)]
    [(symbol? s-exp) (id s-exp)]
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