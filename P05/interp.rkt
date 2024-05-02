#lang plai

(require "grammars.rkt")

(define (interp ast env)
  (match ast
    [(num n) (numV n)]
    [(id x) (lookup x env)]
    [(bool b) (boolV b)]
    [(str s) (strV s)]
    [(op operator args) (apply-operator operator (interp-list args env))]
    [(fun params body) (closureV params body env)]
    [(app function args) (apply-function (interp function env) (interp-list args env))]
    [(iF test-expr then-expr else-expr)
     (if (interp test-expr env)
         (interp then-expr env)
         (interp else-expr env))]))

(define (interp-list lst env)
  (map (lambda (exp) (interp exp env)) lst))

(define (apply-operator operator args)
  (apply operator args))

(define (apply-function closure args)
  (match closure
    [(closureV params body env)
     (interp body (extend-env params args env))]))

(define (extend-env params args env)
  (cond
    [(empty? params) env]
    [else (cons-env (car params) (car args) (extend-env (cdr params) (cdr args) env))]))

(define (lookup search-id env)
  (match env
    [(mt-env) (error 'lookup "Variable no encontrada")]
    [(cons-env id value rest)
     (if (eq? id search-id)
         value
         (lookup search-id rest))]))