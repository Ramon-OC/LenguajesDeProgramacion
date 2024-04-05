#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Parametros:
;;  - sub-id: symbol
;;  - value: WAE
;;  - expr: WAE
;; Tipo del valor de retorno: WAE
(define (subst id value expr)
  (cond
    [(id? expr)
     (if (equal? (id-literal expr) id)
         value
         expr)]
    [(op? expr)
     (op (op-operacion expr)
         (subst id value (op-izq expr))
         (subst id value (op-der expr)))]
    [(with? expr)
     (let ([bindings (with expr)]
           [body (with-body expr)])
       (if (member (binding-id (car bindings)) (map binding-id bindings))
           (with (list->binding (map (lambda (b)
                                       (binding (binding-id b)
                                                (subst id value (binding-value b))))
                                     bindings))
                 body
                 (subst id value body))
           (with bindings
                 body
                 (subst id value body))))]
    [(with*? expr)
     (let ([bindings (with*-bindings expr)]
           [body (with*-body expr)])
       (if (member id (map binding-id bindings))
           (let* ([split-bindings (split-at bindings (index-of id (map binding-id bindings)))]
                  [before-bindings (car split-bindings)]
                  [after-bindings (cdr split-bindings)])
             (with* (append (map (lambda (b)
                                    (binding (binding-id b)
                                             (subst id value (binding-value b))))
                                  before-bindings)
                             after-bindings)
                    body
                    (subst id value body)))
           (with* (map (lambda (b)
                          (binding (binding-id b)
                                   (subst id value (binding-value b))))
                        bindings)
                  body
                  (subst id value body))))]
    [else expr]))


(test (subst 'x (num 1) (num 20)) (num 20))
(test (subst 'x (num 1) (bool #t)) (bool #t))
(test (subst 'x (num 1) (str "Hola")) (str "Hola"))
(test (subst 'x (num 1) (id 'y)) (id 'y))
(test (subst 'x (num 1) (id 'x)) (num 1))
(test (subst 'x (num 1) (parse '{+ x x x}))
      (parse '{+ 1 1 1}))

;(subst 'x (num 1) (parse '{with {{a 1} {x x} {b x}} x}))
;(subst 'x (num 1) (with 'a (num 1) (with 'x (id 'x) (with 'b (id 'x) (id 'x)))))

;; Parametros:
;;  - expr: WAE
;; Tipo del valor de retorno: number ó boolean ó string
(define (interp expr)
  (error 'interp "Sin implementar"))
