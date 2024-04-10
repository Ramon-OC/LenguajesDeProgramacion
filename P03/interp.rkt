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


;Función auxiliar tomada del laboratorio para aplicar a cada with de la lista las propiedades del with
(define (with*->with bs cuerpo)
  (match bs
    ['() cuerpo]
    [(cons x xs) (with (binding-id x) (binding-value x) (with*->with xs cuerpo))]))

;; Parametros:
;;  - expr: WAE
;; Tipo del valor de retorno: number ó boolean ó string
(define (interp expr)
  (type-case WAE expr
    [id (x) (error 'interp "Variable libre")] ;;Se regresa id pues no tiene valor
    [num (x) x] ;;Se regresa un número 
    [str (x) x] ;; Se regresa una cadena
    [bool (x) x] ;; Se regresa un booleano
    [noT (x) (let ([v (interp x)]) ;;Se checa que sea booleano y regresa el valor o error si es de otro tipo
               (if (boolean? v)
                   (not x)
                   (error 'interp "El valor de not tiene que ser del tipo bool")))]
    [with (id valor cuerpo) (let ([aid (interp id)]) ;; Se regresa la evaluación o error en caso de no usar un identificador correcto
                              (if (id? aid)
                                  (interp (subst id (interp valor) cuerpo))
                                  (error "No es un identificador")))]
                            
    [op (f izq der) (let([val (interp izq)]) (let ([val1 (interp der)]) ;; Se realiza la operación indicada y se checa que sean números
                      (if(and (num? val) (num val1))
                         (f (interp izq) (interp der))
                           (error "No es número"))))]
    [with* (bindings cuerpo) (interp (with*->with bindings cuerpo))])) ;;Se regresa la asignación de cada uno de los withs
