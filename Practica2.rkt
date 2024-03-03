#lang plai
#|
Practica 2.
|#

;; Ejercicio 1
(define (mapea func ls)
  (error "Sin implementar"))

;; Ejercicio 2
(define (get-by-index index ls)
  (error "Sin implementar"))

;; Ejercicio 3
;; Define el tipo Figura aquí.

;; Ejercicio 4
(define (area figura)
  (error "Sin implementar"))

;; Ejercicio 5
(define (perimetro figura)
  (error "Sin implementar"))

;; Ejercicio 6
(define-type ArbolDeBusqueda
[vacio]
[nodo (elem number?) (izq ArbolDeBusqueda?) (der ArbolDeBusqueda?)])

;; Ejercicio 6a
;; Función elimina, recibe un árbol binario de búsqueda y un elemento, elimina este último del
;; árbol y regresa el árbol resultante de dicha operación.
;; Contempla el caso de árbol vacío, si el valor por eliminar es menor que el nodo superior, se elimina
;; el elemento de la derecha, caso analogo para mayor que en el caso izquierdo, 
(define (elimina ar el)
  (match ar
    [(vacio) (vacio)]
    [(nodo x izq der)
     (cond
       [(< el x) (nodo x (elimina izq el) der)]
       [(> el x) (nodo x izq (elimina der el))]
       [else (unir izq der)])]))

;; Toma dos árboles binarios de búsqueda como parámetros, regresa un árbol que contiene todos los elementos.
(define (unir izq der)
  (cond
    [(vacio? izq) der]
    [(vacio? der) izq]
    [else
     (let* ([max-izq (maximo izq)]
            [new-izq (elimina izq (maximo izq))])
       (nodo max-izq new-izq der))]))

;; Toma un árbol binario como parametro, regresa el elemento máximo contenido en ese árbol
;; OJO: se puede definir un caso ''mínimo'', pero esto nos devolvería el árbol con una rotación distinta.
(define (maximo ar)
  (match ar
    [(nodo x _ (vacio)) x]
    [(nodo _ _ der) (maximo der)]))


;; Ejercicio 6b
(define (contiene? ar el)
  (error "Sin implementar"))

;; Ejercicio 6c
;; Función filtrar-arbol, recibe un árbol binario de búsqueda y un predicado, y devuelve un árbol que contiene
;; todos los elementos del original que cumplen con el predicado.
(define (filtrar-arbol arb pred?)
  (match arb
    [(vacio) (vacio)]
    [(nodo x izq der)
     (let ([new-izq (filtrar-arbol izq pred?)]
           [new-der (filtrar-arbol der pred?)])
       (if (pred? x)
           (nodo x new-izq new-der)
           (unir new-izq new-der)))]))

;; PUNTO EXTRA