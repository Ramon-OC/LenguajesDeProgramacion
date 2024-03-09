#lang plai
#|
   Práctica Dos
   Integrantes:
     - Ocampo Tovar Fernando [317144451]
     - Rojo Mata Daniel [314297967] 
     - Ortiz Castañeda José Ramón [318357115]

|#

;; Ejercicio 1
;; La función mapea aplica la función func a cada elemento de la lista ls.
(define (mapea func ls)
  ;; Se utiliza cond para manejar casos específicos de la lista.
  (cond
    ; Si la lista es vacía, se devuelve una lista vacía.
    [(empty? ls) empty] 
    [else
     ;; Se utiliza cons para construir una nueva lista aplicando func al primer elemento
     ;; y recursivamente llamando a mapea para el resto de la lista.
     (cons (func (car ls)) (mapea func (cdr ls)))]
  )
)



;;Ejercicio 2
;;Función que dado un ínidice regresa el elemento en dicha posición
;;Se comprueba que sea un índice válido para proceder con la función
;;Se empieza a contar desde el 0
;;Se llama recursivamente a la función hasta que el indice sea 0 y regresar la cabeza de la lista restante
(define (get-by-index index ls)
  (cond
    [(or (>= index (long ls)) (< index 0)) "Indice inválido"]
    [(equal? index 0) (car ls)]
    [else
     (get-by-index (- index 1) (cdr ls))]))

;;Función auxliar para el ejercicio 2
;;Se cuenta el número de elementos en la lista
(define (long ls)
  (cond
    [(empty? ls) 0]
    [else
     (+ 1 (long (cdr ls)))]))


;; Ejercicio 3
;; Definición del tipo Figura que incluye dos variantes: cuadrado y círculo.
;; Un cuadrado se caracteriza por su lado, y un círculo por su diámetro.
(define-type Figura
  [cuadrado (lado number?)]
  [circulo (diametro number?)])



;; Ejercicio 4
;; Calcula el área de una figura, ya sea cuadrado o círculo.
(define (area figura)
  ;; Comprueba si la figura es del tipo Figura
  (if (Figura? figura)
      (type-case Figura figura
        ;; Área del cuadrado
        [cuadrado (cuadrado-lado) (expt cuadrado-lado 2)]
        ;; Área del círculo
        [circulo (circulo-diametro) (* pi (expt (/ circulo-diametro 2) 2))])
      ;; Error si no es una figura válida
      (error "No se recibió un tipo Figura"))) 



;; Ejercicio 5
;; Calcula el perímetro de una figura, ya sea cuadrado o círculo.
(define (perimetro figura)
   ;; Comprueba si la figura es del tipo Figura
  (if (Figura? figura)
      (type-case Figura figura
        ;; Perímetro del cuadrado
        [cuadrado (cuadrado-lado) (* cuadrado-lado 4)]
        ; Perímetro del círculo
        [circulo (circulo-diametro) (* pi circulo-diametro)])
      ; Error si no es una figura válida
      (error "No se recibió un tipo Figura")))



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


;;Ejercicio 6.b
;;Función que nos dice si un elemento está o no en el árbol
;;Se analiza el caso de ser un árbol vacío y se marca como falso en caso de serlo
;;Se checa que el valor del elemento sea mayor o menor al nodo raíz para saber por cuál camino ir
;;Se detiene hasta que el elemento sea igual a la raíz o no se encuentre
(define (contiene? ar el)
  (type-case ArbolDeBusqueda ar
    [vacio () false]
    [nodo (elemento izq der)
     (cond
       [(eq? elemento el) true]
       [(< el elemento) (contiene? izq el)]
       [(> el elemento) (contiene? der el)])]))

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
