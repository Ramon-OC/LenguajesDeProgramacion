#lang plai

;; Práctica 3, ejercicio 2.

(require "grammars.rkt")

;; Parametros:
;;  - s-expr: number ó symbol ó string ó list
;; Tipo del valor de retorno: WAE


;; --------------------------------- PARSER ---------------------------------


;; Función principal para analizar sintácticamente una expresión
(define (parse s-expr)
  (cond
    [(number? s-expr) (num s-expr)] ;; Si es un número, lo representamos como una hoja numérica
    [(equal? s-expr 'true) (bool #t)] ;; Si es el símbolo 'true, lo representamos como una hoja booleana true
    [(equal? s-expr 'false) (bool #f)] ;; Si es el símbolo 'false, lo representamos como una hoja booleana false
    [(symbol? s-expr) (id s-expr)] ;; Si es un símbolo, lo representamos como una hoja identificadora
    [(string? s-expr) (str s-expr)] ;; Si es una cadena de caracteres, lo representamos como una hoja de cadena
    [else (let ([cabeza (car s-expr)])
            (match cabeza
              ['+ (parse-operacion + (rest s-expr))] ;; Si la cabeza es '+, llamamos a la función parse-operacion con la operación de suma
              ['- (parse-operacion - (rest s-expr))] ;; Si la cabeza es '-, llamamos a la función parse-operacion con la operación de resta
              ['and (parse-operacion my-and (rest s-expr))] ;; Si la cabeza es 'and, llamamos a la función parse-operacion con el operador lógico and
              ['or (parse-operacion my-or (rest s-expr))] ;; Si la cabeza es 'or, llamamos a la función parse-operacion con el operador lógico or
              ['not (parse-unaria noT (rest s-expr))] ;; Si la cabeza es 'not, llamamos a la función parse-unaria con el operador unario not
              ['sub1 (parse-sub1 (rest s-expr))] ;; Si la cabeza es 'sub1, llamamos a la función parse-sub1 con los argumentos
              ['with (parse-with s-expr)] ;; Si la cabeza es 'with, llamamos a la función parse-with
              ['with* (parse-with* s-expr)] ;; Si la cabeza es 'with*, llamamos a la función parse-with*
              ['num? (parse-operacion number? (rest s-expr))] ;; Si encontramos 'num?, llamamos a la función parse-operacion con el operador number? y los argumentos restantes
              ['modulo (parse-operacion modulo (rest s-expr))] ;; Si encontramos 'modulo, llamamos a la función parse-operacion con el operador modulo y los argumentos restantes
              ['min (parse-operacion min (rest s-expr))] ;; Si encontramos 'min, llamamos a la función parse-operacion con el operador min y los argumentos restantes
              [else (parse-list s-expr)]))])) ;; Por defecto, llamamos a la función parse-list para manejar otras operaciones desconocidas




;; --------------------------------- FUNCIONES DEFINIDAS EN EL LABORATORIO ---------------------------------



;; Definimos el tipo de datos algebraicos WAE para expresiones de un lenguaje de expresiones aritmético-lógicas
(define-type WAE
  [num (literal number?)] ;; Hojas que representan números
  [id (literal symbol?)] ;; Hojas que representan identificadores
  [bool (literal boolean?)] ;; Hojas que representan booleanos
  [str (literal string?)] ;; Hojas que representan cadenas de caracteres
  [noT (valor WAE?)] ;; Operación unaria not
  [op (operacion procedure?) (izq WAE?) (der WAE?)] ;; Operaciones binarias
  [with (id symbol?) (value WAE?) (body WAE?)] ;; Expresiones with para asignación de variables
  [with* (bindings (listof Binding?)) (body WAE?)]) ;; Expresiones with* para asignación múltiple de variables

;; Definimos el tipo de datos algebraicos Binding para representar los bindings en las expresiones with*
(define-type Binding
  [binding (id symbol?) (value WAE?)]) ;; Un binding consta de un identificador y una expresión

;; Función auxiliar para aplicar un operador lógico and a una lista de argumentos
(define (my-and . args)
  (andmap (lambda (x) x) args))

;; Función auxiliar para aplicar un operador lógico or a una lista de argumentos
(define (my-or . args)
  (ormap (lambda (x) x) args))

;; Función auxiliar para convertir una lista de binding en una lista de estructuras Binding
(define (list->binding b)
  (binding (first b) (parse (second b))))




;; --------------------------------- FUNCIONES AUXILIARES NECESARIAS ---------------------------------



;; Función para analizar la operación sub1
(define (parse-sub1 args)
  (if (= (length args) 1) ;; Si la aridad es 1
      (list 'op 'sub1 (parse (car args))) ;; Construir una lista que representa la operación sub1 con el argumento parseado
      (error 'parse "La aridad de sub1 debe ser 1, se dieron ~a argumentos" (length args)))) ;; Si no, producir un error


;; Función para analizar operaciones desconocidas que se representan como listas
(define (parse-list s-expr)
  (if (not (list? s-expr)) ;; Si no es una lista
      (error 'parse "Se esperaba una lista para la operación desconocida") ;; Producir un error
      (let ([op (car s-expr)] ;; Extraer el operador de la lista
            [args (rest s-expr)]) ;; Extraer los argumentos de la lista
        (if (not (list? args)) ;; Si los argumentos no son una lista
            (error 'parse "La lista de argumentos debe tener al menos un elemento") ;; Producir un error
            (parse-operator op args))))) ;; Llamar a la función para manejar la operación y los argumentos


;; Función para manejar operaciones específicas
(define (parse-operator op args)
  (cond
    ;; Operaciones binarias aritméticas
    [(member op '(+ - * / modulo min max expt sqrt < > <= >= =))
     (parse-operacion op args)]
    ;; Operaciones unarias y lógicas
    [(member op '(sub1 add1 not and or zero? num? str? bool?))
     (parse-unaria op args)]
    ;; Operaciones de cadena
    [(member op '(str-length str-first str-last))
     (parse-string op args)]
    [else (error 'parse "Operador desconocido: ~a" op)])) ;; Si el operador es desconocido, producir un error


;; Función auxiliar para analizar operaciones binarias como suma, resta, and y or
(define (parse-operacion operacion args)
  (if (not (>= (length args) 2))
      (error 'parse "La aridad de ~a debe ser al menos 2, se dieron ~a argumentos" operacion (length args))
      (op operacion (parse (first args)) (parse (second args)))))


;; Función auxiliar para analizar operaciones unarias como not
(define (parse-unaria operacion args)
  (if (not (= (length args) 1))
      (error 'parse "La aridad de ~a debe ser 1, se dieron ~a argumentos" operacion (length args))
      (operacion (parse (first args)))))


;; Función auxiliar para analizar operaciones de cadena
(define (parse-string op args)
  (if (not (>= (length args) 1))
      (error 'parse "La aridad de ~a debe ser al menos 1, se dieron ~a argumentos" op (length args))
      (op (parse (first args))))) ;; Aplicar la operación a los argumentos parseados


;; Función para analizar expresiones with
(define (parse-with s-expr)
  ;; Se extraen los bindings y el cuerpo de la expresión
  (let ([bindings (second s-expr)]
        [body (third s-expr)])
    ;; Se verifica si los bindings son una lista
    (if (not (list? bindings))
        (error 'parse "Los bindings deben ser una lista") ;; Se produce un error si los bindings no son una lista
        (let ([binding-ids (list)]) ;; Se inicializa una lista vacía para almacenar los identificadores de los bindings
          ;; Se define una función auxiliar para procesar los bindings
          (let loop ([bindings bindings])
            ;; Si no hay más bindings, se parsea el cuerpo de la expresión
            (if (null? bindings)
                (parse body)
                ;; Si aún hay bindings por procesar
                (let ([binding (car bindings)]) ;; Se extrae el primer binding
                  (let ([id (first binding)] ;; Se extrae el identificador del binding
                        [value (second binding)]) ;; Se extrae el valor asociado al identificador
                    (if (symbol? id) ;; Se verifica si el identificador es un símbolo
                        (if (member id binding-ids) ; Se verifica si el identificador ya ha sido utilizado
                            (error 'parse "No se pueden declarar dos variables de ligado con el mismo identificador") ;; Se produce un error si el identificador ya ha sido utilizado
                            ;; Si el identificador no ha sido utilizado previamente
                            (begin
                              (set! binding-ids (cons id binding-ids)) ; Se agrega el identificador al conjunto de identificadores utilizados
                              (with id (parse value) (loop (cdr bindings))))) ;; Se parsea el valor asociado al identificador y se continúa con el análisis de los siguientes bindings
                        (error 'parse "El identificador del binding debe ser un símbolo")))))))))) ;; Se produce un error si el identificador del binding no es un símbolo


;; Función para analizar expresiones with*
(define (parse-with* s-expr)
  ;; Se extraen los bindings y el cuerpo de la expresión
  (let ([bindings (second s-expr)]
        [body (third s-expr)])
    ;; Se verifica si los bindings son una lista
    (if (not (list? bindings))
        (error 'parse "Los bindings deben ser una lista") ;; Se produce un error si los bindings no son una lista
        ;; Se define una función auxiliar para procesar los bindings
        (let loop ([bindings bindings]
                   [binding-ids '()]) ;; Lista para almacenar los identificadores de los bindings
          ;; Si no hay más bindings, se parsea el cuerpo de la expresión
          (if (null? bindings)
              (parse body)
              ;; Si aún hay bindings por procesar
              (let ([binding (car bindings)]) ;; Se extrae el primer binding
                (if (not (= (length binding) 2)) ;; Se verifica si el binding tiene exactamente dos elementos (identificador y valor asociado)
                    (error 'parse "El binding debe tener exactamente 2 elementos") ;; Se produce un error si el binding no tiene exactamente dos elementos
                    ;; Si el binding tiene exactamente dos elementos
                    (let ([id (first binding)] ;; Se extrae el identificador del binding
                          [value (second binding)]) ;; Se extrae el valor asociado al identificador
                      (if (symbol? id) ;; Se verifica si el identificador es un símbolo
                          (if (member id binding-ids) ;; Se verifica si el identificador ya está en uso
                              (error 'parse "No se pueden declarar dos variables de ligado con el mismo identificador") ;; Se produce un error si el identificador está duplicado
                              (begin
                                (set! binding-ids (cons id binding-ids)) ;; Se agrega el identificador a la lista de identificadores
                                (with id (parse value) (loop (cdr bindings) binding-ids)))) ;; Se parsea el valor asociado al identificador y se continúa con el análisis de los siguientes bindings
                          (error 'parse "El identificador del binding debe ser un símbolo"))))))))))





;; --------------------------------- EJEMPLOS DE USO ---------------------------------



(displayln (parse 2)) ;; Parseamos el número 2
(displayln (parse 'true)) ;; Parseamos el booleano true
(displayln (parse '"Hola")) ;; Parseamos la cadena "Hola"
(displayln (parse '(sub1 2))) ;; Parseamos la operación sub1 con el argumento 2

(displayln (parse 'x)) ;; Parseamos el símbolo 'x
(displayln (parse 10)) ;; Parseamos el número 10
(displayln (parse 'true)) ;; Parseamos el símbolo 'true

(displayln (parse 'false)) ;; Parseamos el símbolo 'false
(displayln (parse "Hola")) ;; Parseamos la cadena "Hola"
(displayln (parse '{sub1 1})) ;; Parseamos la operación 'sub1 con el argumento 1


;; Estos ejemplos arrojan errores en la aridad
;; Si se quiere probar, descomentarlos y observar el error que se arroja

;(displayln (parse '{sub1 1 2})) ;; Produce un error de aridad porque 'sub1 espera solo un argumento
;(displayln (parse '{num? 1})) ;; Produce un error de aridad porque 'num? espera solo un argumento
;(displayln (parse '{num? 2 1})) ;; Produce un error de aridad porque 'num? espera solo un argumento

(displayln (parse '{modulo 1 2})) ;; Parseamos la operación 'modulo con los argumentos 1 y 2
(displayln (parse '{modulo 1 2 3})) ;; Parseamos la operación 'modulo con los argumentos 1, 2 y 3
(displayln (parse '{min 1 3})) ;; Parseamos la operación 'min con los argumentos 1 y 3
;(displayln (parse '{min})) ;; Produce un error de aridad porque 'min espera al menos un argumento

(displayln (parse '{with {{x 10} {y 20}} x} )) ;; Parseamos la expresión 'with con los bindings {x 10} y {y 20}, luego evaluamos 'x

;; Descomentar lo siguiente para observar el error que se arroja
;(displayln (parse '{with* {{x 10} {x 20}} x})) ;; Produce un error porque hay un binding duplicado con el identificador 'x


