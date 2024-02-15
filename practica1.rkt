#|
   Práctica Uno
   Integrantes:
     -
     -
     - Ortiz Castañeda José Ramón [318357115]
|#

#lang plai

;; Ejercicio1

;; area_total: Real Real -> Real
;; Calcula el área total de un cono circular recto.
;; La fórmula utilizada es A = πrg + πr^2, donde r es el radio de la base
;; y g es la generatriz de la base.
(define (area-total diametro generatriz)
  ;; radio: -> Real
  ;; Calcula y devuelve el radio a partir del diámetro.
  (define (radio)
    (/ diametro 2))

  ;; Calcula el área total utilizando la fórmula A = πrg + πr^2.
  (+ (* pi (radio) generatriz) (* pi (sqr (radio)))))



#|
 Ejercicio Cuatro
   Definir la función primer-letra la cual recibe una string, (cuyos elementos deben ser letras minúsculas),
   y devuelve ”Vocal“ si la primera letra es una vocal y ”Consonante“ si es una consonante.
 Desarrollo:
   Mediante let definimos una variable ‘primera’ con el carácter que esté en la posición cero de la cadena.
   Se obtiene este valor con la función string-ref, que devuelve el carácter en la posición k en str. Dentro de cond,
   se valida si ‘primera’ forma parte de la lista de vocales (#\a #\e #\i #\o #\u), de ser así entonces se devuelve la
   cadena “Vocal”, caso contrario estaríamos entrando al else de este apartado obteniendo así la cadena “Consonante”.
|#

(define (primera-letra str)
  (let* ([primera (string-ref str 0)])
    (cond 
      ((member primera'(#\a #\e #\i #\o #\u)) 
       "Vocal")
      (else  
       "Consonante"))))

#|
 Ejercicio Cinco
    Define la función par?, la cual recibirá un número y devolverá verdadero si el número recibido como parámetro
    es par, y falso en cualquier otro caso.
 Desarrollo:
    Con ayuda del operador remainder, determina el resto de dividir dos enteros. En este caso recibe como primer
    parámetro el entero de entrada, el segundo será dos tal que si es par el resto deberá ser cero, caso contrario
    uno. Para esto se realiza una validación con el operador equal?.

|#
(define (par? n)
  (equal? (remainder n 2) 0))

#|
 Ejercicio Seis
    Define la función impar?, la cual recibirá un número y devolverá verdadero si el número recibido como parámetro es
    impar y falso en cualquier otro caso.
 Desarrollo:
    De forma análoga al inciso anterior, se hará uso de remainder para validar si el número es impar. Se modifica la
    lógica tal que si el resto es uno, entonces se trata de un entero impar. 
|#
(define (impar? n)
  (equal? (remainder n 2) 1))


;; Ejercicio 8 

;; Definición de la función calculadora que realiza operaciones específicas según el argumento "operacion".
(define (calculadora operacion num1 num2)
  ;; Se utiliza cond para evaluar diferentes casos y realizar la operación correspondiente.
  (cond
    ;; Caso: "first". Devuelve el primer número.
    [(equal? operacion "first") num1]
    ;; Caso: "second". Devuelve el segundo número.
    [(equal? operacion "second") num2]
    ;; Caso: "sum". Devuelve la suma de los dos números.
    [(equal? operacion "sum") (+ num1 num2)]
    ;; Caso: "mul". Devuelve la multiplicación de los dos números.
    [(equal? operacion "mul") (* num1 num2)]
    ;; Caso: "div-exact". Devuelve el resultado de la división exacta si el divisor no es cero, de lo contrario, devuelve un mensaje de error.
    [(equal? operacion "div-exact")
     (if (not (= num2 0))
         (quotient num1 num2)
         "Error: División entre cero")]
    ;; Caso: "div". Devuelve el resultado de la división si el divisor no es cero, de lo contrario, devuelve un mensaje de error.
    [(equal? operacion "div")
     (if (not (= num2 0))
         (/ num1 num2)
         "Error: División entre cero")]
    ;; Caso por defecto: Si la operación no es reconocida, devuelve un mensaje de error.
    [else "Error: Operación no válida"]))
