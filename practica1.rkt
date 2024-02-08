#|
   Práctica Uno
   Integrantes:
     -
     -
     - Ortiz Castañeda José Ramón [318357115]
|#

#lang plai

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