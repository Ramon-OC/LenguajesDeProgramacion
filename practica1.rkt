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
|#

(define (primera-letra str)
  (let* ([primera (string-ref str 0)])
    (cond 
      ((member primera '(#\a #\e #\i #\o #\u)) 
       "Vocal")
      (else  
       "Consonante"))))

#|
 Ejercicio Cinco
    Define la función par?, la cual recibirá un número y devolverá verdadero si el número recibido como parámetro
    es par, y falso en cualquier otro caso.
 Desarrollo:
|#
(define (par? n)
  (equal? (remainder n 2) 0))

#|
 Ejercicio Seis
    Define la función impar?, la cual recibirá un número y devolverá verdadero si el número recibido como parámetro es
    impar y falso en cualquier otro caso.
 Desarrollo:
|#
(define (impar? n)
  (equal? (remainder n 2) 1))