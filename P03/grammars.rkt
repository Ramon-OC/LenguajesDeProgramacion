#lang plai

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