#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; desugar :: CFSBWAE -> CFSBAE
(define (desugar expr)
  (type-case CFSBWAE expr
    [idS (i) (id i)]
    [numS (n) (num n)]
    [boolS (b) (bool b)]
    [strS (s) (str s)]
    [opS (oper arg) (op (map desugar arg))]
    [withS (bind body) (desugar bind body)]
    [with*S (bind body) (desugar bind body)]
    [funS (ids body) ((map desugar ids) (desugar body))]
    [appS (f args) ((desugar f) (map desugar args))]
    [iFS (c t e) ((desugar c) (desugar t) (desugar e))]
    [conDS (con els) (desugar-conds con els)]))

(define (desugar-operation oper arg)
  (make-op (desugar oper) (map desugar arg)))


(define (desugar-with bind body)
  (let ([ids (map car bind)])
    (if (contains-duplicates? ids)
        (error "Nombres de variables duplicados en el enlace")
        (withS (map (lambda (b) (make-binding (car b) (desugar (cdr b)))) bind)
               (desugar body)))))


(define (desugar-with-star bind body)
  (let ([ids (map car bind)])
    (if (contains-duplicates? ids)
        (error "Nombres de variables duplicados en el enlace")
        (with*S (map (lambda (b) (make-binding (car b) (desugar (cdr b)))) bind)
                (desugar body)))))


(define (contains-duplicates? lst)
  (not (= (length lst) (length (remove-duplicates lst)))))

(define (desugar-function ids body)
  (fun (map desugar ids) (desugar body)))

(define (desugar-application f args)
  (app (desugar f) (map desugar args)))

(define (desugar-if c t e)
  (iF (desugar c) (desugar t) (desugar e)))

(define (desugar-cond con els)
  (if (empty? con)
      (desugar els)
      (iF (desugar (caar con))
          (desugar (cadar con))
          (desugar-cond (cdr con) els))))


