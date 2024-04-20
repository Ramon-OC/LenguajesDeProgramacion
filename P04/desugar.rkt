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

(define (desugar-conds conds else)
  (if (empty? conds)
      empty
      (iF (desugar (car conds)) (desugar-conds (cdr conds) else) (desugar else))))

(define (desugar-withs bind body)
  (app (fun(map desugar (id-take bind)) (desugar body)) (map desugar (val-take bind))))

(define (id-take lis)
  (if (empty? lis)
      empty
      (cons (car (car lis))(id-take (cdr lis)))))

(define (val-take lis)
  (if (empty? lis)
      empty
      (cons (cdr (car lis)) (val-take (cdr lis)))))

