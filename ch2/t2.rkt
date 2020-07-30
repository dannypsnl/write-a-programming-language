#lang racket

;;; Environment
(define (lookup/type-of env v)
  (hash-ref env v))
(define (extend/env env v t)
  (hash-set env v t))

(define (occurs v t)
  (match* (v t)
    [(v `(-> (,t1* ...) ,t2))
     (or (ormap (λ (t1) (occurs v t1)) t1*)
         (occurs v t2))]
    ((v `(_ ,type-param*))
     (ormap (λ (t) (occurs v t))
            type-param*))
    ((v t) (equal? v t))))

(define (unify t1 t2)
  (match* (t1 t2)
    [(`(-> (,p1* ...) ,r1) `(-> (,p2* ...) ,r2))
     (for-each (λ (p1 p2) (unify p1 p2))
               p1* p2*)
     (unify r1 r2)]
    [(_ t2) #:when (parameter? t2)
            (if (or (eqv? t1 (t2)) (not (occurs (t2) t1)))
                (t2 t1)
                (error (format "~a occurs in ~a" (t2) t1)))]
    [(t1 _) #:when (parameter? t1)
            (unify t2 t1)]
    [(`(,a ,a*) `(,b ,b*))
     #:when (eqv? a b)
     (for-each unify a* b*)]
    [(_ _)
     (unless (eqv? t1 t2)
       (error (format "cannot unify type ~a and ~a" t1 t2)))]))

;;; infer should take a term and produce a type
(define (recur-infer tm [env (make-immutable-hash)])
  (match tm
    [`(λ (,x* ...) ,t)
     (let ([λ-env (foldl (λ (x e)
                           (extend/env e x (make-parameter (gensym))))
                         env x*)])
       `(-> ,(map (λ (x) (lookup/type-of λ-env x)) x*)
            ,(recur-infer t λ-env)))]
    [`(let ([,x* ,xt*] ...) ,t)
     (let ([let-env (foldl (λ (x t e)
                             (extend/env e x (recur-infer t e)))
                           env
                           x* xt*)])
       (recur-infer t let-env))]
    [`(app ,f ,arg* ...)
     (let ([ft (recur-infer f env)]
           [argt* (map (λ (arg) (recur-infer arg env)) arg*)]
           [free (make-parameter (gensym))])
       (unify ft `(-> ,argt* ,free))
       free)]
    [`,x
     (cond
       [(string? x) 'string]
       [(number? x) 'number]
       [(symbol? x) (lookup/type-of env x)]
       [(list? x)
        `(list ,(if (empty? x)
                    (make-parameter (gensym))
                    (let ([et (recur-infer (car x) env)])
                      (for-each (λ (et*) (unify et* et))
                                (map (λ (x) (recur-infer x env)) (cdr x)))
                      et)))])]))

(define (elim-free ty)
  (match ty
    [`(-> (,p* ...) ,r)
     `(-> ,(map elim-free p*) ,(elim-free r))]
    [`(,a ,ty-arg* ...)
     (cons (elim-free a) (map elim-free ty-arg*))]
    [ty #:when (parameter? ty)
        (elim-free (ty))]
    [ty ty]))

(define (infer tm) (elim-free (recur-infer tm)))

(infer '(1 2))
(infer '(λ (x y) x))
(infer '(let ([x 1]) x))
(infer '(let ([y (λ (x y) x)]) (app y 1 2)))
