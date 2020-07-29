#lang racket

;;; Environment
(define (lookup/type-of env v)
  (hash-ref env v))
(define (extend/env env v t)
  (hash-set env v t))

(define (occurs v t)
  (match* (v t)
    [(v `(free ,_))
     (eqv? v t)]
    [(v `(-> (,t1* ...) ,t2))
     (or (ormap (λ (t1) (occurs v t1)) t1*)
         (occurs v t2))]
    ((v `(_ ,type-param*))
     (ormap (λ (t) (occurs v t))
            type-param*))
    ((_ _) false)))

(define (unify t1 t2 subst)
  (match* (t1 t2)
    [(`(-> (,p1* ...) ,r1) `(-> (,p2* ...) ,r2))
     (for-each (λ (p1 p2) (unify p1 p2 subst))
               p1*
               p2*)
     (unify r1 r2 subst)]
    [(_ `(free ,_))
     (let ([v t1]
           [t t2])
       (if (or (eqv? v t) (not (occurs v t)))
           (hash-set! subst t v)
           (void)))]
    [(`(free ,_) _) (unify t2 t1 subst)]
    [(`(,a ,a*) `(,b ,b*))
     #:when (eqv? a b)
     (for-each unify a* b*)]
    [(_ _)
     (unless (eqv? t1 t2)
       (error (format "cannot unify type ~a and ~a" t1 t2)))]))

;;; infer should take a term and produce a type
(define (recur-infer subst tm [env (make-immutable-hash)])
  (match tm
    [`(λ (,x* ...) ,t)
     (let ([λ-env (foldl (λ (x e)
                           (extend/env e x `(free ,(gensym))))
                         env
                         x*)])
       `(-> ,(map (λ (x) (lookup/type-of λ-env x)) x*)
            ,(recur-infer subst t λ-env)))]
    [`(let ([,x* ,xt*] ...) ,t)
     (let ([let-env (foldl (λ (x t e)
                             (extend/env e x (recur-infer subst t e)))
                           env
                           x* xt*)])
       (recur-infer subst t let-env))]
    [`(app ,f ,arg* ...)
     (let ([ft (recur-infer subst f env)]
           [argt* (map (λ (arg) (recur-infer subst arg env)) arg*)]
           [free `(free ,(gensym))])
       (unify ft
              `(-> ,argt* ,free)
              subst)
       free)]
    [`,x
     (cond
       [(string? x) 'string]
       [(number? x) 'number]
       [(symbol? x) (lookup/type-of env x)]
       [(list? x)
        `(list ,(if (empty? x)
                    `(free ,(gensym))
                    (let ([et (recur-infer subst (car x) env)])
                      (for-each (λ (et*)
                                  (unify et* et subst))
                                (map (λ (x) (recur-infer subst x env)) (cdr x)))
                      et)))])]))

(define (elim-free ty subst)
  (match ty
    [`(-> ,p* ... ,r)
     `(-> ,(map (λ (p) (elim-free p subst)) p*) ,(elim-free r subst))]
    [`(free ,x)
     (elim-free (hash-ref subst ty (λ () x))
                subst)]
    [`(,a ,ty-arg* ...)
     `(,(elim-free a subst)
       ,(map (λ (ty-arg) (elim-free ty-arg subst)) ty-arg*))]
    [`,x x]))

(define (infer tm [subst (make-hash)])
  (let ([ty (recur-infer subst tm)])
    (elim-free ty subst)))

(infer '(1 2))
(infer '(λ (x y) x))
(infer '(let ([x 1]) x))
(infer '(let ([y (λ (x y) x)]) (app y 1 2)))
