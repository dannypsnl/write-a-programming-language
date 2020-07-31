#lang racket

;;; Environment
(define (lookup/type-of env v)
  (hash-ref env v))
(define (extend/env env v t)
  (hash-set env v t))

(define (occurs v t)
  (match* (v t)
    [(v `(,t* ...))
     (ormap (λ (t) (occurs v t)) t*)]
    ((v t) (equal? v t))))

(define (unify t1 t2)
  (match* (t1 t2)
    [(_ t2) #:when (parameter? t2)
            (if (or (eqv? t1 (t2)) (not (occurs (t2) t1)))
                (t2 t1)
                (error (format "~a occurs in ~a" (t2) t1)))]
    [(t1 _) #:when (parameter? t1)
            (unify t2 t1)]
    [(`(,a* ...) `(,b* ...))
     (for-each unify a* b*)]
    [(_ _)
     (unless (eqv? t1 t2)
       (error (format "cannot unify type ~a and ~a" t1 t2)))]))

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
                           env x* xt*)])
       (recur-infer t let-env))]
    [`(quote ,p*)
     `(list ,(if (empty? p*)
                 (make-parameter (gensym))
                 (let ([et (recur-infer (car p*) env)])
                   (for-each (λ (et*) (unify et* et))
                             (map (λ (x) (recur-infer x env)) (cdr p*)))
                   et)))]
    [`(,f ,arg* ...)
     (let ([free (make-parameter (gensym))])
       (unify (recur-infer f env)
              `(-> ,(map (λ (arg) (recur-infer arg env)) arg*) ,free))
       free)]
    [x (cond
         [(string? x) 'string]
         [(number? x) 'number]
         [(char? x) 'char]
         [(symbol? x) (lookup/type-of env x)]
         [else (error (format "unknown form: ~a" x))])]))

(define (elim-free ty)
  (match ty
    [`(,ty* ...)
     (map elim-free ty*)]
    [ty (if (parameter? ty)
            (elim-free (ty))
            ty)]))

(define (infer tm) (elim-free (recur-infer tm)))

(infer '#\c)
(infer ''(1 2))
;;; id
(infer '(λ (x) x))
(infer '(let ([x 1]) x))
(infer '(let ([y (λ (x y) x)]) (y 1 2)))
(infer '((λ () (let ([x '("a" "b" "c")]) x))))
;;; const
(infer '(λ (x) (let ([foo (λ (y) x)]) foo)))
