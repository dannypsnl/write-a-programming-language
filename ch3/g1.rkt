#lang nanopass

(define (variable? x) (symbol? x))
(define (constant? x)
  (or (number? x)
      (char? x)
      (string? x)))
;;; quoted expression
(define (datum? x) #t)
(define (register? r)
  (member r '(r0 r1 r2 r3 r4 r5)))
(define-language ASM
  (terminals
   (variable (x))
   (datum (d))
   (constant (c))
   (register (r)))
  (Expr (e)
        x
        'd
        c)
  (Instruction (i)
               (load r e)
               (store r e)
               ; get value from r1, add it with e and store to r0
               (add r0 r1 e)
               (ret)
               (halt)))

(define-pass asm->string : (ASM Instruction) (i) -> * (string)
  (Instruction : Instruction (i) -> * (string)
               [(load ,r ,e)
                (format "LOAD ~a, ~a" r e)]
               [(store ,r ,e)
                (format "STORE ~a, ~a" r e)]
               [(add ,r0 ,r1 ,e)
                (format "ADD ~a, ~a, ~a" r0 r1 e)]
               [(ret) "ret"]
               [(halt) "halt"])
  (Instruction i))

(with-output-language (ASM Instruction)
  (define (build inst)
    (match inst
      [`(load ,r ,e)
       `(load ,r ,e)]
      [`(store ,r ,e)
       `(store ,r ,e)]
      [`(add ,r0 ,r1 ,e)
       `(add ,r0 ,r1 ,e)]
      [`(ret) `(ret)]
      [`(halt) `(halt)]
      [else (error 'wtf "unknown instruction")])))

(for ([s (map (λ (i) (asm->string (build i)))
              '((load r0 1)
                (add r0 r0 1)
                (store r1 r0)
                (halt)))])
  (displayln s))
