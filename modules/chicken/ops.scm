(import (chicken foreign) (chicken bitwise) (chicken syntax))

;; Maybe it's cleaner to put the bignums in an array of GC roots in C?
(define bignum-vector #f)
(define next-bignum-position 0)

(define settable-list) ; This might be set! to, defeating optimization

(define-external (chicken_bignum_initialize (int bignum-count)) void
  (set! bignum-vector (make-vector bignum-count 0))
  (set! next-bignum-position 0) )

(define-external (chicken_bignum_cleanup (int bignum)) void
  (vector-set! bignum-vector bignum 0) )

(define-external (chicken_bignum_shutdown) void (void))

(define-external (chicken_bignum_from_string ((const c-string) s)) int
  (let ((bignum (string->number s 10)))
    (vector-set! bignum-vector next-bignum-position bignum)
    (set! next-bignum-position (add1 next-bignum-position)) ) )

(define-external (chicken_string_from_bignum (int bignum)) c-string
  (number->string (vector-ref bignum-vector bignum)) )

(define-external (chicken_bignum_operation
                  (int op) (int a) (int b) (int c) (int d) (int opt)) int
 (cond ((and (<= 0 op (sub1 (vector-length operation-table)))
             (vector-ref operation-table op))
        => (lambda (operation)
             (operation a b c d (bit->boolean opt 1))))
       (else -1)))


(define-syntax make-op
  (ir-macro-transformer
   (lambda (e i c)
     (let* ((arg-count (cadr e))
            (op-args (let lp ((i 0)     ; "take" (but we avoid srfi-1)
                              (known-op-args '(b c d)))
                       (if (= i arg-count)
                           '()
                           (cons (car known-op-args)
                                 (lp (add1 i) (cdr known-op-args))))))
            (op (caddr e))
            (guard (if (null? (cdddr e)) `(lambda _ #t) (cadddr e))))
       ;; Shadow old args (indexes) with new ones (the actual bignums)
       `(lambda (a b c d apply?)
          (let (,@(map (lambda (arg)
                         `(,arg (vector-ref bignum-vector ,arg)))
                       op-args))
            (cond ((,guard ,@op-args)
                   (vector-set! bignum-vector a ; still an index
                                (if apply?
                                    (apply ,op (append (list ,@op-args)
                                                       settable-list))
                                    (,op ,@op-args)))
                   0)
                  (else -1))))))))

(define (second-nonzero? x y) (not (zero? y)))

(define (exp-mod-ok? b c d) (and (>= b 0) (>= c 0) (not (zero? d))))

(define BN_FUZZ_OP_ADD (make-op 2 +))
(define BN_FUZZ_OP_SUB (make-op 2 -))
(define BN_FUZZ_OP_MUL (make-op 2 *))
(define BN_FUZZ_OP_DIV (make-op 2 quotient second-nonzero?))
(define BN_FUZZ_OP_MOD (make-op 2 modulo second-nonzero?))

(define BN_FUZZ_OP_EXP_MOD
  (make-op 3 (lambda (base power mod)
               (modulo (expt base power) mod))
           exp-mod-ok?))

(define BN_FUZZ_OP_LSHIFT
  (make-op 1 (lambda (x) (arithmetic-shift x 1))))

(define BN_FUZZ_OP_RSHIFT
  (make-op 1 (lambda (x) (arithmetic-shift x -1))))

(define BN_FUZZ_OP_GCD (make-op 2 gcd))

(define BN_FUZZ_OP_CMP (make-op 2 (lambda (x y)
                                    (cond ((> x y) 1)
                                          ((< x y) -1)
                                          (else 0)))))

(define BN_FUZZ_OP_SQR (make-op 1 (lambda (x) (* x x))))
(define BN_FUZZ_OP_NEG (make-op 1 -))
(define BN_FUZZ_OP_ABS (make-op 1 abs))

(define operation-table
  (vector
   BN_FUZZ_OP_ADD
   BN_FUZZ_OP_SUB
   BN_FUZZ_OP_MUL
   BN_FUZZ_OP_DIV
   BN_FUZZ_OP_MOD
   BN_FUZZ_OP_EXP_MOD
   BN_FUZZ_OP_LSHIFT
   BN_FUZZ_OP_RSHIFT
   BN_FUZZ_OP_GCD
   #f ;BN_FUZZ_OP_MOD_ADD
   #f ;BN_FUZZ_OP_EXP
   BN_FUZZ_OP_CMP
   BN_FUZZ_OP_SQR
   BN_FUZZ_OP_NEG
   BN_FUZZ_OP_ABS
   ))
