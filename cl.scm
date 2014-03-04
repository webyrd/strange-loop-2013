(load "mk.scm")
(load "test-check.scm")

;; SKI

;; weak-reducer for Combinatory Logic, from pages 24 and 25 of Hindley
;; and Seldin 'Lambda-Calculus and Combinators: An Introduction',
;; 2008, Cambridge University Press

(define contracto
;;; Contract an occurrence of a weak redex
  (lambda (U U^)
    (conde
      [(== `(I ,U^) U)]
      [(fresh (y)
         (== `((K ,U^) ,y) U))]
      [(fresh (x y z)
         (== `(((S ,x) ,y) ,z) U)
         (== `((,x ,z) (,y ,z)) U^))])))

(define ->1wo
;;; weakly contract U to U^ (a single weak reduction step)
  (lambda (U U^)
    (conde
      [(contracto U U^)]
      [(fresh (V1 V2 V^)
         (== `(,V1 ,V2) U)
         (conde
;; could end up with two possible reduction paths (Church-Rosser diamond property)           
           [(== `(,V^ ,V2) U^)
            (->1wo V1 V^)]
           [(== `(,V1 ,V^) U^)
            (->1wo V2 V^)]))])))

(define ->wo
;;; U (weakly) reduces to V if V is obtained by a finite number of weak contractions
  (lambda (U V)
    (conde
      [(== U V)]
      [(fresh (U^)
         (->1wo U U^)
         (->wo U^ V))])))



;; reverse conversion
;; http://en.wikipedia.org/wiki/Combinatory_logic#Reverse_conversion
(define Lo
  (lambda (T T^)
    (conde
      [(== 'I T) (== '(lambda (x) x) T^)]
      [(== 'K T) (== '(lambda (x) (lambda (y) x)) T^)]
      [(== 'S T) (== '(lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))) T^)]
      [(fresh (T1 T2 T1^ T2^)
         (== `(,T1 ,T2) T)
         (== `(,T1^ ,T2^) T^)
         (Lo T1 T1^)
         (Lo T2 T2^))])))

(define L-etao
; add inverse etas to the applications in S  
  (lambda (T T^)
    (conde
      [(== 'I T) (== '(lambda (x) x) T^)]
      [(== 'K T) (== '(lambda (x) (lambda (y) x)) T^)]
      [(== 'S T) (== '(lambda (x) (lambda (y) (lambda (z) (lambda (w) (((lambda (v) ((x z) v)) (lambda (v) ((y z) v))) w))))) T^)]
      [(fresh (T1 T2 T1^ T2^)
         (== `(,T1 ,T2) T)
         (== `(,T1^ ,T2^) T^)
         (L-etao T1 T1^)
         (L-etao T2 T2^))])))

; exercise 2.17b
(run 1 (W)
  (eigen (x y)
    (->wo `((,W ,x) ,y) `((,x ,y) ,y))))



(run 1 (I)
  (eigen (x)
    (->wo `(,I ,x) x)))



(test "Fixpoint-combinator-self-app-verify"
  (run 1 (Y)
    (fresh (?)
      (eigen (x)
        (== '(((S (S (K (S I)))) I)
              ((S (S (K (S I)))) I))
            Y)
        (== `(,? ,?) Y)
        (->wo `(,Y ,x) `(,x (,Y ,x))))))
  '((((S (S (K (S I)))) I)
     ((S (S (K (S I)))) I))))

(time (test "Fixpoint-combinator-self-app"
        (run 1 (Y)
          (fresh (?)
            (eigen (x)
              (== `(,? ,?) Y)
              (->wo `(,Y ,x) `(,x (,Y ,x))))))
        '((((S (S (K (S I)))) I)
           ((S (S (K (S I)))) I)))))

(test "Lo"
  (run* (q)
    (Lo
      '(((S (S (K (S I)))) I)
        ((S (S (K (S I)))) I))
      q))
  '(((((lambda (x)
         (lambda (y)
           (lambda (z)
             ((x z) (y z)))))
       ((lambda (x)
          (lambda (y)
            (lambda (z) ((x z) (y z)))))
        ((lambda (x)
           (lambda (y) x))
         ((lambda (x)
            (lambda (y)
              (lambda (z) ((x z) (y z)))))
          (lambda (x) x)))))
      (lambda (x) x))
     (((lambda (x)
         (lambda (y)
           (lambda (z) ((x z) (y z)))))
       ((lambda (x)
          (lambda (y)
            (lambda (z) ((x z) (y z)))))
        ((lambda (x)
           (lambda (y) x))
         ((lambda (x)
            (lambda (y)
              (lambda (z) ((x z) (y z)))))
          (lambda (x) x)))))
      (lambda (x) x)))))

#|
(let ((F (eval (car (run 1 (q)
                      (fresh (Y ?)
                        (eigen (X)
                          (== `(,? ,?) Y)
                          (->wo `(,Y ,X) `(,X (,Y ,X)))
                          (Lo Y q)))))
               (environment '(rnrs)))))
  ((F (lambda (f)
        (lambda (n)
          (if (= n 0)
              1
              (* n (f (- n 1)))))))
   5))
|#















(test "L-etao"
  (run* (q)
    (L-etao
     '(((S (S (K (S I)))) I)
       ((S (S (K (S I)))) I))
     q))
  '(((((lambda (x)
         (lambda (y)
           (lambda (z)
             (lambda (w)
               (((lambda (v) ((x z) v)) (lambda (v) ((y z) v)))
                w)))))
       ((lambda (x)
          (lambda (y)
            (lambda (z)
              (lambda (w)
                (((lambda (v) ((x z) v))
                  (lambda (v) ((y z) v)))
                 w)))))
        ((lambda (x) (lambda (y) x))
         ((lambda (x)
            (lambda (y)
              (lambda (z)
                (lambda (w)
                  (((lambda (v) ((x z) v))
                    (lambda (v) ((y z) v)))
                   w)))))
          (lambda (x) x)))))
      (lambda (x) x))
     (((lambda (x)
         (lambda (y)
           (lambda (z)
             (lambda (w)
               (((lambda (v) ((x z) v)) (lambda (v) ((y z) v)))
                w)))))
       ((lambda (x)
          (lambda (y)
            (lambda (z)
              (lambda (w)
                (((lambda (v) ((x z) v))
                  (lambda (v) ((y z) v)))
                 w)))))
        ((lambda (x) (lambda (y) x))
         ((lambda (x)
            (lambda (y)
              (lambda (z)
                (lambda (w)
                  (((lambda (v) ((x z) v))
                    (lambda (v) ((y z) v)))
                   w)))))
          (lambda (x) x)))))
      (lambda (x) x)))))

(test "Fixpoint-combinator-self-app-verify-L-etao-fact"
  (let ((F (eval (car (run 1 (q)
                        (fresh (Y ?)
                          (eigen (x)
                            (== `(,? ,?) Y)
                            (->wo `(,Y ,x) `(,x (,Y ,x)))
                            (L-etao Y q)))))
                 (environment '(rnrs)))))
    ((F (lambda (f)
          (lambda (n)
            (if (= n 0)
                1
                (* n (f (- n 1)))))))
     5))
  120)
