(load "pmatch.scm")
(load "mk.scm")
(load "test-check.scm")

;;; deterministic finite automaton (DFA) from:
;;; http://en.wikipedia.org/wiki/Deterministic_finite_automaton

;;; We can represent the transition function 'delta' as a table:
;;;
;;;     |  0    1
;;; --------------
;;; q0 |  q0   q1
;;; q1 |  q2   q0
;;; q2 |  q1   q2

(define fsm-ho
  (lambda (str)
    (letrec ([q1 (trace-lambda '1 (str)
                   (pmatch str
                     [() 'accept]
                     [(0 . ,d) (q1 d)]
                     [(1 . ,d) (q2 d)]))]
             [q2 (trace-lambda '2 (str)
                   (pmatch str
                     [() 'reject]
                     [(0 . ,d) (q3 d)]
                     [(1 . ,d) (q1 d)]))]
             [q3 (trace-lambda '3 (str)
                   (pmatch str
                     [() 'reject]
                     [(0 . ,d) (q2 d)]
                     [(1 . ,d) (q3 d)]))])
      (q1 str))))

(test "fsm-ho-1"
  (fsm-ho '(0 1 1))
  'accept)

(test "fsm-ho-2"
  (fsm-ho '(0 1 1 1))
  'reject)


; out argument, with explicit accept and reject values

(define simple-dfao
  (lambda (str out)
    (letrec ([q1 (lambda (str)
                   (conde
                     [(== '() str) (== 'accept out)]
                     [(fresh (a d)
                        (== `(,a . ,d) str)
                        (conde
                          [(== 0 a) (q1 d)]
                          [(== 1 a) (q2 d)]))]))]
             [q2 (lambda (str)
                   (conde
                     [(== '() str) (== 'reject out)]
                     [(fresh (a d)
                        (== `(,a . ,d) str)
                        (conde
                          [(== 0 a) (q3 d)]
                          [(== 1 a) (q1 d)]))]))]
             [q3 (lambda (str)
                   (conde
                     [(== '() str) (== 'reject out)]
                     [(fresh (a d)
                        (== `(,a . ,d) str)
                        (conde
                          [(== 0 a) (q2 d)]
                          [(== 1 a) (q3 d)]))]))])
      (q1 str))))

(test "simple-dfao-1"
  (run* (q) (simple-dfao '(0 1 1) q))
  '(accept))

(test "simple-dfao-2"
  (run* (q) (simple-dfao '(0 1 1 1) q))
  '(reject))

(test "simple-dfao-3"
  (run 10 (q) (simple-dfao q 'accept))
  '(()
    (0)
    (0 0)
    (1 1)
    (0 0 0)
    (1 1 0)
    (0 1 1)
    (1 0 0 1)    
    (0 0 0 0)
    (1 1 0 0)))

(test "simple-dfao-4"
  (run 10 (q) (simple-dfao q 'reject))
  '((1)
    (0 1)
    (1 0)
    (0 0 1)
    (0 1 0)
    (1 0 0)
    (1 1 1)
    (1 0 1)
    (0 0 0 1)
    (0 0 1 0)))

(test "simple-dfao-5"
  (run 10 (q)
    (fresh (str out)
      (simple-dfao str out)
      (== `(,str ,out) q)))
  '((() accept)
    ((0) accept)
    ((1) reject)
    ((0 0) accept)
    ((1 0) reject)
    ((0 1) reject)
    ((1 1) accept)
    ((0 0 0) accept)
    ((1 0 0) reject)
    ((0 1 0) reject)))

(test "simple-dfao-6"
  (run* (q) (simple-dfao `(0 ,q) 'accept))
  '(0))

(test "simple-dfao-7"
  (run 10 (q) (simple-dfao `(0 . ,q) 'accept))
  '(()
    (0)
    (0 0)
    (1 1)
    (0 0 0)
    (1 1 0)
    (0 1 1)
    (1 0 0 1)
    (0 0 0 0)
    (1 1 0 0)))

(test "simple-dfao-8"
  (run 10 (q) (simple-dfao `(1 . ,q) 'accept))
  '((1)
    (1 0)
    (0 0 1)
    (1 0 0)
    (1 1 1)
    (0 1 0 1)
    (0 0 1 0)
    (1 0 0 0)
    (1 1 1 0)
    (1 0 1 1)))

(test "simple-dfao-9"
  (run 10 (q)
    (fresh (b rest)
      (== `(,b 0 . ,rest) q)
      (simple-dfao q 'accept)))
  '((0 0)
    (0 0 0)
    (1 0 0 1)
    (0 0 0 0)
    (0 0 1 1)
    (1 0 1 0 1)
    (1 0 0 1 0)
    (0 0 0 0 0)
    (0 0 1 1 0)
    (0 0 0 1 1)))




; DFA D from p. 58 of Sipser (figure 1.44)

; accept = succeed
; reject = fail

(define Do
  (letrec ([q1 (lambda (str)
                 (conde
                   [(== '() str)]    ; accept state
                   [(fresh (a d)
                      (== `(,a . ,d) str)
                      (conde
                        [(== 'a a) (q1 d)]
                        [(== 'b a) (q4 d)]))]))]
           [q2 (lambda (str)
                 (conde          ; reject state
                   [(fresh (a d)
                      (== `(,a . ,d) str)
                      (conde
                        [(== 'a a) (q1 d)]
                        [(== 'd a) (q3 d)]))]))]
           [q3 (lambda (str)
                 (conde         ; reject state
                   [(fresh (a d)
                      (== `(,a . ,d) str)
                      (conde
                        [(== 'a a) (q3 d)]
                        [(== 'b a) (q3 d)]))]))]
           [q4 (lambda (str)
                 (conde         ; reject state
                   [(fresh (a d)
                      (== `(,a . ,d) str)
                      (conde
                        [(== 'a a) (q5 d)]
                        [(== 'b a) (q2 d)]))]))]
           [q5 (lambda (str)
                 (conde      ; reject state
                   [(fresh (a d)
                      (== `(,a . ,d) str)
                      (conde
                        [(== 'a a) (q6 d)]
                        [(== 'b a) (q2 d)]))]))]
           [q6 (lambda (str)
                 (conde
                   [(== '() str)]  ; accept state
                   [(fresh (a d)
                      (== `(,a . ,d) str)
                      (conde
                        [(== 'a a) (q6 d)]
                        [(== 'b a) (q5 d)]))]))])
    q1))


(test "Do-1"
  (run* (q) (Do '()))
  '(_.0))

(test "Do-2"
  (run* (q) (Do '(a)))
  '(_.0))

(test "Do-3"
  (run* (q) (Do '(b a a)))
  '(_.0))

(test "Do-5"
  (run* (q) (Do '(b)))
  '())

(test "Do-6"
  (run* (q) (Do '(b b)))
  '())

(test "Do-7"
  (run* (q) (Do '(b a b b a)))
  '())

(test "Do-8"
  (run* (q) (Do '(b a b a)))
  '(_.0))

(test "Do-9"
  (run 20 (q) (Do q))
  '(()
    (a)
    (a a)
    (b a a)
    (b b a)
    (a a a)
    (a b a a)
    (b a b a)
    (a b b a)
    (b a a a)
    (b b a a)
    (a a a a)
    (a a b a a)
    (a b a b a)
    (b a a b a)
    (a a b b a)
    (a b a a a)
    (b a b a a)
    (a b b a a)
    (b a a a a)))



;; NFA N4 from p. 53 of Sipser 3rd ed

(define N4o
  (letrec ([q1 (lambda (str)
                 (conde
                   [(== '() str)]       ; accept state
                   [(q3 str)]           ; epsilon transition
                   [(fresh (a d)
                      (== `(,a . ,d) str)
                      (conde
                        [(== 'b a) (q2 d)]))]))]
           [q2 (lambda (str)
                 (conde                  ; reject state     
                   [(fresh (a d)
                      (== `(,a . ,d) str)
                      (conde
                        [(== 'a a)
                         (conde         ; non-deterministic choice
                           [(q2 d)]
                           [(q3 d)])]
                        [(== 'b a) (q3 d)]))]))]
           [q3 (lambda (str)
                 (fresh (a d) ; reject state
                   (== `(,a . ,d) str)
                   (== 'a a) (q1 d)))])
    q1))

(test "N4o-1"
  (run* (q) (N4o '()))
  '(_.0))

(test "N4o-2"
  (run* (q) (N4o '(a)))
  '(_.0))

(test "N4o-3"
  (run* (q) (N4o '(b a a)))
  '(_.0))

(test "N4o-5"
  (run* (q) (N4o '(b)))
  '())

(test "N4o-6"
  (run* (q) (N4o '(b b)))
  '())

(test "N4o-7"
  (run* (q) (N4o '(b a b b a)))
  '())

(test "N4o-8"
  (run* (q) (N4o '(b a b a)))
  '(_.0))

(test "N4o-9"
  (run 20 (q) (N4o q))
  '(()
    (a)
    (a a)
    (b b a)
    (a a a)
    (b a a)
    (b b a a)
    (a b b a)
    (a a a a)
    (b a a a)
    (a b a a)
    (b a b a)
    (b b a a a)
    (a b b a a)
    (a a b b a)
    (b b a b b a)
    (b a a a)
    (a a a a a)
    (b a a a a)
    (a b a a a)))




(test "N4o-Do-1"
  (let ((n 200))
    (let loop ((ans (run n (q) (N4o q))))
      (cond
        [(null? ans) #t]
        [else
         (and (not (null? (run 1 (q) (Do q) (== (car ans) q))))
              (loop (cdr ans)))])))
  #t)

(test "Do-N4o-1"
  (let ((n 200))
    (let loop ((ans (run n (q) (Do q))))
      (cond
        [(null? ans) #t]
        [else
         (and (not (null? (run 1 (q) (N4o q) (== (car ans) q))))
              (loop (cdr ans)))])))
  #t)
