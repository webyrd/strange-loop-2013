(load "mk.scm")
(load "test-check.scm")

; out argument, with explicit accept and reject values

; fig 2.15  p. 115 of Sipser
;
;; {0^n 1^n | n >= 0}

(define simple-pdao
  (lambda (str)
    (letrec ([q1 (lambda (str stack)
                   (conde
                     [(== '() str)]
                     [(q2 str `($ . ,stack))]))] ; epsilon
             [q2 (lambda (str stack)
                   (fresh (a d rest)
                     (== `(,a . ,d) str)
                     (conde
                       [(== 0 a) (q2 d `(0 . ,stack))]
                       [(== 1 a) (== `(0 . ,rest) stack) (q3 d rest)])))]
             [q3 (lambda (str stack)
                   (conde
                     [(fresh (rest)     ; epsilon
                        (== `($ . ,rest) stack)
                        (q4 str rest))]
                     [(fresh (a d rest)
                        (== `(,a . ,d) str)
                        (== 1 a) (== `(0 . ,rest) stack) (q3 d rest))]))]
             [q4 (lambda (str stack)
                   (== '() str))])
      (q1 str '()))))

(test "simple-pdao-0a"
  (run* (q) (simple-pdao '(0 0 0 1 1)))
  '())

(test "simple-pdao-0c"
  (run* (q) (simple-pdao '(0 0 0 1 1 1)))
  '(_.0))

(test "simple-pdao-1"
  (run 10 (q) (simple-pdao q))
  '(()
    (0 1)
    (0 0 1 1)
    (0 0 0 1 1 1)
    (0 0 0 0 1 1 1 1)
    (0 0 0 0 0 1 1 1 1 1)
    (0 0 0 0 0 0 1 1 1 1 1 1)
    (0 0 0 0 0 0 0 1 1 1 1 1 1 1)
    (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
    (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)))




(define simple-explicit-pdao
  (lambda (str out)
    (letrec ([q1 (lambda (str stack)
                   (conde
                     [(== '() str) (== 'accept out)]
                     [(q2 str `($ . ,stack))]))] ; epsilon
             [q2 (lambda (str stack)
                   (conde
                     [(== '() str) (== 'reject out)]
                     [(fresh (a d rest)
                        (== `(,a . ,d) str)
                        (conde
                          [(== 0 a) (q2 d `(0 . ,stack))]
                          [(== 1 a) (== `(0 . ,rest) stack) (q3 d rest)]))]))]
             [q3 (lambda (str stack)
                   (conde
                    [(== '() str) (== 'reject out)]
                    [(fresh (rest)     ; epsilon
                       (== `($ . ,rest) stack)
                       (q4 str rest))]
                     [(fresh (a d rest)
                        (== `(,a . ,d) str)
                        (== 1 a) (== `(0 . ,rest) stack) (q3 d rest))]))]
             [q4 (lambda (str stack)
                   (fresh ()
                     (== '() str) (== 'accept out)))])
      (q1 str '()))))


(test "simple-explicit-pdao-0a"
  (run* (q) (simple-explicit-pdao '(0 0 0 1 1) 'accept))
  '())

(test "simple-explicit-pdao-0b"
  (run* (q) (simple-explicit-pdao '(0 0 0 1 1) q))
  '(reject))

(test "simple-explicit-pdao-0c"
  (run* (q) (simple-explicit-pdao '(0 0 0 1 1 1) 'accept))
  '(_.0))

(test "simple-explicit-pdao-0d"
  (run* (q) (simple-explicit-pdao '(0 0 0 1 1 1) q))
  '(reject accept))

(test "simple-explicit-pdao-1"
  (run 10 (q) (simple-explicit-pdao q 'accept))
  '(()
    (0 1)
    (0 0 1 1)
    (0 0 0 1 1 1)
    (0 0 0 0 1 1 1 1)
    (0 0 0 0 0 1 1 1 1 1)
    (0 0 0 0 0 0 1 1 1 1 1 1)
    (0 0 0 0 0 0 0 1 1 1 1 1 1 1)
    (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)
    (0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1)))

(test "simple-explicit-pdao-2"
  (run 10 (q) (simple-explicit-pdao q 'reject))
  '(()
    (0)
    (0 0)
    (0 1)
    (0 0 0)
    (0 0 1)
    (0 0 0 0)
    (0 0 1 1)
    (0 0 0 1)
    (0 0 0 0 0)))
