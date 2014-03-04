(load "mk.scm")
(load "test-check.scm")

(test "eigen test 1"
  (run* (q) (eigen (x) (== x q)))
  '())

(test "eigen test 2"
  (run 1 (q)  (eigen (x) (fresh (y) (== x y))))
  '(_.0))

(test "eigen test 3"
  (run 1 (q) (eigen (x) (fresh (y) (== x y) (== q y))))
  '())

(test "eigen test 4"
  (run 1 (q) (eigen (x) (fresh (y) (== x y) (== y q))))
  '())

(test "eigen test 5"
  (run 1 (q) (eigen (x) (fresh (y) (== `(,x ,x) y) (== y q))))
  '())

(test "eigen test 6"
  (let ((f (lambda (q x)
             (fresh (y) (== x y) (== y q)))))
    (run 1 (q) (eigen (x) (f q x))))
  '())

(test "eigen test 7"
  (run 1 (q) (eigen (x) (symbolo x)))
  '())

(test "eigen test 8"
  (run 1 (q) (eigen (x) (numbero x)))
  '())

(test "eigen test 9"
  (run 1 (q) (eigen (x) (absento x q)))
  '(_.0))

(test "eigen test 10"
  (run 1 (q) (eigen (x) (=/= x q)))
  '(_.0))

(test "eigen test 11"
  (run 1 (x) (eigen (e) (fresh (y) (== `(,y) x) (== y e))))
  '())

(test "eigen-test 12"
  (run 1 (q) (eigen (e1 e2) (=/= 5 e2)))
  '(_.0))

(test "eigen-test 13"
  (run 1 (q) (eigen (e1 e2) (=/= e1 e2)))
  '(_.0))
  
(test "eigen-numbero-1"
  (run 1 (q) (eigen (e) (numbero e)))
  '())

(test "eigen-numbero-2"
  (run 1 (q) (eigen (e) (fresh (x) (numbero x) (== x e))))
  '())

(test "eigen-numbero-3"
  (run 1 (q) (eigen (e) (fresh (x) (== x e) (numbero x))))
  '())

(test "eigen-numbero-4"
  (run 1 (q) (eigen (e) (fresh (x) (== x e))))
  '(_.0))

(test "eigen-symbolo"
  (run 1 (q) (eigen (e) (symbolo e)))
  '())

(test "eigen-==-1"
  (run 1 (q) (eigen (e1) (== e1 5)))
  '())

(test "eigen-==-2"
  (run 1 (q) (eigen (e1 e2) (== e1 e2)))
  '())

(test "eigen-==-3"
  (run 1 (q) (eigen (e1) (eigen (e2) (== e1 e2))))
  '())

(test "eigen-==-3b"
  (run 1 (q) (eigen (e1) (eigen (e2) (fresh (x y) (== x y) (== x e1) (== y e2)))))
  '())

(test "eigen-==-3c"
  (run 1 (q) (eigen (e1) (eigen (e2) (fresh (x y) (== x e1) (== x y) (== y e2)))))
  '())

(test "eigen-==-3d"
  (run 1 (q) (eigen (e1) (eigen (e2) (fresh (x y) (== x e1) (== y e2) (== x y)))))
  '())

(test "eigen-==-4"
  (run 1 (q)
    (eigen (e1)
      (fresh (x y)
        (== e1 `(,x . ,y)))))
  '())

(test "eigen-=/=-1"
  (run 1 (q) (eigen (e) (=/= q e)))
  '(_.0))

(test "eigen-=/=-2"
  (run 1 (q) (eigen (e) (fresh (x) (=/= q x))))
  '(_.0))

(test "eigen-=/=-3a"
  (run 1 (q) (eigen (e1 e2) (=/= e1 e2)))
  '(_.0))

(test "eigen-=/=-3b"
  (run 1 (q) (eigen (e1) (eigen (e2) (=/= e1 e2))))
  '(_.0))

(test "eigen-=/=-4"
  (run 1 (q) (eigen (e1) (=/= e1 e1)))
  '())

(test "eigen-=/=-5"
  (run 1 (q) (eigen (e1) (=/= e1 5)))
  '(_.0))

(test "eigen-simple-1"
  (run 1 (q) (eigen (x) (== q x)))
  '())

(test "eigen-simple-2"
  (run 1 (q) (eigen (x) (fresh (r) (== r x))))
  '(_.0))

(test "eigen-busted-1"
  (run 1 (x)
    (eigen (e)
      (fresh (y)
        (== `(,y) x)
        (== y e))))
  '())

(test "eigen-busted-2"
  (run 1 (x)
    (eigen (e)
      (fresh (y)
        (== y e)
        (== `(,y) x))))
  '())
