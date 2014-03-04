(defmatche (contracto U U^)
  (((I ,X) ,X))
  ((((K ,X) ,Y) ,X))
  (((((S ,X) ,Y) ,Z) ((,X ,Z) (,Y ,Z)))))

(defmatche (->1wo T T^)
  ((,U ,U^) (contracto U U^))
  (((,V1 ,V2) (,V^ ,V2)) (->1wo V1 V^))
  (((,V1 ,V2) (,V1 ,V^)) (->1wo V2 V^)))

(defmatche (->wo T T^)
  ((,U ,U))
  ((,U ,V) (fresh (U^) (->1wo U U^) (->wo U^ V))))


(run 1 (Y) (eigen (x) (->wo `(,Y ,x) `(,x (,Y ,x)))))

; exists Y . forall x . Y x = x (Y x)
