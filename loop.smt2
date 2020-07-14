(set-option :smt.MBQI false)
(declare-fun f (Int) Int)
(assert (forall ((i Int)) (!
  (= (f (+ i 1)) (f (f i))) :pattern ((f i)))))
(assert (= (f 0) 1))
(check-sat)
(get-model)