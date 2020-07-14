(declare-fun f (Bool) Bool)
(assert (forall ((x Bool)) (!
    (f x)
    :pattern (f x)
	)))
(assert (not (f true)))
(check-sat)