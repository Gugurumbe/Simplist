;;Bienvenue dans ce test !
(defun factorielle (n)
  (cond
   ((<= n 0) 1)
   (t (* n (factorielle (- n 1))))
   )
  )
(if (= (factorielle 3) 6) (print "SUCCÈS") (print "ÉCHEC"))
