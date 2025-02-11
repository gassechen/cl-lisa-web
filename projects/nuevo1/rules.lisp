;; Rules for project nuevo1

(defrule pump-context::start-bomb ()
  (event (type start))
  =>
  (assert (pump (state on)))
  (format t "Regla: Encendiendo bomba~%"))

(defrule pump-context::stop-bomb ()
  (event (type stop))
  =>
  (assert (pump (state off)))
  (format t "Regla: Apagando bomba~%"))
(defrule pump-11 ()
  (a a)
  =>
  (b b))
  
  (defrule pump-sadasdas()
  (a a)
  =>
  (b b))
  
  
  (defrule pump-sadasdas()
  (a xxxxxa)
  =>
  (b b))




    

    
