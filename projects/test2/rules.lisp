(in-package :cl-lisa-web)(defrule turn-on-heater ()
  (ambient-temperature (current-temperature ?ct) 
                       (desired-temperature ?dt)
                       (location ?loc))
  (test (equal ?loc "room"))
  =>
  (assert (heater-state (state on)))
  (print "Turning on the heater"))

(defrule turn-off-heater ()
  (ambient-temperature (current-temperature ?ct) 
                       (desired-temperature ?dt)
                       (location ?loc))
  (test (equal ?loc "room"))
  =>
  (assert (heater-state (state off)))
  (print "Turning off the heater"))
         
