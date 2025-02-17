(in-package :cl-lisa-web)
(in-package :cl-lisa-web)

(deftemplate ambient-temperature ()
  (slot current-temperature)
  (slot desired-temperature)
  (slot location))

(deftemplate heater-state ()
  (slot state))
 

(in-package :cl-lisa-web)

(defrule turn-on-heater ()
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
         

(in-package :cl-lisa-web)
;; Functions for project proyect2


         

(in-package :cl-lisa-web)
;; Functions for project proyect2
(assert (ambient-temperature (current-temperature 20)
                                (desired-temperature 22)
                                (location "room")))

         

         

