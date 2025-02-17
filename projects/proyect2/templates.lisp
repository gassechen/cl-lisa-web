(in-package :cl-lisa-web)

(deftemplate ambient-temperature ()
  (slot current-temperature)
  (slot desired-temperature)
  (slot location))

(deftemplate heater-state ()
  (slot state))
 
