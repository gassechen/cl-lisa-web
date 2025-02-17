(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package :cl-lisa-web))
    (defpackage  #:cl-lisa-web
      (:use :easy-routes :spinneret :lisa-lisp ))))



(defun heater-simulation ()
  ;; Initialize the system state
  (reset)
  (assert (ambient-temperature (current-temperature 20)
                                (desired-temperature 22)
                                (location "room")))
  (loop
    do
      ;; Execute the inference engine
      (run)

      ;; Add a new ambient-temperature fact
      (assert (ambient-temperature (current-temperature (+ 10 (random 20)))
                                    (desired-temperature 22)
                                    (location "room")))

      ;; Pause for 10 seconds
      (sleep 10)))
