(in-package :cl-lisa-web)(deffacts start()
(ambient-temperature (current-temperature 20)
                                (desired-temperature 22)
                                (location "room")))
         
