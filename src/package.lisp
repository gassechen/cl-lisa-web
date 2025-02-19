(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (not (find-package :cl-lisa-web))
    (defpackage  #:cl-lisa-web
      (:use :easy-routes :spinneret :lisa-lisp ))))


