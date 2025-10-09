(in-package :cl-lisa-web)

(defparameter *server* nil
  "Server instance (Hunchentoot acceptor).")

(defparameter *csp-header*  "frame-ancestors 'self'; form-action 'self'")

(defparameter *port* 8888
  "We can override it in the config file or from an environment variable.")

(defvar *static-files-directory* "/static/")

(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                :port (or port *port*)))
    
  (push (hunchentoot:create-folder-dispatcher-and-handler
       "/static/" (merge-pathnames "src/static/" 
                                   (asdf:system-source-directory :cl-lisa-web)))
  
	hunchentoot:*dispatch-table*)
  
  (hunchentoot:start *server*))

(defun stop-server ()
  (hunchentoot:stop *server*))

