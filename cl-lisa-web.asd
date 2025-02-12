(asdf:defsystem "cl-lisa-web"
  :version "0.1"
  :author ""
  :license "WTFPL"
  :depends-on (:local-time
               :cl-ppcre
               :hunchentoot
               :easy-routes
               :alexandria
	       :spinneret
	       :spinneret/ps
	       :parenscript
	       :str
	       :Lisa
	       :swank)
  	       
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "routes")
		 (:file "project-management")
		 (:file "server")
		 (:file "system-execution")	 
		)))


    ;; To build a binary:
  :build-operation "program-op"
  :build-pathname "cl-lisa-web"
  :entry-point "cl-lisa-web::main"

  :description "A web aplication for lisa  "
  ;; :long-description
  ;; #.(read-file-string
  ;;    (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "cl-lisa-web-test"))))

;; Smaller binary.
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

