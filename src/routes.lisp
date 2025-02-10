(in-package :cl-lisa-web)

;; Definir las rutas de la API
(easy-routes:defroute load-projects ("/api/projects" :method :get)()
  (let ((html (render-project-management-page)))
    (setf (hunchentoot:content-type*) "text/html")
    (setf (hunchentoot:header-out "Content-Security-Policy") *csp-header*)
    (setf (hunchentoot:header-out "X-Frame-Options") "DENY")
    (setf (hunchentoot:header-out "X-Content-Type-Options") "nosniff")
    (format nil "~a" html)))
  

;; Ruta para crear un nuevo proyecto
(easy-routes:defroute create-project ("/api/projects/new" :method :post) ()
  "Crea un nuevo proyecto con el nombre proporcionado en el formulario."
  (setf (hunchentoot:content-type*) "text/html")
  (setf (hunchentoot:header-out "Content-Security-Policy") "default-src 'self'")
  (setf (hunchentoot:header-out "X-Frame-Options") "DENY")
  (setf (hunchentoot:header-out "X-Content-Type-Options") "nosniff")
  ;; Obtener los parámetros POST
  (let* ((params (hunchentoot:post-parameters*))
         (project-name (cdr (assoc "projectName" params :test #'string=))))
    (if project-name
        ;; Crear la carpeta y los archivos
        (let ((project-path (create-project-files project-name)))
          (format nil "Project '~a' created successfully at: ~a" project-name project-path))
        ;; Si no se proporciona un nombre de proyecto
        "Error: Project name is required.")))


(defun create-project-files (project-name)
  "Crea una carpeta con el nombre del proyecto dentro de la carpeta 'projects' y dos archivos dentro: rules.lisp y templates.lisp."
  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web))) ; Carpeta "projects" en la raíz del proyecto
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path)))
    ;; Crear la carpeta del proyecto si no existe
    (ensure-directories-exist project-path)
    ;; Crear el archivo rules.lisp
    (with-open-file (rules-file (merge-pathnames "rules.lisp" project-path)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
      (format rules-file ";; Rules for project ~a~%" project-name))
    ;; Crear el archivo templates.lisp
    (with-open-file (templates-file (merge-pathnames "templates.lisp" project-path)
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
      (format templates-file ";; Templates for project ~a~%" project-name))
    project-path))


(defun get-rules-from-project (project-name)
  "Carga y devuelve el contenido del archivo 'rules.lisp' del proyecto especificado."
  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
         (rules-file-path (merge-pathnames "rules.lisp" project-path)))
    ;; Verificar si el archivo existe
    (if (uiop:file-exists-p rules-file-path)
        ;; Leer el contenido del archivo
        (with-open-file (stream rules-file-path
                                :direction :input
                                :element-type 'character)
          (let ((content (loop for line = (read-line stream nil)
                               while line
                               collect line)))
	     (format nil "<pre>~a</pre>" (format nil "~{~a~%~}" content))))
            
        ;; Si el archivo no existe, devolver un mensaje de error
        (format nil "Error: No rules file found for project '~a'." project-name))))


(defun get-templates-from-project (project-name)
  "Carga y devuelve el contenido del archivo 'templates.lisp' del proyecto especificado."
  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
         (templates-file-path (merge-pathnames "templates.lisp" project-path)))
    ;; Verificar si el archivo existe
    (if (uiop:file-exists-p templates-file-path)
        ;; Leer el contenido del archivo
        (with-open-file (stream templates-file-path
                                :direction :input
                                :element-type 'character)
          (let ((content (loop for line = (read-line stream nil)
                               while line
                               collect line)))
	     (format nil "<pre>~a</pre>" (format nil "~{~a~%~}" content))))
            
        ;; Si el archivo no existe, devolver un mensaje de error
        (format nil "Error: No templates file found for project '~a'." project-name))))





(easy-routes:defroute load-projects-list ("/api/projects/list" :method :get) ()
  "Devuelve una lista HTML con los nombres de los proyectos disponibles."
  (setf (hunchentoot:content-type*) "text/html")
  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
       	 (projects (uiop:subdirectories base-path))
	 (project-names (when projects
                          (mapcar #'(lambda (name) 
				      (car (reverse (pathname-directory(parse-namestring  name))))) 
				  projects))))
    (if project-names
        ;; Generar una lista de proyectos con HTMX
	(spinneret:with-html-string
	  (:ul :class "uk-list uk-list-divider")
          (dolist (project project-names)
	    (:li (:button :class "uk-button uk-button-link" :type "button"
			  :data-hx-post (format nil "/api/projects/load/~A" project)
			  :data-hx-swap "outerHTML"
			  :data-hx-target "Body"
			  (format nil "~A" project))))))))




(easy-routes:defroute load-projects-load ("/api/projects/load/:x" :method :post) ()
  "Carga proyecto"
  (setf (hunchentoot:content-type*) "text/html")
  (setf (hunchentoot:header-out "Content-Security-Policy") "default-src 'self'")
  (setf (hunchentoot:header-out "X-Frame-Options") "DENY")
  (setf (hunchentoot:header-out "X-Content-Type-Options") "nosniff")
  ;; Obtener los parámetros POST
  (render-system-execution-page x))


(easy-routes:defroute load-rules ("/api/rules/:project-name" :method :get)()
  "Carga la lista de reglas."
  (setf (hunchentoot:content-type*) "text/html")
  (get-rules-from-project project-name))


(easy-routes:defroute load-templatess ("/api/templates/:project-name" :method :get)()
  "Carga la lista de templates."
  (setf (hunchentoot:content-type*) "text/html")
  (get-templates-from-project project-name))



(easy-routes:defroute add-rule ("/api/add-rule" :method :post) ()
  "Procesa los datos del formulario para agregar una nueva regla."
  (let* ((params (hunchentoot:post-parameters*)) ; Obtener los parámetros POST
	 (project-name (cdr (assoc "projectName" params :test #'string=)))
         (rule-name (cdr (assoc "ruleName" params :test #'string=)))
         (rule-condition (cdr (assoc "ruleCondition" params :test #'string=)))
         (rule-action (cdr (assoc "ruleAction" params :test #'string=))))
    ;; Validar que todos los campos estén presentes
    (if (and rule-name rule-condition rule-action)
        ;; Guardar la regla y devolver un mensaje de éxito
        (progn
          (save-rule-to-file project-name rule-name rule-condition rule-action)
          (spinneret:with-html-string
            (:div :class "uk-alert uk-alert-success" :data-uk-alert t
                  (:p (format nil "Regla '~a' agregada correctamente." rule-name)))))
        ;; Si falta algún campo, devolver un mensaje de error
        (spinneret:with-html-string
          (:div :class "uk-alert uk-alert-danger" :data-uk-alert t
                (:p "Error: Todos los campos son obligatorios."))))))



(defun save-rule-to-file (project-name rule-name rule-condition rule-action)
  "Guarda una nueva regla en el archivo 'rules.lisp' del proyecto actual."
  (let* ((project-name project-name) ; Reemplaza esto con el nombre del proyecto actual
         (base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
         (rules-file-path (merge-pathnames "rules.lisp" project-path)))
    ;; Asegurarse de que el archivo exista
    (ensure-directories-exist rules-file-path)
    ;; Agregar la nueva regla al archivo
    (with-open-file (stream rules-file-path
                            :direction :output
                            :if-exists :append
                            :if-does-not-exist :create)
      (format stream "(defrule ~a ()~%  ~a~%  =>~%  ~a)~%~%~%"
              rule-name rule-condition rule-action))))



(defroute save-rule ("/api/rules" :method :post)
  "Guarda una nueva regla."
  (print "OK"))

(defroute load-templates ("/api/templates" :method :get)
  "Carga la lista de plantillas."
  (print "OK"))

(defroute add-template-form ("/api/add-template-form" :method :get)
  "Carga el formulario para agregar una nueva plantilla."
  (print "OK"))

(defroute save-template ("/api/templates" :method :post)
  "Guarda una nueva plantilla."
  (print "OK"))


(easy-routes:defroute run-system ("/api/run-system" :method :get)()
  (let ((html (render-system-execution-page)))
    (setf (hunchentoot:content-type*) "text/html")
    ;;(setf (hunchentoot:header-out "Content-Security-Policy") *csp-header*)
    (setf (hunchentoot:header-out "X-Frame-Options") "DENY")
    (setf (hunchentoot:header-out "X-Content-Type-Options") "nosniff")
    (format nil "~a" html)))
