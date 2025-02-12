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
    ;; Crear el archivo facts.lisp
    (with-open-file (rules-file (merge-pathnames "facts.lisp" project-path)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
      (format rules-file ";; Facts for project ~a~%" project-name))
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
    ;; Crear el archivo functions.lisp
    (with-open-file (rules-file (merge-pathnames "functions.lisp" project-path)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
      (format rules-file ";; Functions for project ~a~%" project-name))
    project-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GET FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-facts-from-project (project-name)
  "Carga y devuelve el contenido del archivo 'facts.lisp' del proyecto especificado."
  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
         (templates-file-path (merge-pathnames "facts.lisp" project-path)))
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
        (format nil "Error: No facts file found for project '~a'." project-name))))


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


(defun get-rules-from-project (project-name)
  "Carga y devuelve el contenido del archivo 'rules.lisp' del proyecto especificado."
  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
         (rules-file-path (merge-pathnames "rules.lisp" project-path)))
    ;; Verificar si el archivo existe
    (if (uiop:file-exists-p rules-file-path)
        ;; Leer el contenido del archivo
        (with-open-file (stream rules-file-path
                                :element-type 'character)
          (let ((content (loop for line = (read-line stream nil)
                               while line
                               collect line)))
	     (format nil "<pre>~a</pre>" (format nil "~{~a~%~}" content))))
            
        ;; Si el archivo no existe, devolver un mensaje de error
        (format nil "Error: No rules file found for project '~a'." project-name))))



(defun get-functions-from-project (project-name)
  "Carga y devuelve el contenido del archivo 'functions.lisp' del proyecto especificado."
  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
         (rules-file-path (merge-pathnames "functions.lisp" project-path)))
    ;; Verificar si el archivo existe
    (if (uiop:file-exists-p rules-file-path)
        ;; Leer el contenido del archivo
        (with-open-file (stream rules-file-path
                                :element-type 'character)
          (let ((content (loop for line = (read-line stream nil)
                               while line
                               collect line)))
	     (format nil "<pre>~a</pre>" (format nil "~{~a~%~}" content))))
            
        ;; Si el archivo no existe, devolver un mensaje de error
        (format nil "Error: No functions file found for project '~a'." project-name))))




;;;;;;;;;;;;;;;;;;;;;;; SAVE FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-template-to-file (project-name template-body )
  "Guarda una nueva template en el archivo 'templates.lisp' del proyecto actual."
  (let* ((project-name project-name) ; Reemplaza esto con el nombre del proyecto actual
         (base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
         (templates-file-path (merge-pathnames "templates.lisp" project-path)))
    ;; Asegurarse de que el archivo exista
    (ensure-directories-exist templates-file-path)
    ;; Agregar la nueva regla al archivo
    (with-open-file (stream templates-file-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "~a ~%" template-body))))



(defun save-rule-to-file (project-name rule-condition )
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
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "~a~%" rule-condition ))))




(defun indent-lines (text)
  "Aplica una indentación de dos espacios a cada línea del texto."
  (mapcar (lambda (line)
            (if (> (length line) 0)
                (concatenate 'string "  " line)
                ""))
          (split-sequence:split-sequence #\Newline text)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EVAL FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eval-template (project-name )
  "Evalúa el contenido de una plantilla en el contexto del proyecto especificado."
  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
         (templates-file-path (merge-pathnames "templates.lisp" project-path)))
    ;; Asegurarse de que el archivo exista
    (unless (uiop:file-exists-p templates-file-path)
      (error "El archivo de plantillas no existe para el proyecto '~a'." project-name))
    ;; Leer el contenido del archivo
    (let ((template-content (uiop:read-file-string templates-file-path)))
      ;; Evaluar el contenido en un entorno seguro
      (safe-eval template-content))))


(defun safe-eval (code)
  "Evalúa el código en un entorno restringido para mayor seguridad."
  (let ((*package* (find-package :cl-lisa-web))) ; Restringir el paquete
    (eval (read-from-string code))))



;;;;;;;;;;;;;;;;;;;;;;;;;ROUTES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



;;;;;;;;;;;;;;;;;;;; LOAD ROUTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(easy-routes:defroute load-facts ("/api/facts/:project-name" :method :get)()
  "Carga la lista de facts."
  (setf (hunchentoot:content-type*) "text/html")
  (get-facts-from-project project-name))

(easy-routes:defroute load-templates ("/api/templates/:project-name" :method :get)()
  "Carga la lista de templates."
  (setf (hunchentoot:content-type*) "text/html")
  (get-templates-from-project project-name))

(easy-routes:defroute load-rules ("/api/rules/:project-name" :method :get)()
  "Carga la lista de reglas."
  (setf (hunchentoot:content-type*) "text/html")
  (get-rules-from-project project-name))

(easy-routes:defroute load-fucntions ("/api/fucntions/:project-name" :method :get)()
  "Carga la lista de fucntions."
  (setf (hunchentoot:content-type*) "text/html")
  (get-functions-from-project project-name))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SAVE ROUTES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(easy-routes:defroute save("/api/save-templates" :method :post) ()
  "Procesa los datos del formulario para agregar una nueva template."
  (let* ((params (hunchentoot:post-parameters*)) ; Obtener los parámetros POST
	 (project-name (cdr (assoc "projectName" params :test #'string=)))
         (template-body (cdr (assoc "content" params :test #'string=))))
     ; Validar que todos los campos estén presentes
    (if template-body
        ;; Guardar la regla y devolver un mensaje de éxito
        (progn
          (save-template-to-file project-name template-body)
          (spinneret:with-html-string
            (:div :class "uk-alert uk-alert-success" :data-uk-alert t
		  (:a :class "uk-alert-close"  :data-uk-close t)
                  (:p (format nil "Saved")))))
        ;; Si falta algún campo, devolver un mensaje de error
        (spinneret:with-html-string
          (:div :class "uk-alert uk-alert-danger" :data-uk-alert t
		(:a :class "uk-alert-close"  :data-uk-close t)
                (:p "Error: Todos los campos son obligatorios."))))))


(easy-routes:defroute save-rule ("/api/save-rule" :method :post) ()
  "Procesa los datos del formulario para agregar una nueva regla."
  (let* ((params (hunchentoot:post-parameters*)) ; Obtener los parámetros POST
	 (project-name (cdr (assoc "projectName" params :test #'string=)))
         (rule-condition (cdr (assoc "ruleBody" params :test #'string=))))
    ;; Validar que todos los campos estén presentes
    (if rule-condition
        ;; Guardar la regla y devolver un mensaje de éxito
        (progn
          (save-rule-to-file project-name rule-condition )
          (spinneret:with-html-string
            (:div :class "uk-alert uk-alert-success" :data-uk-alert t
		  (:a :class "uk-alert-close"  :data-uk-close t)
                  (:p ("Saved Rules")))))
        ;; Si falta algún campo, devolver un mensaje de error
        (spinneret:with-html-string
          (:div :class "uk-alert uk-alert-danger" :data-uk-alert t
		(:a :class "uk-alert-close"  :data-uk-close t)
                (:p "Error: Todos los campos son obligatorios."))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EVAL ROUTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(easy-routes:defroute api-eval-template ("/api/eval-template" :method :post) ()
  "Procesa los datos del formulario para evaluar una plantilla."
  (let* ((params (hunchentoot:post-parameters*)) ; Obtener los parámetros POST
         (project-name (cdr (assoc "projectName" params :test #'string=))))
    ;; Validar que todos los campos estén presentes
    (if project-name
        (handler-case
            ;; Intentar evaluar la plantilla
            (progn
              (eval-template project-name)
              (spinneret:with-html-string
                (:div :class "uk-alert uk-alert-success" :data-uk-alert t
                      (:a :class "uk-alert-close" :data-uk-close t)
                      (:p "La plantilla se ha evaluado correctamente."))))
          ;; Manejar errores durante la evaluación
          (error (e)
            (spinneret:with-html-string
              (:div :class "uk-alert uk-alert-danger" :data-uk-alert t
                    (:a :class "uk-alert-close" :data-uk-close t)
                    (:p (format nil "Error al evaluar la plantilla: ~a" e))))))
        ;; Si falta algún campo, devolver un mensaje de error
        (spinneret:with-html-string
          (:div :class "uk-alert uk-alert-danger" :data-uk-alert t
                (:a :class "uk-alert-close" :data-uk-close t)
                (:p "Error: Todos los campos son obligatorios."))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



