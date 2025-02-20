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

	  (spinneret:with-html-string
            (:div :class "uk-alert uk-alert-success" :data-uk-alert t
                  (:a :class "uk-alert-close" :data-uk-close t)
                  (format nil "Project '~a' created successfully at: ~a" project-name project-path))))

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
                                :if-does-not-exist :create))
    ;; Crear el archivo rules.lisp
    (with-open-file (rules-file (merge-pathnames "rules.lisp" project-path)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create))
      
    ;; Crear el archivo templates.lisp
    (with-open-file (templates-file (merge-pathnames "templates.lisp" project-path)
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create))

    ;; Crear el archivo functions.lisp
    (with-open-file (rules-file (merge-pathnames "functions.lisp" project-path)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create))

    project-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GET FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-data-from-project (project-name resource)
  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
	 (res (concatenate 'string resource ".lisp"))
         (templates-file-path (merge-pathnames res project-path)))
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
        (format nil "Error: No file found for project '~a'." project-name))))

;;;;;;;;;;;;;;;;;;;;;;; SAVE FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-to-file (project-name template-body resource)
  (let* ((project-name project-name) ; Reemplaza esto con el nombre del proyecto actual
         (base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
	 (res (concatenate 'string resource ".lisp"))
         (templates-file-path (merge-pathnames res project-path)))
    ;; Asegurarse de que el archivo exista
    (ensure-directories-exist templates-file-path)
    ;; Agregar la nueva regla al archivo
    (with-open-file (stream templates-file-path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (format stream "~a ~%" template-body))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EVAL FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eval-form (project-name resource)
  "Evalúa el contenido de una plantilla en el contexto del proyecto especificado."
  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
	 (res (concatenate 'string resource ".lisp"))
         (templates-file-path (merge-pathnames res project-path)))
    ;; Asegurarse de que el archivo exista
    (unless (uiop:file-exists-p templates-file-path)
      (error "El archivo no existe para el proyecto '~a'." project-name))
    ;; Leer el contenido del archivo
    (let ((content (uiop:read-file-string templates-file-path)))
      ;; Evaluar el contenido en un entorno seguro
      (safe-eval content))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SAVE ROUTES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(easy-routes:defroute save ("/api/save/:resource" :method :post) ()
  "Procesa los datos del formulario para guardar un recurso específico."
  (let* ((params (hunchentoot:post-parameters*)) ; Obtener los parámetros POST
         (project-name (cdr (assoc "projectName" params :test #'string=)))
         (content (cdr (assoc "content" params :test #'string=))))
    ;; Validar que todos los campos estén presentes
    (if (and project-name content)
	(progn
	  (save-to-file project-name content resource)
	  (spinneret:with-html-string
                (:div :class "uk-alert uk-alert-success" :data-uk-alert t
                      (:a :class "uk-alert-close" :data-uk-close t)
                      (:p "Saved."))))
	;; Si falta algún campo, devolver un mensaje de error
        (spinneret:with-html-string
          (:div :class "uk-alert uk-alert-danger" :data-uk-alert t
                (:a :class "uk-alert-close" :data-uk-close t)
                (:p "Error: Todos los campos son obligatorios."))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EVAL ROUTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(easy-routes:defroute api-eval ("/api/eval/:resource" :method :post) ()
  "Procesa los datos del formulario para evaluar una plantilla."
  (let* ((params (hunchentoot:post-parameters*)) ; Obtener los parámetros POST
         (project-name (cdr (assoc "projectName" params :test #'string=))))
    ;; Validar que todos los campos estén presentes
    (if project-name
        (handler-case
            ;; Intentar evaluar la plantilla
            (progn
              (eval-form project-name resource)
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

(defun start-project (project-name)
  "Carga los archivos del proyecto, inicializa el motor de reglas y ejecuta el programa."
  (reset)
  (clear)
  (make-inference-engine)

  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (project-path (merge-pathnames (format nil "~a/" project-name) base-path))
         (rules-file (merge-pathnames "rules.lisp" project-path))
         (facts-file (merge-pathnames "facts.lisp" project-path))
         (functions-file (merge-pathnames "functions.lisp" project-path))
         (templates-file (merge-pathnames "templates.lisp" project-path)))
    ;; Validar que los archivos existan
    (unless (and (uiop:file-exists-p rules-file)
                 (uiop:file-exists-p facts-file)
                 (uiop:file-exists-p functions-file)
                 (uiop:file-exists-p templates-file))
      (error "Uno o más archivos requeridos no existen para el proyecto '~a'." project-name))
    ;; Cargar los archivos en el entorno
    (handler-case
        (progn
	  (load-file templates-file)
          (load-file rules-file)
	  (load-file facts-file)
          (load-file functions-file))
      ;; Manejar errores durante la ejecución
      (error (e)
        (error "Error al ejecutar el programa: ~a" e)))))


(defun load-file (file-path)
  "Carga un archivo Lisp en el entorno actual."
  (when (uiop:file-exists-p file-path)
    (load file-path)))


(defun run-project () 
  ;; Initialize the system state
  (trace-project)
  (run))


(defun trace-project()
  (watch :activations)
  (watch :facts)
  (watch :rules))


(easy-routes:defroute api-start-project ("/api/project/start/:project-name" :method :post) ()
  "Ejecuta el programa para el proyecto especificado."
  (handler-case
      (progn
        ;; Ejecutar el proyecto
        (start-project project-name)
        ;; Devolver un mensaje de éxito
        (spinneret:with-html-string
          (:div :class "uk-alert uk-alert-success" :data-uk-alert t
                (:a :class "uk-alert-close" :data-uk-close t)
                (:p (format nil "El programa se ha ejecutado correctamente para el proyecto '~a'." project-name)))))
    ;; Manejar errores durante la ejecución
    (error (e)
      (spinneret:with-html-string
        (:div :class "uk-alert uk-alert-danger" :data-uk-alert t
              (:a :class "uk-alert-close" :data-uk-close t)
              (:p (format nil "Error al ejecutar el programa: ~a" e)))))))



(easy-routes:defroute api-stop-project ("/api/project/stop/:project-name" :method :post) ()
  "Stop el programa para el proyecto especificado."
  (handler-case
      (progn
        ;; Ejecutar el proyecto
	(reset)
	(clear)
        (halt)
        ;; Devolver un mensaje de éxito
        (spinneret:with-html-string
          (:div :class "uk-alert uk-alert-success" :data-uk-alert t
                (:a :class "uk-alert-close" :data-uk-close t)
                (:p (format nil "El programa se ha detenido correctamente para el proyecto '~a'." project-name)))))
    ;; Manejar errores durante la ejecución
    (error (e)
      (spinneret:with-html-string
        (:div :class "uk-alert uk-alert-danger" :data-uk-alert t
              (:a :class "uk-alert-close" :data-uk-close t)
              (:p (format nil "Error al ejecutar el programa: ~a" e)))))))





(easy-routes:defroute api-run-project ("/api/project/run" :method :post) ()
  "Procesa los datos del formulario para evaluar una plantilla."
  (let* ((params (hunchentoot:post-parameters*))
         (content (cdr (assoc "content" params :test #'string=))))

    ;;(print content)
    (safe-eval content)

    (handler-case
	(progn
          ;; Ejecutar el proyecto
          (let ((output (with-output-to-string (*standard-output*)
                          (run-project))))
            ;; Devolver un mensaje de éxito
            (spinneret:with-html-string
              (:div (:textarea :id "output-area" :rows "10" :cols "80" :readonly t
                               (format nil "~a" output))))))
      ;; Manejar errores durante la ejecución
      (error (e)
	(spinneret:with-html-string
          (:div :class "uk-alert uk-alert-danger" :data-uk-alert t
		(:a :class "uk-alert-close" :data-uk-close t)
		(:p (format nil "Error al ejecutar el programa: ~a" e))))))))




