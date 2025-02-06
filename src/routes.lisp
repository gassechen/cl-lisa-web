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


(easy-routes:defroute load-projects-list ("/api/projects/list" :method :get) ()
  "Devuelve una lista HTML con los nombres de los proyectos disponibles."
  (setf (hunchentoot:content-type*) "text/html")
  (let* ((base-path (merge-pathnames "projects/" (asdf:system-source-directory :cl-lisa-web)))
         (projects (uiop:directory-exists-p base-path))
	 (projects-sub-dir (uiop:subdirectories base-path))
         (project-names (when projects
                          (mapcar #'(lambda (name) 
			 (car (reverse (pathname-directory(parse-namestring  name))))) 
		     projects-sub-dir))))
    (if project-names
        ;; Generar una lista de proyectos con HTMX
        (with-output-to-string (s)
          (format s "<ul class='uk-list uk-list-divider'>")
          (dolist (project project-names)
            (format s "<li><a href='/system-execution.html?project=~a' hx-get='/api/load-project?name=~a' hx-target='#system-details'>~a</a></li>"
                    project project project))
          (format s "</ul>"))
        ;; Si no hay proyectos disponibles
        "<div class='uk-alert uk-alert-warning' data-uk-alert><p>No projects available. Create one!</p></div>")))




(defroute load-rules ("/api/rules" :method :get)
  "Carga la lista de reglas."
  (print "OK"))

(defroute add-rule-form ("/api/add-rule-form" :method :get)
  "Carga el formulario para agregar una nueva regla."
  (print "OK"))

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
