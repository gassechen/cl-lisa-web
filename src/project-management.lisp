(in-package :cl-lisa-web)

(defun render-project-management-page ()
  (spinneret:with-html-string
    (:doctype)
    (:html :lang "en"
      (:head
        (:meta :charset "UTF-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
        (:title "Project Management")
        (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/uikit@3.16.26/dist/css/uikit.min.css")
        (:script :src "https://unpkg.com/htmx.org@1.9.5"))
      (:body
        (:div :class "uk-container"
          (:h1 :class "uk-heading-primary" "Project Management")

          ;; Create New Project (HTMX Integration)
          (:div :class "uk-card uk-card-default uk-card-body"
            (:h3 :class "uk-card-title" "Create New Project")
            (:form :data-hx-post "/api/projects/new" :data-hx-target "#projects-list" :data-hx-swap "beforeend" :class "uk-form-stacked"
              (:div :class "uk-margin"
                (:label :class "uk-form-label" :for "project-name" "Project Name")
                (:div :class "uk-form-controls"
                  (:input :class "uk-input" :id "project-name" :name "projectName" :type "text" :placeholder "Example: Medical Diagnosis" :required t)))
              (:div :class "uk-margin"
                (:label :class "uk-form-label" :for "project-description" "Description")
                (:div :class "uk-form-controls"
                  (:textarea :class "uk-textarea" :id "project-description" :name "projectDescription" :rows "4" :placeholder "Describe the purpose of the project")))
              (:div :class "uk-margin"
                (:button :class "uk-button uk-button-primary" :type "submit" "Create Project"))))


	  ;; List of Projects (Dynamic Updates with HTMX)
	  (:div :class "uk-card uk-card-default uk-card-body"
		(:h3 :class "uk-card-title" "List of Projects")
		(:button :class "uk-button uk-button-default" :data-hx-get "/api/projects/list"
			 :data-hx-target "#projects-list"
			 :data-hx-swap "innerHTML" "Load Projects")
		(:div :id "projects-list"
		      (:div :class "uk-alert uk-alert-warning" :data-uk-alert t
			    (:p "No projects available. Create one!"))))

          ;; Button to go to System Execution
          (:div :class "uk-text-center"
            (:a :href "run-system" :class "uk-button uk-button-primary" "Go to System Execution")))

        ;; UIkit JS
        (:script :src "https://cdn.jsdelivr.net/npm/uikit@3.16.26/dist/js/uikit.min.js")
        (:script :src "https://cdn.jsdelivr.net/npm/uikit@3.16.26/dist/js/uikit-icons.min.js")))))
