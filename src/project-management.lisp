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
            (:script :src "https://unpkg.com/htmx.org@1.9.5")
	    (:script  :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.12/ace.js"))
	   (:body
            (:div :class "uk-container"
		  (:h1 :class "uk-heading-primary" "Project Management")

		  ;; Create New Project (HTMX Integration)
		  (:div :class "uk-card uk-card-default uk-card-body"
			(:h3 :class "uk-card-title" "Create New Project")
			(:form :data-hx-post "/api/projects/new"
			       :data-hx-target "#projects-list"
			       :data-hx-swap "beforeend"
			       :class "uk-form-stacked"
			       (:div :class "uk-margin"
				     (:label :class "uk-form-label" :for "project-name" "Project Name")
				     (:div :class "uk-form-controls"
					   (:input :class "uk-input" :id "project-name"
						   :name "projectName"
						   :type "text"
						   :placeholder "Example: Medical Diagnosis"
						   :required t)))
			       
			       (:div :class "uk-margin"
				     (:button :class "uk-button uk-button-primary" :type "submit" "Create Project"))))


		  ;; List of Projects (Dynamic Updates with HTMX)
		  (:div :class "uk-card uk-card-default uk-card-body"
			(:h3 :class "uk-card-title" "List of Projects")
			(:button :class "uk-button uk-button-default"
				 :data-hx-get "/api/projects/list"
				 :data-hx-target "#projects-list"
				 :data-hx-swap "innerHTML"
				 "Load Projects")
			(:div :id "projects-list")))

            ;; UIkit JS
            (:script :src "https://cdn.jsdelivr.net/npm/uikit@3.16.26/dist/js/uikit.min.js")
            (:script :src "https://cdn.jsdelivr.net/npm/uikit@3.16.26/dist/js/uikit-icons.min.js")))))
