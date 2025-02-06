(in-package :cl-lisa-web)

(defun render-system-execution-page (project-name)
  (spinneret:with-html-string
    (:doctype)
    (:html :lang "en"
	   (:head
            (:meta :charset "UTF-8")
            (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
            (:title "System Execution")
            (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/uikit@3.16.26/dist/css/uikit.min.css")
            (:script :src "https://unpkg.com/htmx.org@1.9.5"))
	   (:body
            (:div :class "uk-container"
		  (:h1 :class "uk-heading-primary" (format nil "System Execution ~A project" project-name))

		  ;; Control Panel (HTMX Integration)
		  (:div :class "uk-card uk-card-default uk-card-body"
			(:h3 :class "uk-card-title" "Control Panel")
			(:div :class "uk-grid-small" :data-uk-grid t
			      (:div :class "uk-width-auto"
				    (:button :id "start-system-btn" :class "uk-button uk-button-success"
					     (:span :class "status-indicator status-stopped" :id "status-indicator")
					     "Start System"))
			      (:div :class "uk-width-auto"
				    (:button :id "stop-system-btn" :class "uk-button uk-button-danger" :disabled t "Stop System"))
			      (:div :class "uk-width-auto"
				    (:button :id "debug-system-btn" :class "uk-button uk-button-secondary" :disabled t "Debug"))
			      (:div :class "uk-width-expand"
				    (:button :id "reset-system-btn" :class "uk-button uk-button-default" "Reset System"))))

		  ;; Console Output (Dynamic Updates with HTMX)
		  (:div :class "uk-card uk-card-default uk-card-body"
			(:h3 :class "uk-card-title" "Console Output")
			(:div :class "console-output" :id "console-output"
			      "System stopped. Press \"Start System\" to begin."))


		  ;; Rule Management (HTMX Integration)
		  (:div :class "uk-card uk-card-default uk-card-body"
			(:h3 :class "uk-card-title" "Rule Management")
			;; List of Rules (Dynamic Updates with HTMX)
			(:div :id "rules-list"
			      (:button :class "uk-button uk-button-default" :data-hx-get "/api/rules"
				       :data-hx-target "#rules-list"
				       :data-hx-swap "innerHTML" "Load Rules")
			      (:div :class "uk-alert uk-alert-warning" :data-uk-alert t
				    (:p "No rules defined.")))
			;; Add Rule Button (HTMX Integration)
			(:button :class "uk-button uk-button-secondary" :data-hx-get "/api/add-rule-form"
				 :data-hx-target "#rule-modal-content"
				 :data-hx-swap "innerHTML"
				 :data-uk-toggle "target: #rule-modal" "Add Rule"))

		  ;; Modal for Adding/Editing Rules
		  (:div :id "rule-modal" :data-uk-modal t
			(:div :class "uk-modal-dialog uk-modal-body"
			      (:h2 :class "uk-modal-title" "Add/Edit Rule")
			      (:div :id "rule-modal-content"
				    (:p "Loading rule form..."))))
		  ;; Template Management (HTMX Integration)
		  (:div :class "uk-card uk-card-default uk-card-body"
			(:h3 :class "uk-card-title" "Template Management")
			;; List of Templates (Dynamic Updates with HTMX)
			(:div :id "templates-list"
			      (:button :class "uk-button uk-button-default" :data-hx-get "/api/templates"
				       :data-hx-target "#templates-list"
				       :data-hx-swap "innerHTML" "Load Templates")
			      (:div :class "uk-alert uk-alert-warning" :data-uk-alert t
				    (:p "No templates defined.")))
			;; Add Template Button (HTMX Integration)
			(:button :class "uk-button uk-button-secondary" :data-hx-get "/api/add-template-form"
				 :data-hx-target "#template-modal-content"
				 :data-hx-swap "innerHTML"
				 :data-uk-toggle "target: #template-modal" "Add Template"))

		  ;; Modal for Adding/Editing Templates
		  (:div :id "template-modal" :data-uk-modal t
			(:div :class "uk-modal-dialog uk-modal-body"
			      (:h2 :class "uk-modal-title" "Add/Edit Template")
			      (:div :id "template-modal-content"
				    (:p "Loading template form..."))))

		  ;; System Execution (HTMX Integration)
		  (:div :class "uk-card uk-card-default uk-card-body"
			(:h3 :class "uk-card-title" "Run System")
			(:form :data-hx-post "/api/run-system" :data-hx-target "#console-output" :data-hx-swap "innerHTML" :class "uk-form-stacked"
			       (:div :class "uk-margin"
				     (:label :class "uk-form-label" :for "input-data" "Input Data")
				     (:div :class "uk-form-controls"
					   (:textarea :class "uk-textarea" :id "input-data" :rows "4" :placeholder "Enter input data to run the system")))
			       (:div :class "uk-margin"
				     (:button :class "uk-button uk-button-primary" :type "submit" "Run"))))

		  ;; Button to return to Project Management
		  (:div :class "uk-text-center"
			(:a :href "project-management.html" :class "uk-button uk-button-default" "Return to Project Management")))

            ;; UIkit JS
            (:script :src "https://cdn.jsdelivr.net/npm/uikit@3.16.26/dist/js/uikit.min.js")
            (:script :src "https://cdn.jsdelivr.net/npm/uikit@3.16.26/dist/js/uikit-icons.min.js")

            ;; JavaScript for dynamic behavior
            (:script "
          document.addEventListener('DOMContentLoaded', () => {
            const consoleOutput = document.getElementById('console-output');
            const statusIndicator = document.getElementById('status-indicator');
            const startSystemBtn = document.getElementById('start-system-btn');
            const stopSystemBtn = document.getElementById('stop-system-btn');
            const debugSystemBtn = document.getElementById('debug-system-btn');
            const resetSystemBtn = document.getElementById('reset-system-btn');

            let systemRunning = false;

            // Function to start the system
            startSystemBtn.addEventListener('click', () => {
              if (!systemRunning) {
                systemRunning = true;
                statusIndicator.classList.remove('status-stopped');
                statusIndicator.classList.add('status-running');
                startSystemBtn.disabled = true;
                stopSystemBtn.disabled = false;
                debugSystemBtn.disabled = false;
                consoleOutput.textContent = 'System started. Waiting for commands...';
              }
            });

            // Function to stop the system
            stopSystemBtn.addEventListener('click', () => {
              if (systemRunning) {
                systemRunning = false;
                statusIndicator.classList.remove('status-running');
                statusIndicator.classList.add('status-stopped');
                startSystemBtn.disabled = false;
                stopSystemBtn.disabled = true;
                debugSystemBtn.disabled = true;
                consoleOutput.textContent = 'System stopped.';
              }
            });

            // Function to reset the system
            resetSystemBtn.addEventListener('click', () => {
              systemRunning = false;
              statusIndicator.classList.remove('status-running');
              statusIndicator.classList.add('status-stopped');
              startSystemBtn.disabled = false;
              stopSystemBtn.disabled = true;
              debugSystemBtn.disabled = true;
              consoleOutput.textContent = 'System reset. Press \"Start System\" to begin.';
            });
          });
        ")))))
