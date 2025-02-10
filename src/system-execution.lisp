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
            (:script :src "https://unpkg.com/htmx.org@1.9.5")
	   (:script  :src "https://cdnjs.cloudflare.com/ajax/libs/ace/1.4.12/ace.js"))

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


		   ;; Template Management (HTMX Integration)
		  (:div :class "uk-card uk-card-default uk-card-body"
			(:h3 :class "uk-card-title" "Template Management")
			;; List of Templates (Dynamic Updates with HTMX)
			(:div :id "templates-view"
			      :style "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;"
			      (:raw (get-templates-from-project project-name)))
			;; Add Template Button (HTMX Integration)
			(:button :class "uk-button uk-button-secondary"
				 ;;:data-hx-get "/api/add-rules-form"
				 :data-hx-target "#template-modal-content"
				 :data-hx-swap "innerHTML"
				 :data-uk-toggle "target: #template-modal" "Add Template")
			(:button :class "uk-button uk-button-secondary"
				 :data-hx-get (format nil "/api/templates/~A" project-name) 
				 :data-hx-target "#templates-view"
				 :data-hx-swap "innerHTML" "Reload templates"))
			

		  ;; Modal for Adding/Editing Templates
		  (modal-add-edit-templates project-name)
		  

		  ;; Rule Management (HTMX Integration)
		  (:div :class "uk-card uk-card-default uk-card-body"
			(:h3 :class "uk-card-title" "Rule Management")
			;; List of Rules (Dynamic Updates with HTMX)
			;; Editor for Rules
			(:div :id "rules-view"
			      :style "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;"
			      (:raw (get-rules-from-project project-name)))
		  	;; Add Template Button (HTMX Integration)
			(:button :class "uk-button uk-button-secondary"
				 ;;:data-hx-get "/api/add-rules-form"
				 :data-hx-target "#rule-modal-content"
				 :data-hx-swap "innerHTML"
				 :data-uk-toggle "target: #rule-modal" "Add Rule")
			(:button :class "uk-button uk-button-secondary"
				 :data-hx-get (format nil "/api/rules/~A" project-name) 
				 :data-hx-target "#rules-view"
				 :data-hx-swap "innerHTML" "Reload Rules"))

		  ;; Modal for Adding/Editing Rules
		 (modal-add-edit-rules project-name)

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
            (:raw "<script>
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

        </script>")))))



(defun modal-add-edit-templates (project-name)
  (spinneret:with-html
    (:div :id "template-modal" :data-uk-modal t
      (:div :class "uk-modal-dialog uk-modal-body"
        (:h2 :class "uk-modal-title" "Agregar/Editar Template")
        (:form :id "template-form" :class "uk-form-stacked"
          ;; Nombre del template
          (:div :class "uk-margin"
            (:label :class "uk-form-label" :for "template-name" "Nombre del template")
            (:div :class "uk-form-controls"
              (:input :class "uk-input" :id "template-name" :name "templateName" :type "text"
                      :placeholder "Ejemplo: template-1" :required t)))
          ;; Condición (Ace Editor)
          (:div :class "uk-margin"
            (:label :class "uk-form-label" :for "template-body" "Body")
            (:div :class "uk-form-controls"
              (:div :id "template-body-editor" :style "height: 150px; border: 1px solid #ddd; background-color: #f9f9f9;")))
          (:input :type "hidden" :name "projectName" :value project-name)
          ;; Botones
          (:div :class "uk-margin"
            (:button :class "uk-button uk-button-primary" :type "submit" "Guardar")
            (:button :class "uk-button uk-button-default uk-modal-close" :type "button" "Cancelar"))
          (:div :id "template-response-message")))
      
      ;; Initialize Ace Editors
      (:raw "
        <script>
          // Declarar variables globales para los editores
          var conditionEditor;

          // Initialize Ace Editors when the modal is shown
          UIkit.util.on('#template-modal', 'show', function() {
            // Initialize Condition Editor
            conditionEditor = ace.edit('template-body-editor');
            conditionEditor.session.setMode('ace/mode/lisp');
            conditionEditor.setOptions({
              maxLines: 10,
              minLines: 5,
              fontSize: '14px',
              showGutter: false
            });
          });

          // Manejar el envío del formulario
          document.getElementById('template-form').addEventListener('submit', function(event) {
            event.preventDefault(); // Evitar el envío del formulario

            // Obtener el contenido de los editores
            var conditionContent = conditionEditor.getValue();
            var projectName = document.querySelector('input[name=\"projectName\"]').value;

            // Agregar el contenido a campos ocultos o enviarlo mediante HTMX
            console.log('Template Body:', conditionContent);

            // Ejemplo: Enviar datos al servidor usando HTMX
            htmx.ajax('POST', '/api/add-template', {
              values: {
                templateName: document.getElementById('template-name').value,
                templateBody: conditionContent,
                projectName: projectName // Incluir el nombre del proyecto
              },
              target: '#template-response-message', // Actualizar el contenedor de mensajes
              swap: 'innerHTML' // Reemplazar el contenido del contenedor
            });
          });
        </script>
      "))))



(defun modal-add-edit-rules (project-name)
  (spinneret:with-html
    (:div :id "rule-modal" :data-uk-modal t
	  (:div :class "uk-modal-dialog uk-modal-body"
		(:h2 :class "uk-modal-title" "Agregar/Editar Regla")
		(:form :id "rule-form" :class "uk-form-stacked"
		       ;; Nombre de la Regla
		       (:div :class "uk-margin"
			     (:label :class "uk-form-label" :for "rule-name" "Nombre de la Regla")
			     (:div :class "uk-form-controls"
				   (:input :class "uk-input" :id "rule-name" :name "ruleName" :type "text"
					   :placeholder "Ejemplo: Regla 1" :required t)))
		       ;; Condición (Ace Editor)
		       (:div :class "uk-margin"
			     (:label :class "uk-form-label" :for "rule-condition" "Condición")
			     (:div :class "uk-form-controls"
				   (:div :id "rule-condition-editor" :style "height: 150px; border: 1px solid #ddd; background-color: #f9f9f9;")))
		       ;; Acción (Ace Editor)
		       (:div :class "uk-margin"
			     (:label :class "uk-form-label" :for "rule-action" "Acción")
			     (:div :class "uk-form-controls"
				   (:div :id "rule-action-editor" :style "height: 150px; border: 1px solid #ddd; background-color: #f9f9f9;")))
		       (:input :type "hidden" :name "projectName" :value project-name)
		       ;; Botones
		       (:div :class "uk-margin"
			     (:button :class "uk-button uk-button-primary" :type "submit" "Guardar")
			     (:button :class "uk-button uk-button-default uk-modal-close" :type "button" "Cancelar"))
		       (:div :id "rule-response-message")))
	  
	  ;; Initialize Ace Editors
	  ;; Initialize Ace Editors
      (:raw "
        <script>
          // Declarar variables globales para los editores
          var conditionEditor, actionEditor;
         // Initialize Ace Editors when the modal is shown
            UIkit.util.on('#rule-modal', 'show', function() {
              // Initialize Condition Editor
              conditionEditor = ace.edit('rule-condition-editor');
              conditionEditor.session.setMode('ace/mode/lisp');
              conditionEditor.setOptions({
                maxLines: 10,
                minLines: 5,
                fontSize: '14px',
                showGutter: false
              });

              // Initialize Action Editor
              actionEditor = ace.edit('rule-action-editor');
              actionEditor.session.setMode('ace/mode/lisp');
              actionEditor.setOptions({
                maxLines: 10,
                minLines: 5,
                fontSize: '14px',
                showGutter: false
              });
            });
          

          // Manejar el envío del formulario
          document.getElementById('rule-form').addEventListener('submit', function(event) {
            event.preventDefault(); // Evitar el envío del formulario

            // Obtener el contenido de los editores
            var conditionContent = conditionEditor.getValue();
            var actionContent = actionEditor.getValue();
            var projectName = document.querySelector('input[name=\"projectName\"]').value;
            // Agregar el contenido a campos ocultos o enviarlo mediante HTMX
            console.log('Condición:', conditionContent);
            console.log('Acción:', actionContent);

            // Ejemplo: Enviar datos al servidor usando HTMX
            htmx.ajax('POST', '/api/add-rule', {
              values: {
                ruleName: document.getElementById('rule-name').value,
                ruleCondition: conditionContent,
                ruleAction: actionContent,
                projectName: projectName // Incluir el nombre del proyecto 

              },
               target: '#rule-response-message', // Actualizar el contenedor de mensajes
               swap: 'innerHTML' // Reemplazar el contenido del contenedor
            });
          });
        </script>
      ")
	)))
