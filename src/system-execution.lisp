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
				    (:button :id "start-system-btn"
					     :data-hx-post (format nil "/api/project/start/~A" project-name)
					     :data-hx-target "#result-alert"
					     :class "uk-button uk-button-success"
					     (:span :class "status-indicator status-stopped" :id "status-indicator")
					     "Start System"))
			      (:div :class "uk-width-auto"
				    (:button :id "stop-system-btn"
					     :data-hx-post (format nil "/api/project/stop/~A" project-name)
					     :data-hx-target "#result-alert"
					     :class "uk-button uk-button-danger"
					      "Stop System"))
			      (:div :class "uk-width-auto"
				    (:button :id "debug-system-btn" :class "uk-button uk-button-secondary" :disabled t "Debug"))
			      (:div :class "uk-width-expand"
				    (:button :id "reset-system-btn" :class "uk-button uk-button-default" "Reset System")))
			      (:div :id "result-alert"))

		  ;; Console Output (Dynamic Updates with HTMX)
		  (:div :class "uk-card uk-card-default uk-card-body"
			(:h3 :class "uk-card-title" "Console Output")
			(:div :class "console-output" :id "console-output"
			      "System stopped. Press \"Start System\" to begin."))

		  (:div :class "uk-card uk-card-default uk-card-body"
			(:ul :data-uk-accordion "multiple: true"
			     (:li
			      (:a :class "uk-accordion-title" "Facts Management")
			      (:div :class "uk-accordion-content"
				    (:div :class "uk-card uk-card-default uk-card-body"
					  ;; List of Rules (Dynamic Updates with HTMX)
					  (:div :id "facts-view"
						:style "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;"
						(:raw (get-data-from-project project-name "facts")))
					  ;; Buttons
					  (:button :class "uk-button uk-button-secondary"
						   :onclick "edit('facts');"
						   "Edit facts")
					  (:button :id "evalfacts" :class "uk-button uk-button-secondary" :disabled t
						   :onclick (format nil "eval('~a','facts');" project-name)
						   "Eval facts")
					  (:button :class "uk-button uk-button-secondary"
						   :onclick (format nil "save('~a','facts');" project-name)
						   "Save facts")
					  (:div :id "facts-response-message"))))
			     (:li 
			      (:a :class "uk-accordion-title" "Template Management")
			      (:div :class "uk-accordion-content"
				    (:div :class "uk-card uk-card-default uk-card-body"
					  ;; List of Templates (Dynamic Updates with HTMX)
					  (:div :id "templates-view"
						:style "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;"
						(:raw (get-data-from-project project-name "templates")))
					  ;; Buttons
					  (:button :class "uk-button uk-button-secondary"
						   :onclick "edit('templates');"
						   "Edit templates")
					  (:button :id "evaltemplates" :class "uk-button uk-button-secondary" :disabled t
						   :onclick (format nil "eval('~a','templates');" project-name)
						   "Eval templates")
					  (:button :class "uk-button uk-button-secondary"
						   :onclick (format nil "save('~a','templates');" project-name)
						   "Save templates")
					  (:div :id "templates-response-message"))))
			     (:li
			      (:a :class "uk-accordion-title" "Rule Management")
			      (:div :class "uk-accordion-content"
				    (:div :class "uk-card uk-card-default uk-card-body"
					  ;; List of Rules (Dynamic Updates with HTMX)
					  (:div :id "rules-view"
						:style "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;"
						(:raw (get-data-from-project project-name "rules")))
					  ;; Buttons
					  (:button :class "uk-button uk-button-secondary"
						   :onclick "edit('rules');"
						   "Edit rules")
					  (:button :id "evalrules" :class "uk-button uk-button-secondary" :disabled t
						   :onclick (format nil "eval('~a','rules');" project-name)
						   "Eval rules")
					  (:button :class "uk-button uk-button-secondary"
						   :onclick (format nil "save('~a','rules');" project-name)
						   "Save rules")
					  (:div :id "rules-response-message"))))
			     (:li
			      (:a :class "uk-accordion-title" "Functions Management")
			      (:div :class "uk-accordion-content"
				    (:div :class "uk-card uk-card-default uk-card-body"
					  ;; List of Rules (Dynamic Updates with HTMX)
					  (:div :id "functions-view"
						:style "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;"
						(:raw (get-data-from-project project-name "functions")))
					  ;; Buttons
					  (:button :class "uk-button uk-button-secondary"
						   :onclick "edit('functions');"
						   "Edit functions")
					  (:button :id "evalfunctions" :class "uk-button uk-button-secondary" :disabled t
						   :onclick (format nil "eval('~a','functions');" project-name)
						   "Eval functions")
					  (:button :class "uk-button uk-button-secondary"
						   :onclick (format nil "save('~a','functions');" project-name)
						   "Save functions")
					  (:div :id "functions-response-message"))))

			     (:li
			      (:a :class "uk-accordion-title" "Run System")
			      (:div :class "uk-accordion-content"
				    (:div :class "uk-card uk-card-default uk-card-body"
					  ;; List of Rules (Dynamic Updates with HTMX)
					  (:div :id "asserts-view"
						:style "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;")
					  ;; Buttons
					  (:button :class "uk-button uk-button-secondary"
						   :onclick "edit('asserts');"
						   "Edit Asserts")
					  (:button :class "uk-button uk-button-primary"
						   :onclick "run();"
						   "Run")

					  (:div :id "asserts-response-message")))))))

            ;; UIkit JS
            (:script :src "https://cdn.jsdelivr.net/npm/uikit@3.16.26/dist/js/uikit.min.js")
            (:script :src "https://cdn.jsdelivr.net/npm/uikit@3.16.26/dist/js/uikit-icons.min.js")

            ;; JavaScript for dynamic behavior
            (:raw "<script>

function edit(param) {
            
            let typeForm = param;
            conditionEditor = ace.edit(param+'-view');
            conditionEditor.setTheme('ace/theme/github');
            conditionEditor.session.setMode('ace/mode/lisp');
            conditionEditor.setOptions({
              maxLines: 10,
              minLines: 5,
              fontSize: '14px',
              showGutter: false
            });}


function save(projectName,param){
            document.getElementById('eval'+param).disabled = false; 
            // Obtener el contenido de los editores
            var package='(in-package :cl-lisa-web)';
            var conditionContent = package + conditionEditor.getValue();
            var projectName = projectName;
            var api=param;

            // Agregar el contenido a campos ocultos o enviarlo mediante HTMX
            console.log('projectName Body:', projectName);
            console.log('Template Body:', package + conditionContent);

            // Ejemplo: Enviar datos al servidor usando HTMX
            htmx.ajax('POST', '/api/save/'+api, {
              values: {
                content: conditionContent,
                projectName: projectName // Incluir el nombre del proyecto
              },
              target: '#'+param+'-response-message', // Actualizar el contenedor de mensajes
              swap: 'innerHTML' // Reemplazar el contenido del contenedor
            });}


function eval(projectName,param){
            var conditionContent = conditionEditor.getValue();
            var projectName = projectName;
            var api=param;
   
            // Ejemplo: Enviar datos al servidor usando HTMX
            htmx.ajax('POST', '/api/eval/'+api, {
              values: {
                content: conditionContent,
                projectName: projectName // Incluir el nombre del proyecto
              },
              target: '#'+param+'-response-message', // Actualizar el contenedor de mensajes
              swap: 'innerHTML' // Reemplazar el contenido del contenedor
            });}



function run(){
            var conditionContent = conditionEditor.getValue();
            // Ejemplo: Enviar datos al servidor usando HTMX
            htmx.ajax('POST', '/api/project/run', {
              values: {
                content: conditionContent,
              },
              target: '#console-output', // Actualizar el contenedor de mensajes
              swap: 'innerHTML' // Reemplazar el contenido del contenedor
            });}


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
