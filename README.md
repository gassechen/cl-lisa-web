# cl-lisa-web
# cl-lisa-web: Web Interface for the Lisa Rule Engine

This project is a **web-based user interface (GUI)** for **Lisa**, a powerful expert-system shell written in Common Lisp. The main goal is to provide a visual and intuitive platform for creating, managing, and running expert systems, abstracting the user from direct command-line interaction.

## Key Features

* **Project Management**: Create, list, and load different projects, each with its own files for rules, facts, templates, and functions.
* **Integrated Code Editor**: Includes a built-in code editor to modify project files directly in the browser, streamlining the development process.
* **Execution Control**: Offers buttons to **start**, **stop**, and **run** the Lisa inference engine, with real-time output displayed in a web console.
* **Dynamic Architecture**: Utilizes **HTMX** for asynchronous page updates, providing a smooth and responsive user experience without full page reloads.

## Technologies Used

* **Frontend**:
    * **Common Lisp**: Rendering and server communication logic.
    * **Spinneret**: HTML generation.
    * **UIkit**: CSS framework for a modern design.
    * **HTMX**: Dynamic client-side interactions.
    * **Ace Editor**: Embedded code editor for efficient editing.
* **Backend**:
    * **Common Lisp**: Programming language.
    * **Lisa**: The underlying rule engine.
    * **Easy-routes**: Web route management.
    * **Hunchentoot**: Web server.

## Project Structure

The system is designed to organize project files in a clear and consistent manner:

/projects
└─── /project-name
├─── facts.lisp
├─── rules.lisp
├─── templates.lisp
└─── functions.lisp

## Installation and Usage

1.  **Prerequisites**: Make sure you have a **Common Lisp** environment with **Quicklisp** installed and configured.
2.  **Clone Repository**:
	```sh
	cd ~/quicklisp/local-projects/
	git clone https://github.com/gassechen/cl-lisa-web.git
	cd cl-lisa-web
	
    ```

3.  **Load and Run**: In your Common Lisp REPL, load and start the system:
    ```lisp
    (ql:quickload :cl-lisa-web)
	(in-package :cl-lisa-web
    (start-server)
    ```
4.  **Access**: Open your web browser and navigate to `http://localhost:8888`.

## Contributions

Contributions are welcome. Feel free to open an issue or submit a pull request.



Prototype of web interface for building expert systems with Lisa https://github.com/youngde811/Lisa


https://youtu.be/v_eEz3Z83uU?si=64bvK7y03946RqKM
