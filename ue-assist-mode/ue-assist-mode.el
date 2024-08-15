;; -*- lexical-binding: t -*-
;; `https://www.emacswiki.org/emacs/KeywordArguments'
;; `https://emacsdocs.org/docs/elisp/Quoting'
;; https://github.com/magnars/dash.el?tab=readme-ov-file#binding
;; USE `plist-member' and `plist-get' and `cdr'

(require 'cl)
(require 'dash)

(declare-function yas-activate-extra-mode "ext:yansippet")
(declare-function yas-deactivate-extra-mode "ext:yansippet")

(defvar ue-assist-uproject nil
  "the uproject file of the current project")

(defvar ue-assist-project-name nil
  "Project name")

;; BAT vars -- these are getting phased out later

(make-variable-buffer-local
 (defvar ue-assist--generate-bat nil
   "Project-local generate.bat from ue-assist.py"))

(make-variable-buffer-local
 (defvar ue-assist--build-bat nil
   "Project-local build.bat from ue-assist.py"))

(make-variable-buffer-local
 (defvar ue-assist--editor-bat nil
   "Project-local editor.bat from ue-assist.py"))

(make-variable-buffer-local
 (defvar ue-assist-api nil
   "Name of your project's Module API"))

(defun ue-assist--api-check ()
  "Checks that ue-assist-api is set & messages if not"
  (if (boundp 'ue-assist-api)
	  t
	(message "Set ue-assist-api in project root .dir-locals.el!") nil))

;; EXE vars -- these will be introduced when I stop using batch files

(defcustom ue-assist--editor nil
  "Full Path to UnrealEditor."
  :group 'ue-assist-mode
  :type 'string)

(defcustom ue-assist--ubt nil
  "Full path to UnrealBuildTool."
  :group 'ue-assist-mode
  :type 'string)

;; :start-message
;; :arguments
;; :callback
(defmacro ue-assist--process (command process-name buffer-name &rest args)
  "DO I LOOK LIKE I KNOW WHAT A JAYPEG IS?"
  (declare (indent defun))
  ;; (message "%s" (eval command))
  (cond ((not (eval command)) (message "symbol %s is nil!" command))
		;; ((-> (eval command)(f-file-p)(not)) (message "Command %s has no associated executable: %s" command (eval command)))
		((-> (eval process-name)(get-process)(process-live-p)) (message "Process %s already running!" (eval process-name)))
		(t (let
			   ((start-message (eval (plist-get args :start-message)))
				(arguments (eval (plist-get args :arguments)))
				(callback (cdr (plist-member args :callback))))
			 (message "stat-message: %s" start-message)
			 (message "arguments: %s" arguments)
			 (message "callback: %s" callback)
			 (--> `(,(eval process-name) ,(eval buffer-name) ,(eval command))
				  (append it arguments)
				  (apply 'start-process it)
				  (set-process-sentinel it (lambda (process event)
											 (unless (process-live-p process)
											   (message "%s %s" (process-name process) event)
											   (if (string= event "finished\n")
												   (progn (kill-buffer (process-buffer process))
														  (eval (macroexp-progn callback)))
												 (unless (get-buffer-window (process-buffer process))
												   (-> (split-window-sensibly)
													   (set-window-buffer (process-buffer process)))))))))
											   ;; (eval (macroexp-progn callback))))))
			 (if start-message (message start-message) (eval command))))))

;; (defun ue-assist--editor-target ()
;;   (concat ue-assist-project-name "Editor"))

;; (defun ue-assist--commands-outdir ()
;;   (concat "-OutputDir=" (file-name-directory ue-assist-uproject)))

(defun ue-assist-generate ()
  "Generate a compilation database for the current Unreal project."
  (interactive)
  (macroexpand '(ue-assist--process ue-assist--generate-bat "ue-assist: compilation commands" "*UE Assist--Compilation Commands*"
				  :start-message "Generating compilation_database.json"
				  :then
				  (when (and (fboundp 'lsp-workspace-restart)(fboundp 'lsp-workspaces))
					(mapc #'lsp-workspace-restart (lsp-workspaces))))))

(defun ue-assist-build ()
  "Build the current Unreal project."
  (interactive)
  (macroexpand '(ue-assist--process ue-assist--build-bat "ue-assist: build editor" "*UE Assist--Build Editor"
				  :start-message "Building editor binary")))

(defun ue-assist-build-then-editor ()
  "Build the current Unreal project and launch the editor if it was succesful."
  (interactive)
  (macroexpand '(ue-assist--process ue-assist--build-bat "ue-assist: build editor" "*UE Assist--Build Editor"
				  :start-message "Building editor binary"
				  :then (ue-assist-editor))))

(defun ue-assist-editor ()
  "Launch the Unreal Editor for the current project."
  (interactive)
  (macroexpand '(ue-assist--process ue-assist--editor-bat "ue-assist: editor" "*UE Assist--Editor"
				  :start-message "Launching the editor")))

(defun ue-assist-mode-enter ()
  "Setup for entering unreal engine mode."
  (message "enter ue-assist")
  (yas-activate-extra-mode 'ue-assist-mode))
  ;; (add-hook 'yas-after-exit-snippet-hook #'hack-local-variables))

(defun ue-assist-mode-exit ()
  "Cleanup for exiting unreal engine mode."
  (message "exit ue-assist")
  (yas-deactivate-extra-mode 'ue-assist-mode))
  ;; (remove-hook 'yas-after-exit-snippet-hook #'hack-local-variables))

(define-minor-mode ue-assist-mode
  "UE-Assist provides functions with useable keybinds & snippets to Emacs for developing Unreal projects."
  ;; :keymap ue-assist-mode-map
  :init-value nil
  :lighter " UE-Assist"
  :group 'ue-assist-mode
  (if ue-assist-mode
	  (ue-assist-mode-enter)
	(ue-assist-mode-exit)))

(defcustom ue-assist-keymap-prefix "C-c u"
  "ue-assist-mode keymap prefix."
  :group 'ue-assist-mode
  :type 'string)

(defvar ue-assist-command-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "g") #'ue-assist-generate)
	(define-key map (kbd "b") #'ue-assist-build)
	(define-key map (kbd "e") #'ue-assist-editor)
	(define-key map (kbd "f") #'ue-assist-build-then-editor)
	map)
  "Keymap for UE-Assist commands after `ue-assist-keymap-prefix'.")
(fset 'ue-assist-command-map ue-assist-command-map)


;; (add-hook 'ue-assist-mode-hook)
;; (add-hook 'ue-assist-mode-on-hook #'ue-assist-mode-enter)
;; (add-hook 'ue-assist-mode-off-hook #'ue-assist-mode-exit)

(provide 'ue-assist-mode)
;;; ue-assist-el ends here
