;; THIS CARRIES--see .dir-locals.el
;; this is by far the FASTEST option (IF YOU USE A BUFFER--WHY???)
(defun my/headless-godot-editor (&optional quiet)
  "Hand over the lsp and no-one gets hurt >:^("
  (interactive)
  (when (gdscript-util--find-project-configuration-file)
	  (let ((godot-buffer "*Headless Godot*")
			(godot-process(format "Headless Godot (%s)" (gdscript-util--get-godot-project-name))))
		(if (get-buffer godot-buffer)
			(unless quiet (message "%s already exists!" godot-process))
		  (start-process godot-process godot-buffer gdscript-godot-executable "--verbose" "-e" "--headless" "--lsp-port" "6008" (gdscript-util--find-project-configuration-file))
		  (unless quiet (message "%s started!" godot-process))))))

(defun my/kill-headless-godot-editor ()
  "STOP THAT LANGUAGE SEVER!"
  (interactive)
  (kill-buffer "*Headless Godot*"))

(bind-keys :prefix-map gdscript-command-map
		   :prefix "C-c g"
		   :menu-name "GDScript commands"
		   ("k" . gdscript-docs-online-search-api)
		   ("s" . my/headless-godot-editor)
		   ("q" . my/kill-headless-godot-editor)
		   ("r" . gdscript-godot-run-project-debug)
		   ("e" . gdscript-godot-open-project-in-editor)
		   ("d" . gdscript-debug-make-server)) ;; this is for DAP

(use-package gdscript-mode
  :straight (gdscript-mode
			 :type git
			 :host github
			 :repo "godotengine/emacs-gdscript-mode")
  :commands (gdscript-util--find-project-configuration-file
			 gdscript-docs-online-search-api
			 gdscript-godot-run-prject-debug
			 gdscript-godot-open-project-in-editor
			 gdscript-debug-make-server)
  :config
  (setq gdscript-docs-use-eww nil)
  (defun gdscript-eglot-contact (_interactive)
	'("netcat" "localhost" "6008"))
  (defun my/godot-project-setup ()
	"Stuff to do when projectile switches to a Godot project."
	(when (gdscript-util--find-project-configuration-file)
	  (my/headless-godot-editor)))
  :hook
  (gdscript-mode . eglot-ensure)
  (gdscript-mode . company-mode)
  (projectile-after-switch-project . my/godot-project-setup)
  ;; NOTE: you only need this if using godot 3
  :custom (gdscript-eglot-version 4))

;; (defun lsp--gdscript-ignore-errors (original-function &rest args)
;;   "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
;;   (if (string-equal major-mode "gdscript-mode")
;; 	  (let ((json-data (nth 0 args)))
;; 		(if (and (string= (gethash "jsonrpc" json-data "") "2.0")
;; 				 (not (gethash "id" json-data nil))
;; 				 (not (gethash "method" json-data nil)))
;; 			nil ; (message "Method not found")
;; 		  (apply original-function args)))
;; 	(apply original-function args)))
;; ;; Runs the function `lsp--gdscript-ignore-errors` around `lsp--get-message-type` to suppress unknown notification errors.
;; (advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors)

(use-package gdshader-mode
  :straight (gdshader-mode :type git :host github :repo "bbbscarter/gdshader-mode")
  ;; Optional customisations for company-mode completion.
  :init
  (defun gdshader-config()
    (interactive)
    (setq-local company-dabbrev-downcase nil)
    (setq-local company-backends
                '((company-keywords company-dabbrev))))
  :hook (gdshader-mode . gdshader-config)
  :config
  (add-to-list 'company-keywords-alist (append '(gdshader-mode) gdshader-all-keywords)))
