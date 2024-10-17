(define-minor-mode coil-mode
  "Coil mode enables the use of the coil program from within Emacs."
  :global t
  :init-value nil
  :lighter " coil"
  :group 'coil-mode
  (unless (executable-find "coil") (user-error "coil executable not found in path!"))
  (if coil-mode
	  (progn
		(defun coil--kill-shell-command (cmd)
		  "Kills the output of a shell command."
		  (kill-new (shell-command-to-string cmd)))
		(defun coil-yank-region (&optional point mark)
		  "coil definitions from the region."
		  (interactive "r")
		  (if (and (equal major-mode #'c++-mode) (buffer-file-name))
			  (coil--kill-shell-command (format "coil -p -o %s %d %d" (buffer-name) point mark))
			(let ((str (buffer-substring point mark)))
			  (coil--kill-shell-command (format "coil \"%s\"" str))))
		  (when (and (bound-and-true-p evil-mode) (evil-visual-state-p)) (evil-normal-state)))
		(defun coil-file ()
		  "lmao"
		  (interactive)
		  (unless (buffer-file-name) (user-error "not visiting a file buffer!"))
		  (if (equal major-mode #'c++-mode)
			  (coil--kill-shell-command (format "coil -p -o %s" (buffer-name)))
			(coil--kill-shell-command (format "coil -o %s" (buffer-name)))))
		(defun coil-yank-file-with-header ()
		  "lmao"
		  (interactive)
		  (unless (buffer-file-name) (user-error "not visiting a file buffer!"))
		  (if (equal major-mode #'c++-mode)
			  (coil--kill-shell-command (format "coil -i -p -o %s" (buffer-name)))
			(coil--kill-shell-command (format "coil -i -o %s" (buffer-name)))))
		(defun coil-new-source (&optional origin dest)
		  "lmao"
		  (interactive "fheader: \nFsource: ")
		  (if (string-equal (downcase (file-name-extension dest)) "cpp")
			  (shell-command (format "coil -i -p -o %s -c %s" "" (file-name-nondirectory origin) dest))
			(shell-command (format "coil -i -o %s -c %s" (file-name-nondirectory origin) dest)))
		  (find-file dest))
		(defun coil-new-source-from-buffer ()
		  "honestly this might be TOO hot"
		  (interactive)
		  (unless (buffer-file-name) (user-error "not visiting a file buffer!"))
		  (let ((header (buffer-name)))
			(if (equal major-mode #'c++-mode)
				(shell-command (format "coil -i -p -o %s -c %s" header (file-name-with-extension header "cpp")))
			(shell-command (format "coil -i -o %s -c %s" header (file-name-with-extension header "c"))))
			(find-file (file-name-with-extension header "c")))))
	(fmakunbound #'coil--kill-shell-command)
	(fmakunbound #'coil-yank-region)
	(fmakunbound #'coil-yank-header)
	(fmakunbound #'coil-yank-header-and-include)
	(fmakunbound #'coil-new-source)
	(fmakunbound #'coil-new-source-from-buffer)))

  (downcase (file-name-extension "main.cpp"))

(defcustom coil-mode-keymap-prefix "C-c c"
  "coil-mode keymap prefix."
  :group 'coil-mode
  :type 'string)

(defvar coil-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "y") #'coil-yank-region)
	(define-key map (kbd "f") #'coil-file) ; -o option
	(define-key map (kbd "Y") #'coil-yank-file-with-header) ; -O option
	(define-key map (kbd "c") #'coil-new-source) ; -O -c
	map)
  "Keymap for coil commands after `coil-mode-keymap-prefix'.")
(fset 'coil-mode-map coil-mode-map)

(provide 'coil-mode)


(defun coil-sexp-buffer ()
  "..."
  (interactive)
  (unless (buffer-file-name) (user-error "not visiting a file buffer!"))
  (coil--kill-shell-command (format "coil -p -i -o %s" (buffer-name))))
