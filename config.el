;; EMACS CONFIG
;; see `https://blog.aaronbieber.com/2015/05/24/from-vim-to-emacs-in-fourteen-days.html' for the gestalt
;; add emacs dir with the binary to the PATH
;; create an einvornmnent variable HOME, (at Users/name or wherever), create .emacs.d there
;; remember -- you _MUST_ run `all-the-icons-install-fonts' AND `nerd-icons-install-fonts' then install the fonts to get that working ;; NOTE now run `nerd-icons-install-fonts' then install the fonts to get that working
;; see `https://www.emacswiki.org/emacs/BookMarks' for bookmark usage
;; NOTE sometimes you gotta run package-refresh-contents

;; (uinversal-argument) into (rectangle-number-lines)

;; NOTE see `'https://irreal.org/blog' for some emacs stuff
;; NOTE see `'https://codelearn.me/' for more emacs stuff

;; Make startup faster by reducing the frequency of gc.  Default is 800kb -- measured in bytes.
(setq gc-cons-threshod (* 50 1000 1000))

;; increase the number of lisp variable bindings
;; (setq max-specpdl-size (* 4 2500))

;; enable local variables PLEASE
(setq enable-local-variables t)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(tooltip-mode -1)
(toggle-scroll-bar -1)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(set-fringe-mode 10)
(menu-bar-mode -1)
;; (setq display-time-day-and-date 1)
;; (display-time)
(setq visible-bell t)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq warning-minimum-level :error)


;; NO PROMPT WARRANTS MORE THAN 1 CHARACTER
(defalias 'yes-or-no-p 'y-or-n-p)

;; only show trailing white space when programming
(add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))
;; (add-hook 'prog-mode-hook (electric-indent-mode 0))

;; (defun my/desktop-read ()
;;   "Save current desktop into the desktop directory."
;;   (interactive)
;;   (desktop-read desktop-dirname))

;; (global-set-key [f6] 'desktop-save-in-desktop-dir)
;; example/

;; enable folding while in prog-mode
(add-hook 'prog-mode-hook (lambda ()
							(hi-lock-mode)
							(highlight-regexp "TODO")
							(highlight-regexp "NOTE")
							(highlight-regexp "FAMPAI")
							(highlight-regexp "SENPAI")
							(highlight-regexp "NOTA BENE")))

(add-hook 'prog-mode-hook 'outline-minor-mode)


;; 80 character line in prog-mode
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))

;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escapequit)

;; shell settings
(setq eshell-scroll-to-bottom-on-input t) ;; NOTE eshell only q.q
;; make file paths clickable?
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
;; (add-hook 'eshell-mode-hook 'compilation-shell-minor-mode) ;; it just don't work

;; (setq quick-insert-str "change inserted string by (seq-local quick-insert-str \"foo\") ")

;; enable recent file tracking
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(run-at-time nil (* 5 60) 'recentf-save-list)
;; prevent using UI dialogs for prompts
(setq use-dialog-box nil)
;; global auto revert files
(global-auto-revert-mode 1)

(defun qinsert-func ()
  "Inserts the `qinsert' string. Or encourages user to assign it."
  (interactive)
  (if (boundp 'qinsert)
	  (progn
		(insert qinsert)
		(evil-append 0 0))
	(message "No quick insert string!  Eval (setq-local qinsert VALUE)")))
;; (message "No quick insert string!  Use (setq-local qinsert \"foo\") then `eval-last-sexp'")))
;; (global-set-key (kbd "M-i") 'qinsert-func)

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

;; manually load some packages\p
;; (add-to-list 'load-path "~/.emacs.d/lisp")

;; add undo-browse so it's EASY to replay changes
;; this shit's kinda fucky tbh
;;(load "undo-browse")

;; (defun replay-buffer ()
;;   ;; replays a buffer using undo-browse's movie
;;   (interactive)
;;   (ub-movie-history)
;;   (read-only-mode -1)
;;   (ub-mode))

;; hide the async shell command buffer
(add-to-list 'display-buffer-alist '("*Async Shell Command*" . (display-buffer-no-window . nil)))

;; allow remembering risky commands

;; turn on RELATIVE line numbers
;; NOTE: visual is better than relative for navigating w/ code folds
(column-number-mode)
(setq display-line-numbers 'visual)
(display-line-numbers-mode)
(advice-add 'risky-local-variable-p :override #'ignore)

(defun toggle-linums ()
  "Toggle between relative & absolute line numbers."
  (interactive)
  (if (eq display-line-numbers t)
	  (setq display-line-numbers 'visual)
	(setq display-line-numbers t)))

(defun absolute-linums ()
  "set absolute line numbers."
  (setq display-line-numbers t))

(defun relative-linums ()
  "set relative line numbers."
  (setq display-line-numbers 'visual))

;; HAND OVER THE RELATIVE LINUMS NOW!
(add-hook 'prog-mode-hook 'relative-linums)

(defun obs-1080p ()
  "set the frame size to 1900x1080 pixels because for some raisin emacs gives a free 20 horizontal pixels."
  (interactive)
  (set-frame-size (selected-frame) 1900 1080 t))

(defun obs-720p ()
  "set the frame size to 1900x1080 pixels because for some raisin emacs gives a free 20 horizontal pixels."
  (interactive)
  (set-frame-size (selected-frame) 1260 720 t))

(defun ymd-date ()
  "lol lmao"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; but disable for some modes
(dolist (mode '(term-mode-hook
				eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; IT JUST WORKS
(setq-default tab-width 4)
(setq c-default-style "bsd")
(setq c-basic-offset 4)

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name
		"straight/repos/straight.el/bootstrap.el"
		(or (bound-and-true-p straight-base-dir)
			user-emacs-directory)))
	  (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package with straight

;; setup use-package
;; NOTE: see `https://github.com./jweiegley/use-package' for usage
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; install auctex for the memes
;; (unless (package-installed-p 'auctex)
;;   (package-refresh-contents)
;;   (package-install 'auctex))

;; DO NOT LIKE
;; (use-package latex-preview-pane
;;   :ensure t
;;   :init (latex-preview-pane-enable))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(straight-use-package 'use-package)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (setq auto-package-update-prompt-before-update (not (daemonp)))
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "17:00"))

(defun my/vsplit-then-move-right ()
  "Split the current window right, then move into the new window."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun my/split-then-move-down ()
  "Split the current window below, then move into the new window."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun my/shell-right ()
  "foo"
  (interactive)
  (my/vsplit-then-move-right)
  (shell))

(defun my/shell-down ()
  "foo"
  (interactive)
  (my/split-then-move-down)
  (shell))

(defun my/ibuffer-toggle ()
  "toggle the ibuffer list"
  (interactive)
  (if (string= (buffer-name) "*Ibuffer*")
	  (kill-buffer-and-window)
	(progn
	  (ibuffer-list-buffers)
	  (pop-to-buffer "*Ibuffer*")
	  (ibuffer-mark-unsaved-buffers))))

(defun my/rectangle-number-lines ()
  "GOD I NEEDED THIS."
  (interactive)
  ;; (universal-argument) ;; lmao doesn't work GOOD TO KNOW THO--BOUND TO C-u BY DEFAULT--ITS NIFTY!
  (rectangle-number-lines (region-beginning) (region-end) (read-number "First digit: " 0) (read-string "Format: " "%d")))

;; tab-bar memels
(setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
(setq tab-bar-close-button-show nil)       ;; hide tab close / X button
;; (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
(setq tab-bar-new-tab-choice t)            ;; lol this is the default AND I LIKE IT
(setq tab-bar-tab-hints t)                 ;; show tab numbers
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(tab-bar-mode 1)                           ;; enable tab bar

;; (defun find-or-dired (dirname &rest switches)
;;   ""
;;   (interactive (dired-read-dir-and-switches ""))
;;   (message "%s %s" dirname switches))

;; THIS IS JUST dired-jump YOU MONG
;; The minibuffer is based but this ok too
;; (defun my/dired-dir ()
;;   "ya im thinkn ya"
;;   (interactive)
;;   (--if-let (file-name-directory (buffer-name))
;; 	  (find-file it)
;; 	(find-file default-directory)))

(defun my/dired-recursive (&optional dirname)
  ""
  (interactive "GDired recursive (directory): ")
  (dired dirname "-laRgho"))

;; (switch-to-buffer-same-window (dired-noselect dirname switches));; dired option

;; minibuffer-with-setup-hook
;; after-change-functions
;; set-window-buffer
;; find-file-noselect
(defun message-and-kill (buffer)
  ""
  (unless (get-buffer-window buffer)
	(message "killing: %s" (buffer-name buffer))
	(kill-buffer buffer)))

(defun my/find-file ()
  "Show `find-file' results (only with full matches) with a psuedo-temporary buffer in current window."
  (interactive)
  (let ((window (selected-window))
		(temp-buffers '()))
	(minibuffer-with-setup-hook
		(lambda ()
		  (add-hook 'after-change-functions
					(lambda (beg end len)
					  (when (file-exists-p (minibuffer-contents))
						(setq temp-buffers (append `(,(find-file-noselect (minibuffer-contents))) temp-buffers))
						(set-window-buffer window (car temp-buffers)))))
		  (add-hook 'minibuffer-exit-hook
					(lambda () (mapc (lambda (buffer)
									   (unless (get-buffer-window buffer)
										 ;; (message "killing: %s" (buffer-name buffer))
										 (kill-buffer buffer)))
									 temp-buffers))))
	  (find-file (read-file-name "Find file: ")))))

(defun my/dired ()
  "Show Dired results with a psuedo-temporary buffer in current window."
  (interactive)
  (let ((window (selected-window))
		(temp-buffers '()))
	(minibuffer-with-setup-hook
		(lambda ()
		  (add-hook 'after-change-functions
					(lambda (beg end len)
					  (setq temp-buffers (append `(,(dired-noselect (minibuffer-contents))) temp-buffers))
					  (set-window-buffer window (car temp-buffers))))
		  (add-hook 'minibuffer-exit-hook
					(lambda () (mapc (lambda (buffer)
									   (unless (get-buffer-window buffer)
										 ;; (message "killing: %s" (buffer-name buffer))
										 (kill-buffer buffer)))
									 temp-buffers))))
	  (dired (read-file-name "Find file: " nil nil nil)))))

(use-package evil
  :ensure t
  :demand t
  :init
  (evil-mode)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (define-prefix-command 'my-shell-command-map)
  (global-set-key (kbd "C-e") 'my-shell-command-map)
  :config
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  :bind
  ("M-e" . eval-last-sexp)
  ("M-f" . my/find-file)
  ("C-c d" . my/dired)
  ("C-c D" . find-name-dired)
  ;; ("M-b" . switch-to-buffer)
  (:map evil-normal-state-map
		("<leader> x" . eval-last-sexp)
		("\\ b" . my/ibuffer-toggle)
		("C-w V" . my/vsplit-then-move-right)
		("C-w S" . my/split-then-move-down)
		("\\ l" . toggle-linums)
		;; I REALLY LIKE THIS WOW
		("<leader> t" . tab-bar-select-tab) ;; press numbers THEN <leader> t
		("<leader> h" . evil-first-non-blank)
		("<leader> l" . evil-end-of-line)
		("\\ d" . dired-jump)
		("\\ D" . my/dired-recursive)
		("\\ n" . tab-bar-new-tab)
		("\\ q" . tab-bar-close-tab)
		("C-s <return>" . shell)
		("C-s v" . my/shell-right)
		("C-s s" . my/shell-down)
		("<leader> f" . my/find-file)
		;; ("<leader> d" . dired)
		;; ("<leader> b" . switch-to-buffer)
		;; ("g <backspace>" . switch-to-prev-buffer)
		("g b" . switch-to-prev-buffer)
		:map evil-visual-state-map
		("\\ #" . my/rectangle-number-lines)
		("<leader> x" . eval-region)
		("<leader> h" . evil-first-non-blank)
		("<leader> l" . evil-end-of-line)
		("\\ b" . my/ibuffer-toggle)
		("\\ l" . toggle-linums)
		;; ("<leader> f" . find-file)
		;; ("<leader> d" . dired)
		;; ("<leader> b" . switch-to-buffer)
		("C-w V" . my/vsplit-then-move-right)
		("C-w S" . my/split-then-move-down)))
;; ("C-w V" . '(progn (split-window-right)(windmove-right)))
;; ("C-w S" . '(progn (split-window-below)(windmove-down))))

;; TODO leader keybinds
(use-package org
  ;; :after evil
  :ensure t
  :init
  (setq org-M-RET-may-split-line nil))

(use-package auto-complete
  :ensure t
  :bind ("<f7>" . auto-complete-mode)
  :diminish auto-complete-mode
  :hook (prog-mode . auto-complete-mode)
  :config
  (ac-config-default))

(use-package yasnippet
  ;; :after evil
  :ensure t
  :init
  (setq yas-indent-line 'fixed)
  (defcustom yas-cpp-class nil
	"Class name of current CPP for yasnippet."
	:type 'string
	:local t)
  (yas-global-mode 1)
  :bind
  ("M-y". yas-expand)
  ("C-M-y". yas-insert-snippet)
  (:map evil-normal-state-map
		("<leader> y" . yas-expand)
		:map evil-visual-state-map
		("<leader> y" . yas-expand)))

;; (use-package origami
;;   :ensure t
;;   :hook (prog-mode . origami-mode))

;; ;; (use-package anzu
;; ;;   :ensure t
;; ;;   :config
;; ;;   (global-anzu-mode))

;; ;; (use-package evil-anzu
;; ;;   :ensure t
;; ;;   :after (:all evil anzu))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :ensure t
  :bind
  (:map evil-normal-state-map
		("<leader> c i" . evilnc-comment-or-uncomment-lines)
		("<leader> c c" . evilnc-copy-and-comment-lines)
		:map evil-visual-state-map
		("<leader> c i" . evilnc-comment-or-uncomment-lines)
		("<leader> c c" . evilnc-copy-and-comment-lines)))

(use-package dired
  :ensure nil
  ;; :after evil-collection
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-lagho --group-directories-first"))
  :hook ((dired-mode . dired-hide-details-mode)
		 (dired-mode . dired-omit-mode)
		 ;; (dired-mode . (display-line-numbers . 'visual))
		 (dired-mode . relative-linums)
		 (dired-mode . display-line-numbers-mode))
  :config
  (setq dired-dwim-target t)
  (evil-collection-define-key 'normal 'dired-mode-map
	"h" 'dired-single-up-directory
	"l" 'dired-single-buffer
	"c" 'my/dired-create)
  :bind
  (:map evil-normal-state-map
		("z c" . dired-kill-subdir)
		("z o" . dired-maybe-insert-subdir)))

;; BRUH
;; (use-package shell
;;   :ensure nil
;;   :bind
;;   (:map evil-normal-state-map
;; 		("<return>" . comint-send-input)
;; 		("C-]" . comint-next-input)
;; 		("C-[" . comint-previous-input)))

;; file-exists-p
(defun my/dired-create (&optional arg)
  "foo"
  (interactive "GNew File/Directory: ")
  (if (or (file-exists-p arg) (f-directory-p arg))
	  (dired-goto-file arg)
	(if (directory-name-p arg)
		(dired-create-directory arg)
		(dired-create-empty-file arg)))
  (revert-buffer-quick))


;; (use-package dired-single)

(use-package dired-hide-dotfiles
  :ensure t
  ;; :after evil-collection
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
	"H" 'dired-hide-dotfiles-mode))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(defun sp-wrap-single-quote (&optional arg)
  "bruh."
  (interactive "P")
  (sp-wrap-with-pair "'"))
(defun sp-wrap-double-quote (&optional arg)
  "bruh."
  (interactive "P")
  (sp-wrap-with-pair "\""))

;; ;; see `https://github.com/Fuco1/smartparens/wiki'
(use-package smartparens
  ;; :after evil
  :ensure t
  :diminish smartparens-mode
  :hook
  (prog-mode . smartparens-mode)
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  (setq sp-highlight-pair-overlay nil)
  :bind
  ("C-l" . sp-forward-sexp)
  ("C-k" . sp-backward-sexp)
  (:map evil-insert-state-map
		("C-k" . sp-backward-sexp)
		:map evil-normal-state-map
		("<leader> '" . sp-wrap-single-quote)
		("<leader> \"" . sp-wrap-double-quote)
		("<leader> (" . sp-wrap-round)
		("<leader> )" . sp-wrap-round)
		("<leader> {" . sp-wrap-curly)
		("<leader> }" . sp-wrap-curly)
		("<leader> ]" . sp-wrap-square)
		("<leader> [" . sp-wrap-square)
		:map evil-visual-state-map
		("<leader> '" . sp-wrap-single-quote)
		("<leader> \"" . sp-wrap-double-quote)
		("<leader> (" . sp-wrap-round)
		("<leader> )" . sp-wrap-round)
		("<leader> {" . sp-wrap-curly)
		("<leader> }" . sp-wrap-curly)
		("<leader> ]" . sp-wrap-square)
		("<leader> [" . sp-wrap-square)))

(use-package js2-mode
  :ensure t
  :init
  (setq js2-bounce-indent-p t))

(use-package json-mode
  :ensure t)

(use-package csharp-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode)))

(use-package flycheck
  ;; :after evil
  :ensure t
  :config
  (setq flycheck-python-flake8-executable "c:/Python39/Scripts/flake8.exe")
  :hook
  ;; (lisp-mode . flycheck-mode)
  (json-mode . flycheck-mode)
  (prog-mode . flycheck-mode)
  (LaTeX-mode . flycheck-mode)
  :bind
  ("<f8>" . flycheck-mode)
  (:map evil-normal-state-map
		("<leader> e" . flycheck-next-error)
		("<leader> E" . flycheck-previous-error)))

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

(use-package all-the-icons ;; you HAVE to install the fonts for windows (run all-the-icons-install-fonts)
  :ensure t)

;; use '-' and '|' to open node in  splits & vsplits respectively
(use-package neotree
  ;; :after evil
  :ensure t
  :init
  (setq neo-smart-open t)
  (setq neo-banner-message "'U' to go up a dir")
  (setq neo-window-position 'right)
  (setq neo-autorefresh t)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq-default neo-show-updir-line t)
  (setq-default neo-show-slash-for-folder t)
  (setq-default neo-show-hidden-files nil)
  (setq neo-window-fixed-size nil)
  (setq neo-theme (if (display-graphic-p) 'classic 'ascii))
  :bind
  ("<f10>" . neotree-hidden-file-toggle)
  (:map evil-normal-state-map
		("\\ t" . neotree-toggle)
		:map evil-visual-state-map
		("\\ t" . neotree-toggle)))

(use-package solaire-mode
  :ensure t
  :diminish solaire-global-mode
  :hook (after-init . solaire-global-mode))

;; I don't use this at all tbqh
;; (use-package undo-tree
;;   :ensure t
;;   :diminish global-undo-tree-mode
;;   :after evil
;;   :bind
;;   (:map evil-normal-state-map
;; 		("\\ u" . undo-tree-visualize)
;; 		:map evil-visual-state-map
;; 		("\\ u" . undo-tree-visualize))
;;   :config
;;   (append neo-hidden-regexp-list '("\\.aux$" "\\.bak$" "\\.toc$")) ;; latex ignores
;;   (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

(use-package magit
  ;; :after evil
  :ensure t
  :config
  :bind
  (:map evil-normal-state-map
		("\\ g" . magit-status)
		:map evil-visual-state-map
		("\\ g" . magit-status)))

(use-package imenu-list
  ;; :after evil
  :ensure t
  :init
  (setq imenu-list-focus-after-activation t)
  :bind
  (:map evil-normal-state-map
		("\\ i" . imenu-list-minor-mode)
		:map evil-visual-state-map
		("\\ i" . imenu-list-minor-mode)))

(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides-mode
  :hook (prog-mode . highlight-indent-guides-mode)
  :config (setq highlight-indent-guides-method 'character))

(defun my/context-ag (&optional initial-value)
  "Use projectile-ag if in a project, otherwise regular ag."
  (interactive)
  (if (projectile-project-p)
	  (ag-project (read-string "search-string (use ag-project-files to limit search to a given filetype):" initial-value))
	(ag (read-string "search-string (use ag-files to limit search to a given filetype):" initial-value)
		(file-name-directory buffer-file-name))))

(defun my/normal-ag ()
  "Use projectile-ag if in a project, otherwise regular ag."
  (interactive)
  (my/context-ag (if (symbol-at-point) (symbol-name (symbol-at-point)))))

(defun my/visual-ag ()
  "Use projectile-ag if in a project, otherwise regular ag."
  (interactive)
  (my/context-ag (buffer-substring-no-properties (region-beginning) (region-end))))

(defun my/projectile-find-other-file ()
  "projectile-find-other-file but with flex-matching enabled by default"
  (interactive)
  (projectile-find-other-file t))

(use-package projectile
  :ensure t
  ;; :after evil
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  :bind
  ;; ("s-p" . projectile-command-map)
  ("C-c p" . projectile-command-map)
  (:map evil-normal-state-map
		("\\ p" . projectile-switch-project)
		("<leader> a" . my/normal-ag)
		("\\ h" . my/projectile-find-other-file)
		:map evil-visual-state-map
		("\\ p" . projectile-switch-project)
		("<leader> a" . my/visual-ag)
		("\\ h" . my/projectile-find-other-file))
  :config (projectile-mode 1))

(use-package ag
  :init
  (setq ag-highlight-search t)
  (setq ag-reuse-window t))

(use-package flycheck-projectile
  :after projectile
  :load-path "~/.emacs.d/packages"
  :bind ("C-c e" . flycheck-projectile-list-errors))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-set-navigator t)
  (setq dashboard-startup-banner "~/.emacs.d/ascii-art.txt")
  (setq dashboard-set-footer t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-items '((projects . 5)
						  (bookmarks . 5)
						  (recents . 5)))
  (add-to-list 'dashboard-items '(agenda) t)
  (dashboard-setup-startup-hook))

(defun my/refresh-revert ()
  "Refresh the dashboard or revert the current buffer."
  (interactive)
  (cond
   ((string= (buffer-name) "*dashboard*")
	(dashboard-refresh-buffer))
   ((string-match-p "magit:.+" (buffer-name))
	(magit-refresh))
   (t (revert-buffer t t t)))
  (message "%s refreshed!" (buffer-name)))
;; OLD
;; (if (string= (buffer-name) "*dashboard*")
;; 	  (progn
;; 		(dashboard-refresh-buffer)
;; 		(message "%s refreshed!" (buffer-name)))
;; 	(if (string-match "magit:.+" (buffer-name))
;; 		(progn
;; 		  (magit-refresh)
;; 		  (message "%s refreshed!" (buffer-name)))
;; 	  (if (string-match "\*.+?\*" (buffer-name))
;; 		  (message "cannot revert %s!" (buffer-name))
;; 		(progn
;; 		  (revert-buffer t t t)
;; 		  (message "%s reverted!" (buffer-name)))))))

(global-set-key [f5] 'my/refresh-revert)

(use-package vertico
  :after evil
  ;; (evil-collection-vertico-setup)
  ;; :after 'evil-collection
  :ensure t
  :demand t
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  (setq vertico-scroll-margin 0)
  (setq vertico-count 20)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  :bind
  (:map vertico-map
		("<escape>" . keyboard-escape-quit)
		("<return>" . vertico-exit)
		("<backspace>" . vertico-directory-delete-char)
		("S-<backspace>" . vertico-directory-up)
		("M-<backspace>" . vertico-directory-delete-word)
		("TAB" . vertico-insert)
		("C-j" . vertico-next)
		("C-k" . vertico-previous)
		("C-0" . vertico-next-group)
		("C-9" . vertico-previous-group)
		("C-n" . vertico-scroll-down)
		("C-m" . vertico-scroll-up)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Show the current function name in the header line
  (which-function-mode)
  (setq-default header-line-format
				'((which-func-mode (" " which-func-format " "))))
  (setq mode-line-misc-info
		;; We remove Which Function Mode from the mode line, because it's mostly
		;; invisible here anyway.
		(assq-delete-all 'which-function-mode mode-line-misc-info))
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
  :demand t
  :after evil
  :bind
  ("M-b" . consult-buffer)
  ("M-s" . consult-line)
  ("M-i" . consult-imenu)
  ("M-I" . consult-imenu)
  ;; ("M-f" . consult-find)
  (:map evil-normal-state-map
		("<leader> b" . consult-buffer)
		("<leader> s" . consult-line)
		("<leader> i" . consult-imenu)
		("<leader> I" . consult-imenu-multi))
		;; ("<leader> f" . consult-find))
  ;; Replace bindings. Lazily loaded due by `use-package'.
  ;; :bind (;; C-c bindings in `mode-specific-map'
         ;; ("C-c M-x" . consult-mode-command)
         ;; ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ;; ("C-c i" . consult-info)
         ;; ([remap Info-search] . consult-info)
         ;; ;; C-x bindings in `ctl-x-map'
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ;; ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; ;; M-s bindings in `search-map'
         ;; ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ;; ("M-s c" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         ;; :map isearch-mode-map
         ;; ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ;; ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; ;; Minibuffer history
         ;; :map minibuffer-local-map
         ;; ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ;; ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  (setq completion-in-region-function
		(lambda (&rest args)
		  (apply (if vertico-mode
					 #'consult-completion-in-region
				   #'completion--in-region)
				 args)))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

(use-package ivy
  :ensure t)
;;   :bind
;;   (:map minibuffer-mode-map
;; 		("C-j" . ivy-next-line)
;; 		("C-k" . ivy-previous-line)
;; 		("TAB" . ivy-alt-done)))
;;   ;; :after (:all evil all-the-icons)
;;   :diminish ivy-mode
;;   :bind
;;   (("C-s" . swiper)
;;    :map evil-normal-state-map
;;    ("<leader> s" . swiper))
   ;; ("\\ s" . swiper)
  ;;  :map evil-visual-state-map
  ;;  ("<leader> s" . swiper)
  ;;  ("\\ s" . swiper)
  ;;  :map ivy-minibuffer-map
  ;;  ("TAB" . ivy-alt-done)
  ;;  ("C-l" . ivy-alt-done)
  ;;  ("C-j" . ivy-next-line)
  ;;  ("C-k" . ivy-previous-line)
  ;;  :map ivy-switch-buffer-map
  ;;  ("C-j" . ivy-next-line)
  ;;  ("C-k" . ivy-previous-line)
  ;;  ("C-l" . ivy-done)
  ;;  ("C-d" . ivy-switch-buffer-kill)
  ;;  :map ivy-reverse-i-search-map
  ;;  ("C-j" . ivy-next-line)
  ;;  ("C-k" . ivy-previous-line)
  ;;  ("C-d" . ivy-reverse-i-search-kill))
  ;; :config
  ;; (setq ivy-use-virtual-buffers t)
  ;; (setq ivy-count-format "(%d/%d) ")
  ;; (ivy-mode 1)

;; (use-package all-the-icons-ivy
;;   :ensure t
;;   :hook
;;   (after-init . all-the-icons-ivy-setup))

;; (use-package counsel
;;   :ensure t
;;   ;; :after evil
;;   :bind
;;   (("M-x" . counsel-M-x)
;;    ;; ("C-x b" . counsel-switch-buffer)
;;    ("M-b" . counsel-switch-buffer)
;;    ;; ("C-x C-m," . counsel-bookmark)
;;    ;; ("C-x C-f" . counsel-find-file)
;;    ("M-f" . counsel-find-file)
;;    ("M-m" . counsel-bookmark)
;;    ("M-d" . counsel-dired)
;;    ("M-r" . counsel-recentf)
;;    :map evil-normal-state-map
;;    ("<leader> r" . counsel-recentf)
;;    ("<leader> i" . counsel-imenu)
;;    ("<leader> m" . counsel-bookmark)
;;    ("<leader> f" . counsel-find-file)
;;    ("<leader> d" . counsel-dired)
;;    ("<leader> b" . counsel-switch-buffer)
;;    :map evil-visual-state-map
;;    ("<leader> r" . counsel-recentf)
;;    ("<leader> i" . counsel-imenu)
;;    ("<leader> m" . counsel-bookmark)
;;    ("<leader> d" . counsel-dired)
;;    ("<leader> f" . counsel-find-file)
;;    ("<leader> b" . counsel-switch-buffer)))
  ;; I dont even use these
   ;; :map dired-mode-map
   ;; ("c" . counsel-find-file)
   ;; :map minibuffer-local-map
   ;; ("C-r" . 'counsel-minibuffer-history)))
;; (use-package counsel
;;   :ensure t
;;   :bind
;;   ("M-i" . counsel-imenu)
;;   (:map evil-normal-state-map
;; 		("<leader> i" . counsel-imenu)
;; 		:map evil-visual-state-map
;; 		("<leader> i" . counsel-imenu)))

(use-package markdown-mode
  :ensure t
  :diminish (markdown-mode gfm-mode)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package vmd-mode ;; enable to begin previewing markdown
  :ensure t)

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook
  (emacs-lisp-mode . rainbow-mode)
  (text-mode . rainbow-mode)
  (lisp-mode . rainbow-mode))

;;  ;; apparently this isn't in the public packages
;;  ;; (use-package undo-browse
;;  ;;   :ensure t)

;; ;; AESTHETICS ;;

;; ;; (use-package yascroll
;; ;;   :ensure t
;; ;;   :config (global-yascroll-bar-mode 1))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
		doom-themes-enable-italic t)
  ;; load theme in `local.el' now
  ;; (load-theme 'doom-solarized-dark t) ;; see `https://github.com/hlissner/emacs-doom-themes' for themes
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config) ;; enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  ;; :hook
  ;; (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-support-imenu t)
  (setq doom-modeline-battery t)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-modal-icon t))

;; RSS nonsense
;; (use-package elfeed
;;   :ensure t
;;   :config
;;   (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
;; 	elfeed-show-entry-switch 'display-buffer)
;;   :bind
;;   ("C-c r" . elfeed)
;; )

;; (use-package elfeed-org
;;   :ensure t
;;   :config
;;   (setq elfeed-show-entry-switch 'display-buffer)
;;   (setq rmh-elfeed-org-files (list "elfeed.org"))
;; )

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-response-timeout 1)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.01)
  (setq lsp-progress-spinner-type 'rotating-line)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol
  :bind
  (:map evil-normal-state-map
		("<leader> w" . lsp-ivy-workspace-symbol)
		:map evil-visual-state-map
		("<leader> w" . lsp-ivy-workspace-symbol)))

(use-package gdscript-mode
  :straight (gdscript-mode
			 :type git
			 :host github
			 :repo "godotengine/emacs-gdscript-mode")
  :hook (gdscript-mode . lsp-deferred))

;; THIS CARRIES--see .dir-locals.el
(defun my/headless-godot-editor (&optional quiet)
  "Hand over the lsp and no-one gets hurt >:^("
  (interactive '(gdscript-mode))
  (if (gdscript-util--find-project-configuration-file)
	  (let ((gdscript-buffer-name (format "*GDScript LSP -- %s*" (gdscript-util--get-godot-project-name)))
			(gdscript-process "GDScript LSP")
			(gdscript-project-file (format "%s/project.godot" (gdscript-util--find-project-configuration-file))))
		(if (not (get-buffer gdscript-buffer-name))
			(progn
			  (start-process gdscript-process gdscript-buffer-name gdscript-godot-executable "-e" "--headless" gdscript-project-file)
			  (message "%s started!" gdscript-process))
		  (if (not quiet)
			  (message "%s already exists!" gdscript-process))))))

(defun my/kill-headless-godot-editor ()
  "STOP THAT LSP!"
  (interactive)
  (kill-buffer (format "*GDScript LSP -- %s*" (gdscript-util--get-godot-project-name))))

(bind-keys :prefix-map my-gdscript-command-map
		   :prefix "C-c g"
		   ("e" . my/headless-godot-editor)
		   ("k" . my/kill-headless-godot-editor)
		   ("r" . gdscript-godot-run-project-debug)
		   ("o" . gdscript-godot-open-project-in-editor)
		   ("d" . gdscript-debug-make-server)) ;; this DOES NOT seem to work???

(defun lsp--gdscript-ignore-errors (original-function &rest args)
  "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
  (if (string-equal major-mode "gdscript-mode")
	  (let ((json-data (nth 0 args)))
		(if (and (string= (gethash "jsonrpc" json-data "") "2.0")
				 (not (gethash "id" json-data nil))
				 (not (gethash "method" json-data nil)))
			nil ; (message "Method not found")
		  (apply original-function args)))
	(apply original-function args)))
;; Runs the function `lsp--gdscript-ignore-errors` around `lsp--get-message-type` to suppress unknown notification errors.
(advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors)


;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq initial-buffer-choice (lambda () (get-buffer "*scratch*")))
;; (setq initial-buffer-choice #'list-packages)
;; (setq initial-buffer-choice (lambda () (list-packages)(package-menu-filter-by-status '("installed" "dependency" "buillt-in"))(package-menu-filter-upgradeable)))
(setq gc-cons-threshod (* 2 1000 1000))
