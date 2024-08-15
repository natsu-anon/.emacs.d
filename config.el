;; EMACS CONFIG
; see `https://blog.aaronbieber.com/2015/05/24/fronmnent variable HOME, (at Users/name or wherever), create .emacs.d there
;; remember -- you _MUST_ run 'all-the-icons-install-fonts' AND 'nerd-icons-install-fonts' then install the fonts to get that working ;; NOTE now run `nerd-icons-install-fonts' then install the fonts to get that working
;; see 'https://www.emacswiki.org/emacs/BookMarks' for bookmark usage
;; NOTE sometimes you gotta run package-refresh-contents

;; (uinversal-argument) into (rectangle-number-lines)

;; NOTE see 'https://irreal.org/blog' for some emacs stuff
;; NOTE see 'https://codelearn.me/' for more emacs stuff

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
(set-fringe-mode 0)
(menu-bar-mode -1)
;; (setq display-time-day-and-date 0)
;; (display-time)
(setq visible-bell t)
(show-paren-mode 1)
(setq show-paren-delay 0)
(setq warning-minimum-level :error)

;; truncate regular lines
;; (set-default 'truncate-lines nil)
;; don't truncate minibuffers
;; (add-hook 'minibuffer-setup-hook (lambda () (setq truncate-lines nil)))

;; WRAP CHADS
(set-default 'truncate-lines nil)


;; NO PROMPT WARRANTS MORE THAN 1 CHARACTER
;; LOOK! FSET!!!!
(fset 'yes-or-no-p 'y-or-n-p)

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
(setq shell-kill-buffer-on-exit t)
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

(defun temp-notes ()
  "Brings up `*TEMP-NOTES*' in org-mode"
  (interactive)
  (switch-to-buffer (get-buffer-create "*TEMP-NOTES*"))
  (org-mode))

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
;; (add-to-list 'display-buffer-alist '("*Async Shell Command*" . (display-buffer-no-window . nil)))

;; disbale blinking cursor (i.e. it stops blinking after 0 blinks)
(blink-cursor-mode 0)

;; allow remembering risky commands
(advice-add 'risky-local-variable-p :override #'ignore)

;; turn on RELATIVE line numbers
;; NOTE: visual is better than relative for navigating w/ code folds
(column-number-mode)
(setq display-line-numbers 'relative)
(display-line-numbers-mode)

(defun toggle-linums ()
  "Toggle between relative & absolute line numbers."
  (interactive)
  (if (eq display-line-numbers t)
	  (setq display-line-numbers 'relative)
	(setq display-line-numbers t)))

(defun absolute-linums ()
  "set absolute line numbers."
  (setq display-line-numbers t))

(defun relative-linums ()
  "set relative line numbers."
  (setq display-line-numbers 'relative))

;; HAND OVER THE RELATIVE LINUMS NOW!
(add-hook 'prog-mode-hook 'relative-linums)

(defun obs-1080p ()
  "set the frame size to 1900x1080 pixels because for some raisin emacs gives a free 20 horizontal pixels."
  (interactive)
  (set-frame-size (selected-frame) 1920 1080 t))

(defun obs-720p ()
  "set the frame size to 1900x1080 pixels because for some raisin emacs gives a free 20 horizontal pixels."
  (interactive)
  (set-frame-size (selected-frame) 1280 720 t))

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

;; I'VE TAKEN THE RIPGREP PILL
;; (setq grep-command "rg -nS --no-heading ")
;; (setq grep-use-null-device nil)


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

;; (use-package eldoc
;;   :hook (eldoc-mode . (lambda () (setq-local truncate-lines (not (eldoc-mode))))))

(use-package gnu-elpa-keyring-update
  :ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (setq auto-package-update-prompt-before-update t)
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
  "Toggle the ibuffer list."
  (interactive)
  (if (string= (buffer-name) "*Ibuffer*")
	  (kill-buffer-and-window)
	(progn
	  (ibuffer-list-buffers)
	  (pop-to-buffer "*Ibuffer*")
	  (ibuffer-mark-unsaved-buffers))))

(defun my/rectangle-number-lines (&optional point mark)
  "GOD I NEEDED THIS."
  (interactive "r")
  ;; (universal-argument) ;; lmao doesn't work GOOD TO KNOW THO--BOUND TO C-u BY DEFAULT--ITS NIFTY!
  (rectangle-number-lines point mark (read-number "First digit: " 0) (read-string "Format: " "%d")))

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
;; (defun message-and-kill (buffer)
;;   ""
;;   (unless (get-buffer-window buffer)
;; 	(message "killing: %s" (buffer-name buffer))
;; 	(kill-buffer buffer)))

;; NOTE: NONE of these are working??? Nor is `consult-buffer'????? wtf?
(defun my/switch-to-buffer ()
  "Show `switch-to-buffer' results in the current window"
  (interactive)
  (let ((window (selected-window)))
	(minibuffer-with-setup-hook
		(lambda ()
		  ;; (setq-local inhibit-modification-)
		  (add-hook 'after-change-functions
					(lambda (beg end len)
					  (message (minibuffer-contents))
					  (set-window-buffer window (get-buffer (minibuffer-contents))))))
					  ;; (set-window-buffer window (get-buffer "*scratch*")))))
	  (message "LETS GO?")
	  (switch-to-buffer (read-buffer "Switch to buffer: ") `(,(window-buffer window))))))

(defun my/find-file ()
  "Show `find-file' results (only with full matches) with a psuedo-temporary buffer in current window."
  (interactive)
  (let ((window (selected-window))
		(current-buffers '())
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

;; not working on laptop :(
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


(defun rename-this-file (&optional new-name)
  "Rename buffer's current file to NEW-NAME."
  (interactive "*FRename current file to: ")
  (when (buffer-file-name)
	(if (file-exists-p new-name)
		(message "%s already exists!" new-name)
	  (let ((old-buffer (buffer-name)))
		(rename-file (buffer-file-name) new-name)
		(find-file new-name)
		(kill-buffer old-buffer)))))

(defun my/shell-command-region (&optional point mark)
  (interactive "r")
  (let ((command (buffer-substring point mark)))
	(shell-command command)))

;; ls /home/jon/

(defun my/goto-eldoc ()
  "Go to the eldoc window in frame."
  (interactive)
  (if (get-buffer-window (eldoc-doc-buffer))
    (select-window (get-buffer-window (eldoc-doc-buffer)))))

(use-package evil
  :ensure t
  ;; :demand t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (define-prefix-command 'my-tab-prefix)
  (define-prefix-command 'my-help-prefix)
  (setq evil-undo-system 'undo-redo)
  ;; (define-prefix-command 'my-consult-buffer-prefix)
  ;; (global-set-key (kbd "C-s") 'my-consult-buffer-prefix)
  ;; (define-prefix-command 'my-leader)
  ;; (define-prefix-command 'my-more-searching)
  ;; (global-set-key (kbd "SPC e") 'my-emacs-leader)
  (evil-mode)
  :config
  ;; (setq evil-undo-system 'undo-redo)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  (evil-global-set-key 'normal (kbd "C-t") 'my-tab-prefix)
  (evil-global-set-key 'normal (kbd "<leader> h") 'my-help-prefix)
  ;; (global-set-key (kbd "C-t") 'my-shell-prefix)
  :bind
  ("M-e" . eval-last-sexp)
  ("M-f" . my/find-file)
  ;; ("C-c d" . my/dired)
  ;; ("C-c d" . dired)
  ;; ("C-c D" . find-name-dired)
  ;; ("M-b" . switch-to-buffer)
  (:map evil-normal-state-map
		("<leader> x" . eval-last-sexp)
		("g K" . my/goto-eldoc)
		("\\ b" . my/ibuffer-toggle)
		("\\ r" . rename-this-file)
		("\\ p" . list-processes)
		("\\ s" . scratch-buffer)
		;; ("\\ h" . doc)
		("C-w V" . my/vsplit-then-move-right)
		("C-w S" . my/split-then-move-down)
		("\\ l" . toggle-linums)
		;; RETVRN TO VIM MOTIONS
		;; ("<leader> t" . tab-bar-select-tab) ;; press numbers THEN <leader> t
		;; ("<leader> h" . evil-first-non-blank) use '_'
		;; ("<leader> l" . evil-end-of-line) use '$'
		("<leader> h v" . describe-variable)
		("<leader> h f" . describe-function)
		("<leader> h k" . describe-key)
		("<leader> h o" . describe-symbol)
		;; ("<leader> r" . undo-redo)
		("-" . dired-jump)
		("\\ d" . dired-jump)
		("\\ D" . my/dired-recursive)
		("C-t n" . tab-bar-new-tab)
		("C-t q" . tab-bar-close-tab)
		;; ("g s" . shell)
		("g S" . project-shell)
		;; ("C-t v" . my/shell-right)
		;; ("C-t s" . my/shell-down)
		("<leader> f" . find-file)
		;; ("<leader> d" . dired)
		;; ("<leader> D" . find-name-dired)
		;; ("<leader> m" . evil-goto-mark-line)
		;; ("<leader> M" . evil-goto-mark)
		("\\ '" . evil-show-marks)
		("\\ f" . evil-show-files)
		;; ("g b" . switch-to-prev-buffer) ;; use [ b
		;; ("g B" . switch-to-next-buffer) ;; use ] b
		;; ("<leader> q" . evil-quit) ;; lmao wtf am I thinkign?
		("<leader> K" . kill-buffer-and-window) ;; not bad tbqh
		("<leader> b" . switch-to-buffer)
		("<leader> r" . recentf)
		:map evil-visual-state-map
		("\\ #" . my/rectangle-number-lines)
		("<leader> x" . eval-region)
		("<leader> h" . evil-first-non-blank)
		("<leader> l" . evil-end-of-line)
		;; ("<leader> m" . evil-goto-mark-line)
		;; ("<leader> M" . evil-goto-mark)
		("<leader> !" . my/shell-command-region)
		("\\ b" . my/ibuffer-toggle)
		("\\ l" . toggle-linums)
		("C-w V" . my/vsplit-then-move-right)
		("C-w S" . my/split-then-move-down)))

(use-package shell-here
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map
			  ("g s" . shell-here)))

;; temporarily highlights modified regions
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

;; TODO leader keybinds
(use-package org
  :after evil
  :ensure t
  :init
  (setq org-M-RET-may-split-line nil)
  :config
  (define-prefix-command 'my-org-prefix)
  (evil-global-set-key 'normal (kbd "<leader> u") 'my-org-prefix)
  :bind
  (:map evil-normal-state-map
		("<leader> u h" . org-do-promote)
		("<leader> u l" . org-do-demote)
		("<leader> u #" . org-update-checkbox-count)
		("<leader> u RET" . org-toggle-checkbox)
		("<leader> u i" . org-insert-item)
		("<leader> u A" . org-insert-heading)
		("<leader> u a" . org-insert-subheading)
		("<leader> u t" . org-insert-todo-subheading)
		("<leader> u T" . org-insert-todo-heading)))

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
  ("M-f". yas-expand)
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
		("<leader> c c" . evilnc-copy-and-comment-lines)
		("<leader> c b" . evilnc-comment-box)))

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
  ;; (unbind-key (kbd "SPC") dired-mode-map)
  (evil-collection-define-key 'normal 'dired-mode-map
	;; "<leader>" nil
	;; "SPC" nil
	" " nil
	"h" 'dired-up-directory
	"l" 'dired-find-file
	"c" 'my/dired-create
	"z c" 'dired-kill-subdir
	"z o" 'dired-maybe-insert-subdir)
  (define-key dired-mode-map (kbd "SPC") nil)
  :bind
  ("C-x D" . find-name-dired))

(use-package tex-mode
  :ensure nil
  :hook
  (tex-mode . company-mode))

  ;; :bind
  ;; (:map evil-normal-state-map ;; DO NOT DO THIS ITS SO BAD GOD STOP
  ;; 		;; ("h" . dired-up-directory)
  ;; 		;; ("l" . dired-find-file)
  ;; 		;; ("c" . my/dired-create)
  ;; 		("z o" . dired-maybe-insert-subdir)))

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

;; (defun sp-wrap-grave (&optional arg)
;;   "bruh"
;;   (interactive "P")
;;   (sp-wrap-with-pair "`"))

;; ;; see `https://github.com/Fuco1/smartparens/wiki'
(use-package smartparens
  ;; :after evil
  :ensure t
  :diminish smartparens-mode
  :hook ; honestly. I just want it on all the time
  (prog-mode . smartparens-mode)
  (text-mode . smartparens-mode)
  :init
  ;; (smartparens-mode)
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

(use-package lua-mode
  :ensure t
  :diminish lua-mode
  :config
  (setq lua-indent-level 2))

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

;; flymake chads now
;; (use-package flycheck
;;   ;; :after evil
;;   :ensure t
;;   :config
;;   (setq flycheck-python-flake8-executable "c:/Python39/Scripts/flake8.exe")
;;   :hook
;;   ;; (lisp-mode . flycheck-mode)
;;   (json-mode . flycheck-mode)
;;   (prog-mode . flycheck-mode)
;;   (LaTeX-mode . flycheck-mode)
;;   :bind
;;   ("<f8>" . flycheck-mode)
;;   (:map evil-normal-state-map
;; 		("<leader> e" . flycheck-next-error)
;; 		("<leader> E" . flycheck-previous-error)))

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

(use-package all-the-icons ;; you HAVE to install the fonts for windows (run all-the-icons-install-fonts)
  :ensure t)

(use-package all-the-icons-ibuffer
  :ensure t
  :after all-the-icons
  :hook
  (ibuffer-mode . all-the-icons-ibuffer-mode))

;; use '-' and '|' to open node in  splits & vsplits respectively
;; (use-package neotree
;;   ;; :after evil
;;   :ensure t
;;   :init
;;   (setq neo-smart-open t)
;;   (setq neo-banner-message "'U' to go up a dir")
;;   (setq neo-window-position 'right)
;;   (setq neo-autorefresh t)
;;   (setq projectile-switch-project-action 'projectile-dired)
;;   (setq-default neo-show-updir-line t)
;;   (setq-default neo-show-slash-for-folder t)
;;   (setq-default neo-show-hidden-files nil)
;;   (setq neo-window-fixed-size nil)
;;   (setq neo-theme (if (display-graphic-p) 'classic 'ascii))
;;   :bind
;;   ("<f10>" . neotree-hidden-file-toggle)
;;   (:map evil-normal-state-map
;; 		("\\ t" . neotree-toggle)
;; 		:map evil-visual-state-map
;; 		("\\ t" . neotree-toggle)))

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


(use-package projectile
  :ensure t
  ;; :after rg
  ;; :commands projectile-command-map
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'auto)
  (setq projectile-switch-project-action #'projectile-dired)
  (define-prefix-command 'projectile-prefix)
  (projectile-mode)
  :config
  (evil-global-set-key 'normal (kbd "<leader> p") 'projectile-command-map) ;; why here?
  (projectile-mode 1)
  :bind
  ("C-c p" . projectile-command-map)
  (:map evil-normal-state-map
		("g a" . projectile-find-other-file)
		("<leader> o" . projectile-find-file)
		("<leader> B" . projectile-switch-to-buffer)))

(use-package rg
  :ensure t
  :bind
  ("C-c s" . rg))

(use-package ag
  :init
  (setq ag-highlight-search t)
  (setq ag-reuse-window t))

;; (use-package flycheck-projectile
;;   :after projectile
;;   :load-path "~/.emacs.d/packages"
;;   :bind ("C-c e" . flycheck-projectile-list-errors))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-set-navigator t)
  (setq dashboard-startup-banner "~/.emacs.d/ascii-art.txt")
  (setq dashboard-set-footer t)
  (setq dashboard-footer-messages '("M-x 'list-packages' 'U' 'x'"))
  (setq dashboard-center-content t)
  ;; (setq dashboard-icon-type 'all-the-icons)
  ;; (setq dashboard-set-heading-icons t)
  ;; (setq dashboard-set-file-icons t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-items '((bookmarks . 10)
						  (projects . 20)))
  ;; (add-to-list 'dashboard-items '(agenda) t)
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
  :straight (vertico
			 :type git
			 :host github
			 :repo "minad/vertico")
  :after orderless
  ;; (evil-collection-vertico-setup)
  ;; :after 'evil-collection
  ;; :ensure t
  :demand t
  :init
  (vertico-mode)
  ;; (vertico-mouse-mode)
  (setq vertico-scroll-margin 0)
  (setq vertico-count 20)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  :bind
  (:map vertico-map
		("<escape>" . keyboard-escape-quit)
		;; ("<return>" . vertico-exit)
		;; ("<backspace>" . vertico-directory-delete-char)
		;; ("S-<backspace>" . vertico-directory-up)
		;; ("M-<backspace>" . vertico-directory-delete-word)
		("TAB" . vertico-insert)
		("C-j" . vertico-next)
		("C-k" . vertico-previous)
		("C-0" . vertico-next-group)
		("C-9" . vertico-previous-group)
		("C-n" . vertico-scroll-down)
		("C-m" . vertico-scroll-up)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  (:map vertico-map
		("<return>" . vertico-directory-enter)
		("<backspace>" . vertico-directory-delete-char)
		("M-<backspace>" . vertico-directory-delete-word)))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
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

(use-package flymake
  :bind
  ("C-c d" . flymake-show-buffer-diagnostics)
  ("C-c D" . flymake-show-project-diagnostics)
  (:map evil-normal-state-map
		("[ d" . flymake-goto-prev-error)
		("] d" . flymake-goto-next-error))
  :hook
  (lisp-mode . flymake-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :after vertico
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :commands (marginalia--orig-completion-metadata-get)
  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode)
  :config
  (setq marginalia-align 'right))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(defun my/completing-read (prompt default)
  (completing-read prompt `(,default)))

(defun my/consult-rg (query)
  (if (projectile-project-p)
	  (consult-ripgrep (project-root (project-current)) query)
	(if (file-name-directory buffer-file-name)
		(consult-ripgrep (file-name-directory buffer-file-name) query)
	  (consult-ripgrep default-directory query))))

(defun my/normal-rg ()
  (interactive)
  ;; (my/consult-rg (my/completing-read "ripgrep: " (if (symbol-at-point) (symbol-at-point) ""))))
  (my/consult-rg (if (thing-at-point 'symbol t) (thing-at-point 'symbol t) "")))

(defun my/visual-rg ()
  (interactive)
  (let ((query (buffer-substring-no-properties (region-beginning) (region-end))))
	(evil-force-normal-state)
	(my/consult-rg query)))


;; Example configuration for Consult
(use-package consult
  :straight (consult
			 :type git
			 :host github
			 :repo "minad/consult")
  :ensure t
  :after evil
  :bind
  ;; ("M-s" . consult-buffer)
  ("M-m" . consult-bookmark)
  ;; ("M-S" . consult-buffer-other-window)
  ;; ("M-p" . consult-project-buffer)
  ("M-j" . consult-line)
  ("M-i" . consult-imenu)
  ("M-I" . consult-imenu-multi)
  ("M-e" . consult-flymake)
  ("M-E" . my/consult-flymake-t)
  ("C-c m" . consult-man)
  ;; ("M-f" . consult-find)
  (:map evil-normal-state-map
		;; ("<leader> s" . consult-buffer)
		("<leader> m" . consult-bookmark)
		;; ("<leader> S" . consult-buffer-other-window)
		;; ("<leader> p" . consult-project-buffer)
		("<leader> s" . my/normal-rg)
		("<leader> S" . consult-ripgrep)
		;; ("<leader> S" . consult-git-grep)
		("<leader> j" . consult-line)
		("<leader> J" . consult-line-multi)
		("<leader> i" . consult-imenu)
		("<leader> I" . consult-imenu-multi)
		("<leader> d" . consult-flymake)
		("<leader> D" . my/consult-flymake-t))
  (:map evil-visual-state-map
		("<leader> s" . my/visual-rg))
  ;; ("<leader> f" . consult-find))
  ;; Replace bindings. Lazily loaded due by `use-package'.
  ;; :bind (;; C-c bindings in `mode-specific-map'
  ;; ("C-c M-x" . consult-mode-command)
  ;; ("C-c h" . consult-history)
  ;; ("C-c k" . consult-kmacro)
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
  (add-to-list 'consult-buffer-sources 'consult--source-hidden-buffer )
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq completion-ignore-case t)
  (defun my/consult-flymake-t ()
	"project-wide consult-flymake"
	(interactive)
	(consult-flymake t))
  (setq completion-in-region-function
		(lambda (&rest args)
		  (apply (if vertico-mode
					 #'consult-completion-in-region
				   #'completion--in-region)
				 args)))

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  (setq consult-preview-key 'any)
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
  ;; (setq consult-narrow-key "<") ;; "C-+"
  (setq consult-narrow-key "C-+") ;; "<"

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

;; (use-package ivy
;;   :ensure t)
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
  :straight (doom-themes
			 :type git
			 :host github
			 :repo "doomemacs/themes")
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

;; lmao no
;; (use-package nix-mode
;;   :ensure t
;;   :mode "\\.nix\\'")

;; this one is working on windows yolo
(use-package lsp-mode
  :ensure t
  :after evil
  :init
  ;; (setq lsp-keymap-prefix "SPC l") ;; DONT DO THIS ITS AN AWFUL MISTAKE
  (setq lsp-keymap-prefix "C-l")
  :config
  (setq lsp-response-timeout 1)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-idle-delay 0.01)
  (setq lsp-progress-spinner-type 'rotating-line)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands (lsp-ui-mode lsp-ui-doc-mode)
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-mode . lsp-ui-doc-mode))

;; (use-package lsp-ivy
;;   :after lsp-mode
;;   :commands lsp-ivy-workspace-symbol
;;   :bind
;;   (:map evil-normal-state-map
;; 		("<leader> w" . lsp-ivy-workspace-symbol)
;; 		:map evil-visual-state-map
;; 		("<leader> w" . lsp-ivy-workspace-symbol)))

;; eglot config -- BUILT IN WOW!
(use-package eglot
  :ensure t
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-report-progress nil)
  (setq eglot-events-buffer-size 0)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq read-process-output-max (* 1024 1024)))

(defun lsp-however ()
  "Launch lsp with either eglot or lsp-mode"
  (if (bound-and-true-p use-lsp-mode)
	  (lsp-deferred)
	(eglot-ensure)))

(defun my/lsp-rename (newname)
  "Rename."
  (interactive
   (list (read-from-minibuffer
          (format "Rename `%s' to: " (or (thing-at-point 'symbol t)
                                         "unknown symbol"))
          nil nil nil nil
          (symbol-name (symbol-at-point)))))
  (if (bound-and-true-p use-lsp-mode)
	  (lsp-rename newname)
	(eglot-rename newname)))

(defun my/lsp-quickfix (BEG &optional END)
  "Quickfix."
  (interactive (and (region-active-p) (list (region-beginning) (region-end))))
  (if (bound-and-true-p use-lsp-mode)
	  (lsp-eslint-fix-all)
	(eglot-code-action-quickfix BEG END)))

(bind-keys :menu-name "LSP commands"
		   :prefix-map my/lsp-map
		   :prefix "C-c l"
		   ("r" . my/lsp-rename)
		   ("q" . my/lsp-quickfix))

(use-package company
  :after eglot
  :ensure t
  :config
  (defun disable-auto-complete-mode ()
	"disables auto-complete so that way company mode takes over"
	(auto-complete-mode -1)
	(message "auto-complete-mode disabled in current buffer!"))
  (setq company-idle-delay 0.00)
  (setq company-tooltip-idle-delay 0.00)
  :hook
  (company-mode . disable-auto-complete-mode)
  (lisp-mode . company-mode)
  (c++-mode . company-mode))

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
		   ("e" . my/headless-godot-editor)
		   ("k" . my/kill-headless-godot-editor)
		   ("r" . gdscript-godot-run-project-debug)
		   ("o" . gdscript-godot-open-project-in-editor)
		   ("d" . gdscript-debug-make-server)) ;; this is for DAP

(use-package gdscript-mode
  :after eglot
  :straight (gdscript-mode
			 :type git
			 :host github
			 :repo "godotengine/emacs-gdscript-mode")
  ;; :hook (gdscript-mode . lsp-deferred))
  :commands (gdscript-util--find-project-configuration-file
			 gdscript-godot-run-prject-debug
			 gdscript-godot-open-project-in-editor
			 gdscript-debug-make-server)
  :config
  (defun my/godot-project-setup ()
	"Stuff to do when projectile switches to a Godot project."
	(when (gdscript-util--find-project-configuration-file)
	  (my/headless-godot-editor t)))
  ;; (push (cons 'gdscript-mode `(,gdscript-godot-executable "-e" "--headless" ,gdscript-util--find-project-configuration-file)) eglot-server-programs)
  (assq-delete-all 'gdscript-mode eglot-server-programs)
  ;; (add-to-list 'eglot-server-programs '(gdscript-mode . ("localhost" 6008)))
  (add-to-list 'eglot-server-programs '(gdscript-mode . ("netcat" "localhost" "6008")))
  :hook
  ;; SET IN .dir-locals.el -- eglot not working with godot lsp >:^(
  ;; (gdscript-mode . lsp-however) ;; BRUH
  ;; (gdscript-mode . lsp)
  ;; (gdscript-mode . my/check-headless-godot)
  ;; (gdscript-mode . my/headless-godot-editor)
  (gdscript-mode . eglot-ensure)
  (gdscript-mode . company-mode)
  (projectile-after-switch-project . my/godot-project-setup)
  ;; (gdscript-mode . (lambda () (message "foo bar gdscript-mode")))
  ;; (gdscript-mode . (lambda ()
  ;; 					 (my/start-headless-godot-if-no-existing-godot)
  ;; 					 (eglot-ensure)))
  ;; NOTE: you only need this if using godot 3
  ;; :custom (gdscript-eglot-version 3))
  )

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


(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;; (setq initial-buffer-choice (lambda () (get-buffer "*scratch*")))
;; (setq initial-buffer-choice #'list-packages)
;; (setq initial-buffer-choice (lambda () (list-packages)(package-menu-filter-by-status '("installed" "dependency" "buillt-in"))(package-menu-filter-upgradeable)))
(setq gc-cons-threshod (* 2 1000 1000))
