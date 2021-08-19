;; TODO ggtags
;; see `https://blog.aaronbieber.com/2015/05/24/from-vim-to-emacs-in-fourteen-days.html' for the gestalt
;; add emacs dir with the binary to the PATH
;; create an einvornmnent variable HOME, (at Users/name or wherever), create .emacs.d there
;; remember -- you _MUST_ run `all-the-icons-install-fonts' then install the fonts to get that working
;; see `https://www.emacswiki.org/emacs/BookMarks' for bookmark usage

;; Make startup faster by reducing the frequency of gc.  Default is 800kb -- measured in bytes.
(setq gc-cons-threshod (* 50 1000 1000))

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


;; NO PROMPT IS WARRANTS MORE THAN 1 CHARACTER
(defalias 'yes-or-no-p 'y-or-n-p)

;; only show trailing white space when programming
(add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))

(defun my/desktop-read ()
  "Save current desktop into the desktop directory."
  (interactive)
  (desktop-read desktop-dirname))

(global-set-key [f6] 'desktop-save-in-desktop-dir)

;; enable folding while in prog-mode
(add-hook 'prog-mode-hook (lambda ()
							(hi-lock-mode)
							(highlight-regexp "TODO")
							(highlight-regexp "NOTE")
							(highlight-regexp "FAMPAI")
							(highlight-regexp "SENPAI")
							(highlight-regexp "NOTA BENE")))

;; 80 character line in prog-mode
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))


;; make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escapequit)

(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

;; turn on line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

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

;; but disable for some modes
(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default tab-width 4)

(setq c-default-style "linux"
	  c-basic-offset 4)

;; setup use-package
;; NOTE: see `https://github.com./jweiegley/use-package' for usage
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package org
  :ensure t
  :init
  ;; (setq org-list-use-circular-motion t)
  (setq org-M-RET-may-split-line nil))

(use-package auto-complete
  :ensure t
  :bind ("<f7>" . auto-complete-mode)
  :diminish auto-complete-mode
  :hook (prog-mode . auto-complete-mode)
  :config
  (ac-config-default))

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

(use-package origami
  :ensure t
  :hook (prog-mode. origami-mode))

(use-package evil
  :ensure t
  :bind
  (:map evil-normal-state-map
		("C-w V" . my/vsplit-then-move-right)
		("C-w S" . my/split-then-move-down)
		("g t" . next-buffer)
		("g T" . previous-buffer)
		:map evil-visual-state-map
		("C-w V" . my/vsplit-then-move-right)
		("C-w S" . my/split-then-move-down))
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode))

(use-package evil-anzu
  :ensure t
  :after (:all evil anzu))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :ensure t)

;; TODO STOP EMBARASSING YOURSELF
;; naw-- just add node_modules/.bin/ to the exec-path
;; (use-package add-node-modules-path
;;   :ensure t
;;   :hook
;;   (js-mode . add-node-modules-path)
;;   (js2-mode . add-node-modules-path))

(use-package js2-mode
  :ensure t
  :init
  ;; (setq js-chain-indent t)
  ;; (setq js-indent-level 2)
  (setq js2-bounce-indent-p t))

(use-package json-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :bind ("<f8>" . flycheck-mode)
  :hook (prog-mode . flycheck-mode))

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))

;; see `https://github.com/Fuco1/smartparens/wiki'
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :bind
  ("C-l" . sp-forward-sexp)
  ("C-k" . sp-backward-sexp)
  :hook
  (prog-mode . smartparens-mode)
  :config
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  (setq sp-highlight-pair-overlay nil))

(use-package all-the-icons ;; you HAVE to install the fonts for windows (run all-the-icons-install-fonts)
  :ensure t)

;; use '-' and '|' to open node in  splits & vsplits respectively
(use-package neotree
  :after evil
  :ensure t
  :init
  (setq neo-smart-open t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq-default neo-show-hidden-files t)
  (setq neo-window-fixed-size nil)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind
  (:map evil-normal-state-map
		("\\ d" . my/desktop-read)
		("\\ t" . neotree-toggle)
		:map evil-visual-state-map
		("\\ d" . my/desktop-read)
		("\\ t" . neotree-toggle)))

(use-package solaire-mode
  :ensure t
  :diminish solaire-global-mode
  :hook (after-init . solaire-global-mode))

(use-package undo-tree
  :ensure t
  :diminish global-undo-tree-mode
  :after evil
  :bind
  (:map evil-normal-state-map
		("\\ u" . undo-tree-visualize)
		:map evil-visual-state-map
		("\\ u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.5))

(use-package magit
  :ensure t
  :after evil
  :bind
  (:map evil-normal-state-map
		("\\ g" . magit-status)
		:map evil-visual-state-map
		("\\ g" . magit-status)))

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
  :diminish projectile-mode
  :after evil
  :init
  (setq projectile-completion-system 'ivy)
  :bind
  ;; ("s-p" . projectile-command-map)
  (("C-c p" . projectile-command-map)
   :map evil-normal-state-map
   ("\\ p" . projectile-switch-project)
   :map evil-visual-state-map
   ("\\ p" . projectile-switch-project))
  :config (projectile-mode 1))


(use-package dashboard
  :ensure t
  :config
  (setq dashboard-set-navigator t)
  (setq dashboard-startup-banner "~/.emacs.d/ascii-art.txt")
  (setq dashboard-set-footer nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-items '((projects . 5)
						  (bookmarks . 5)
						  (recents . 5)))
  (add-to-list 'dashboard-items '(agenda) t)
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(defun my/refresh-revert ()
  "refresh the dashboard or revert the current buffer."
  (interactive)
  (if (string= (buffer-name) "*dashboard*")
	  (progn
		(dashboard-refresh-buffer)
		(message "%s refreshed!" (buffer-name)))
	(if (string-match "magit:.+" (buffer-name))
		(progn
		  (magit-refresh)
		  (message "%s refreshed!" (buffer-name)))
	  (if (string-match "\*.+?\*" (buffer-name))
		  (message "cannot revert %s!" (buffer-name))
		(progn
		  (revert-buffer t t t)
		  (message "%s reverted!" (buffer-name)))))))

(global-set-key [f5] 'my/refresh-revert)

(use-package ivy
  :ensure t
  :after all-the-icons
  :diminish ivy-mode
  :bind
  (("C-s" . swiper)
   :map evil-normal-state-map
   ("\\ s" . swiper)
   :map evil-visual-state-map
   ("\\ s" . swiper)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   ("C-l" . ivy-alt-done)
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   :map ivy-switch-buffer-map
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   ("C-l" . ivy-done)
   ("C-d" . ivy-switch-buffer-kill)
   :map ivy-reverse-i-search-map
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1))

(use-package all-the-icons-ivy
  :ensure t
  :hook
  (after-init . all-the-icons-ivy-setup))

(use-package counsel
  :ensure t
  :after evil
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   :map evil-normal-state-map
   ("\\ r" . counsel-recentf)
   ("\\ m" . counsel-bookmark)
   ("\\ f" . counsel-find-file)
   ("\\ b" . counsel-ibuffer)
   :map evil-visual-state-map
   ("\\ r" . counsel-recentf)
   ("\\ m" . counsel-bookmark)
   ("\\ f" . counsel-find-file)
   ("\\ b" . counsel-ibuffer)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history)))

(use-package markdown-mode
  :ensure t
  :diminish (markdown-mode gfm-mode)
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun sp-wrap-single-quote (&optional arg)
  "bruh."
  (interactive "P")
  (sp-wrap-with-pair "'"))

(defun sp-wrap-double-quote (&optional arg)
  "bruh."
  (interactive "P")
  (sp-wrap-with-pair "\""))

(use-package evil-leader ;; the leader key is SPACEBAR (default is \)
  :ensure t
  :diminish global-evil-leader-mode
  :after evil-nerd-commenter
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
	"<up>" 'flycheck-previous-error
	"<down>" 'flycheck-next-error
	"l" 'toggle-linums
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "." 'evilnc-copy-and-comment-operator
	"'" 'sp-wrap-single-quote
	"\"" 'sp-wrap-double-quote
	")" 'sp-wrap-round
	"(" 'sp-wrap-round
	"}" 'sp-wrap-curly
	"{" 'sp-wrap-curly
	"]" 'sp-wrap-square
	"[" 'sp-wrap-square)
  (evil-leader/set-key-for-mode 'org-mode
	"SPC i" 'org-insert-todo-heading
	"SPC I" 'org-insert-heading
	"SPC a" 'org-insert-heading-respect-content
	"SPC A" 'org-insert-todo-heading-respect-content
	"SPC /" 'org-update-statistics-cookies
	"SPC -" 'org-ctrl-c-minus
	"SPC c" 'org-ctrl-c-ctrl-c
	"SPC h" 'org-metaleft
	"SPC j" 'org-metadown
	"SPC k" 'org-metaup
	"SPC l" 'org-metaright
	"SPC H" 'org-shiftmetaleft
	"SPC RET" 'org-meta-return
	"SPC t" 'org-todo
	"SPC L" 'org-shiftmetaright)
  (evil-leader/set-key-for-mode 'js-mode
	"<backtab>" 'js2-indent-bounce-backward
	"SPC h" 'js2-indent-bounce-backward
	"SPC l" 'js2-indent-bounce
	"TAB" 'js2-indent-bounce)
	)

(use-package vmd-mode ;; enable to begin previewing markdown
  :ensure t)

;; AESTHETICS ;;

(use-package yascroll
  :ensure t
  :config (global-yascroll-bar-mode 1))

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
  :hook
  (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 20)
  (setq doom-modeline-bar-width 4)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-icon t)
  (setq doom-modeline-modal-icon t))

(setq gc-cons-threshod (* 2 1000 1000))
