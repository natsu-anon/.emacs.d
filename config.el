;; EMACS CONFIG
; see `https://blog.aaronbieber.com/2015/05/24/fronmnent variable HOME, (at Users/name or wherever), create .emacs.d there
;; remember -- you _MUST_ run 'all-the-icons-install-fonts' AND 'nerd-icons-install-fonts' then install the fonts to get that working ;; NOTE now run `nerd-icons-install-fonts' then install the fonts to get that working
;; see 'https://www.emacswiki.org/emacs/BookMarks' for bookmark usage
;; NOTE sometimes you gotta run package-refresh-contents

;; (uinversal-argument) into (rectangle-number-lines)

;; NOTE see 'https://irreal.org/blog' for some emacs stuff
;; NOTE see 'https://codelearn.me/' for more emacs stuff


;; NOTE: using --eval on windows: emacs -Q --batch --eval '(message \"Hi!\")'


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
(setq scroll-margin 8)
(setq hscroll-margin 4)
(setq scroll-step 1)
(setq auto-window-vscroll nil)
(setq scroll-up-aggressively 0)
(setq scroll-down-aggressively 0)
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

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(straight-use-package 'use-package)


(setq package-check-signature nil)
(use-package gnu-elpa-keyring-update
  :ensure t)

;; (use-package auto-package-update
;;   :custom
;;   (auto-package-update-interval 7)
;;   (setq auto-package-update-prompt-before-update t)
;;   (auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe)
;;   (auto-package-update-at-time "17:00"))


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

;; tab-bar memels
(setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
(setq tab-bar-close-button-show nil)       ;; hide tab close / X button
;; (setq tab-bar-new-tab-choice "*dashboard*");; buffer to show in new tabs
(setq tab-bar-new-tab-choice t)            ;; lol this is the default AND I LIKE IT
(setq tab-bar-tab-hints t)                 ;; show tab numbers
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(tab-bar-mode 1)                           ;; enable tab bar

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

(defun delete-compile-window-if-successful (buf desc)
  "Bury a compilation buffer if succeeded without warnings"
  (when (and (buffer-live-p buf) (string-match "finished" desc))
	(delete-window (get-buffer-window buf))))
(add-hook 'compilation-finish-functions #'delete-compile-window-if-successful)

(use-package emacs
  :ensure nil
  :init
  ;; Show the current function name in the header line
  ;; NOTE: use C-x t for tab stuff
  ;; (define-prefix-command 'my-tab-prefix)
  ;; (global-set-key (kbd "C-t") 'my-tab-prefix)
  (setq find-program "fd")
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
  (defun kill-current-buffer ()
	"kills the current buffer"
	(interactive)
	(kill-buffer (current-buffer)))
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
  (setq enable-recursive-minibuffers t)
  :bind-keymap
  ("C-c h" . help-map)
  :bind
  ;; ("C-t n" . tab-bar-new-tab)
  ;; ("C-t q" . tab-bar-close-tab)
  ("C-c k" . kill-current-buffer)
  ("C-c b" . my/ibuffer-toggle)
  ("C-c l" . toggle-linums))


;; (bind-keys :prefix-map help-map :prefix "C-c h")

(use-package compile
  :ensure nil
  :init
  (setq compilation-scroll-output t)
  (defun colorize-compilation-buffer ()
	(let ((inhibit-read-only t))
	  (ansi-color-apply-on-region (point-min) (point-max))))
  :hook
  (compilation-filter . colorize-compilation-buffer)
  :bind
  ("C-c r" . recompile))

(setq evil-want-keybinding nil)
(use-package evil
  :ensure t
  :commands (evil-set-leader evil-global-set-key)
  :init
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-integration t)
  (define-prefix-command 'evil-help-prefix)
  (evil-mode)
  :config
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  (evil-global-set-key 'normal (kbd "<leader> h") 'evil-help-prefix)
  (defun my/goto-eldoc ()
	"Go to the eldoc window in frame."
	(interactive)
	(if (not (get-buffer-window (eldoc-doc-buffer)))
		(eldoc t))
	(select-window (get-buffer-window (eldoc-doc-buffer))))
  (defun my/goto-help ()
	"Go to the eldoc window in frame."
	(interactive)
	(select-window (get-buffer-window (help-buffer))))
  (defun my/goto-compile-buffer ()
	"Go to the compilation buffer in frame."
	(interactive)
	(cond ((get-buffer-window "*compilation*") (select-window (get-buffer-window "*compilation*")))
		  ((buffer-live-p (get-buffer "*compilation*")) (set-window-buffer (selected-window) (get-buffer "*compilation*")))
		  (t (message "No compilation buffer!"))))
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
  ;; (defun my/rectangle-number-lines (&optional point mark)
  ;; 	"GOD I NEEDED THIS."
  ;; 	(interactive "r")
  ;; 	;; (universal-argument) ;; lmao doesn't work GOOD TO KNOW THO--BOUND TO C-u BY DEFAULT--ITS NIFTY!
  ;; 	(rectangle-number-lines point mark (read-number "First digit: " 0) (read-string "Format: " "%d")))
  (evil-define-motion my/visual-rectangle-number-lines (count)
	"breh"
	:type block
	:jump t
	(rectangle-number-lines evil-visual-beginning evil-visual-end (or count 0) "%d"))
  :bind
  ("C-c '" . evil-show-marks)
  ("C-c f" . evil-show-files)
  (:map evil-normal-state-map
		("<leader> x" . eval-last-sexp)
		("g K" . my/goto-eldoc)
		("g c" . my/goto-compile-buffer)
		("g h" . my/goto-help)
		;; ("g b" . scratch-buffer)
		("C-w V" . my/vsplit-then-move-right)
		("C-w S" . my/split-then-move-down)
		("C-h" . evil-window-left)
		("C-j" . evil-window-down)
		("C-k" . evil-window-up)
		("C-l" . evil-window-right)
		("<leader> h v" . describe-variable)
		("<leader> h f" . describe-function)
		("<leader> h k" . describe-key)
		("<leader> h o" . describe-symbol)
		("-" . dired-jump)
		("g S" . project-shell)
		("<leader> f" . find-file)
		("<leader> K" . kill-buffer-and-window) ;; not bad tbqh
		("<leader> b" . switch-to-buffer)
		("<leader> r" . recentf)
		("[ t" . tab-bar-switch-to-prev-tab)
		("] t" . tab-bar-switch-to-next-tab)
		("] q" . next-error)
		("[ q" . previous-error)
		:map evil-visual-state-map
		("#" . my/visual-rectangle-number-lines)
		("<leader> x" . eval-region)
		("<leader> h" . evil-first-non-blank)
		("<leader> l" . evil-end-of-line)
		("<leader> !" . my/shell-command-region)
		("C-w V" . my/vsplit-then-move-right)
		("C-w S" . my/split-then-move-down)))

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

(use-package org
  :ensure nil
  :init
  (define-prefix-command 'my-org-prefix)
  (global-set-key (kbd "C-a") 'my-org-prefix)
  :config
  (setq org-M-RET-may-split-line nil)
  (setq org-image-actual-width nil)
  ;; (evil-global-set-key 'normal (kbd "<leader> a") 'my-org-prefix)
  :bind
  (:map org-mode-map
		("C-a h" . org-metaleft)
		("C-a j" . org-metadown)
		("C-a k" . org-metaup)
		("C-a l" . org-metaright)
		("C-a H" . org-shiftmetaleft)
		("C-a J" . org-shiftmetadown)
		("C-a K" . org-shiftmetaup)
		("C-a L" . org-shiftmetaright)
		("C-a #" . org-update-checkbox-count)
		("C-a c" . org-toggle-checkbox)
		("C-a i" . org-insert-item)
		("C-a A" . org-insert-heading)
		("C-a a" . org-insert-subheading)
		("C-a C-t" . org-todo)
		("C-a t" . org-insert-todo-subheading)
		("C-a T" . org-insert-todo-heading)))
  ;; ("C-a h" . org-meta-left)
  ;; ("C-a j" . org-meta-down)
  ;; ("C-a k" . org-meta-up)
  ;; ("C-a l" . org-meta-right)
  ;; ("C-a H" . org-shiftmeta-left)
  ;; ("C-a J" . org-shiftmeta-down)
  ;; ("C-a K" . org-shiftmeta-up)
  ;; ("C-a L" . org-shiftmeta-right)
  ;; ("C-a #" . org-update-checkbox-count)
  ;; ("C-a c" . org-toggle-checkbox)
  ;; ("C-a i" . org-insert-item)
  ;; ("C-a A" . org-insert-heading)
  ;; ("C-a a" . org-insert-subheading)
  ;; ("C-a t" . org-insert-todo-subheading)
  ;; ("C-a T" . org-insert-todo-heading))

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

(use-package evil-collection
  :after evil
  :ensure t
  :config
  ;; prevent overwriting bindings for next-error and previous-error set earlier
  (setq evil-collection-key-blacklist '("[ q" "] q"))
  (evil-collection-init))


;; put it after evil-collection PLS
(use-package comint
  :ensure nil
  :config
  (defun append-at-last-prompt ()
	"jump to last comint prompt"
	(interactive)
	(goto-char (cdr comint-last-prompt))
	(evil-append-line nil))
  (evil-collection-define-key 'normal 'comint-mode-map
	"q" 'quit-window
	"A" 'append-at-last-prompt))

(use-package shell-here
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map
			  ("g s" . shell-here)))

(use-package capf-autosuggest
  :ensure t
  :hook
  (shell-mode . capf-autosuggest-mode))

(use-package evil-nerd-commenter
  :after evil
  :ensure t
  :config
  ;; package wasn't automatically addingin the comment text object; do it manually
  (define-key evil-inner-text-objects-map evilnc-comment-text-object 'evilnc-inner-commenter)
  (define-key evil-outer-text-objects-map evilnc-comment-text-object 'evilnc-outer-commenter)
  (defun append-comment ()
	"append comment at eol and enter insert mode"
	(interactive)
	(atomic-change-group
	  (end-of-line)
	  (sp-comment)
	  (evil-append-line nil)))
  (defun my/visual-comment-or-uncomment-region (&optional start end)
	"comments out a region"
	(interactive "r")
	(evilnc-comment-or-uncomment-region start end))
  :bind
  (:map evil-normal-state-map
		("<leader> c a" . append-comment)
		("<leader> c i" . evilnc-comment-or-uncomment-lines)
		("<leader> c c" . evilnc-copy-and-comment-lines)
		("<leader> c p" . evilnc-comment-or-uncomment-paragraphs)
		("<leader> c b" . evilnc-comment-box) ;; tbh this works better here for some reason
		:map evil-visual-state-map
		("<leader> c i" . evilnc-comment-or-uncomment-lines)
		("<leader> c c" . evilnc-copy-and-comment-lines)
		("<leader> c r" . my/visual-comment-or-uncomment-region)))

(defun my/dired-recursive (&optional dirname)
  "Recursively open a file in dired."
  (interactive "GDired recursive (directory): ")
  (dired dirname "-laRgho"))

(defun my/dired-create (&optional arg)
  "foo"
  (interactive "GNew File/Directory: ")
  (if (or (file-exists-p arg) (f-directory-p arg))
	  (dired-goto-file arg)
	(if (directory-name-p arg)
		(dired-create-directory arg)
		(dired-create-empty-file arg)))
  (revert-buffer-quick))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-lagho --group-directories-first"))
  :hook ((dired-mode . dired-hide-details-mode)
		 (dired-mode . dired-omit-mode)
		 ;; (dired-mode . (display-line-numbers . 'visual))
		 (dired-mode . relative-linums)
		 (dired-mode . display-line-numbers-mode))
  :config
  (setq dired-auto-revert-buffer t)
  (setq dired-do-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (evil-collection-define-key 'normal 'dired-mode-map
	" " nil
	"h" 'dired-up-directory
	"l" 'dired-find-file
	"+" 'my/dired-create
	"zc" 'dired-kill-subdir
	"zo" 'dired-maybe-insert-subdir)
  (define-key dired-mode-map (kbd "SPC") nil)
  :bind
  ("C-x f" . find-name-dired)
  ("C-x D" . my/dired-recursive))
;; (find-name-dired "." "*.el")

;; NOTE: just not working well with evil & windows :(
;; (use-package dirvish
;;   :ensure t
;;   :init
;;   (setq dired-mouse-drag-files t)                   ; added in Emacs 29
;;   (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
;;   (dirvish-override-dired-mode)
;;   :config
;;   ;; (dirvish-peek-mode) ; Preview files in minibuffer
;;   ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
;;   (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
;;   (setq dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
;;   (setq delete-by-moving-to-trash t)
;;   (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group") (dirvish-override-dired-mode))

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
	"H" 'dired-hide-dotfiles-mode))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package tex-mode
  :ensure nil
  :hook
  (tex-mode . company-mode))

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
  (defun sp-wrap-single-quote (&optional arg)
	"bruh."
	(interactive "P")
	(sp-wrap-with-pair "'"))
  (defun sp-wrap-double-quote (&optional arg)
	"bruh."
	(interactive "P")
	(sp-wrap-with-pair "\""))
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  (setq sp-highlight-pair-overlay nil)
  :bind
  ("C-c C-l" . sp-forward-sexp)
  ("C-c C-k" . sp-backward-sexp)
  (:map evil-insert-state-map
		("C-;" . sp-comment)
		("C-l" . sp-forward-sexp)
		("C-h" . sp-backward-sexp)
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

(use-package solaire-mode
  :ensure t
  :diminish solaire-global-mode
  :hook (after-init . solaire-global-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.3))

;; just use C-x g damnid
(use-package magit
  :ensure t)

(use-package imenu-list
  :ensure t
  :init
  (setq imenu-list-focus-after-activation t)
  :bind
  ("C-c i" . imenu-list-minor-mode))

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
  (define-prefix-command 'projectile-prefix)
  (projectile-mode)
  :config
  (setq projectile-completion-system 'auto)
  (setq projectile-switch-project-action #'projectile-dired)
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
  :config
  (setq grep-highlight-matches t)
  (defun my/goto-rg ()
	"Go to the rg buffer"
	(interactive)
	(cond ((get-buffer-window "*rg*") (select-window (get-buffer-window "*rg*")))
		  ((buffer-live-p (get-buffer "*rg*")) (set-window-buffer (selected-window) (get-buffer "*rg*")))
		  ((t (message "No rg buffer!")))))
  :bind
  ("C-c s" . rg)
  (:map evil-normal-state-map
			  ("g /" . my/goto-rg)))

;; (use-package ag
;;   :init
;;   (setq ag-highlight-search t)
;;   (setq ag-reuse-window t))

;; NOTE: I would like it but it opens up too many buffers while navigating between results
;; (use-package deadgrep
;;   :ensure t
;;   :config
;;   (setq deadgrep-max-buffers 1)
;;   (setq grep-highlight-matches t)
;;   (defun my/goto-deadgrep ()
;; 	"go to the \"first\" deadgrep buffer"
;; 	(interactive)
;; 	(let ((buf (car (deadgrep--buffers))))
;; 	  (cond ((get-buffer-window buf) (select-window (get-buffer-window buf)))
;; 			((buffer-live-p buf) (set-window-buffer (selected-window) buf)))
;; 			(t (message "No deadgrep buffer!")))))
;;   :bind
;;   ("C-c s" . deadgrep))

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (setq dashboard-set-navigator t)
;;   (setq dashboard-startup-banner "~/.emacs.d/ascii-art.txt")
;;   (setq dashboard-set-footer t)
;;   (setq dashboard-footer-messages '("M-x 'list-packages' 'U' 'x'"))
;;   (setq dashboard-center-content t)
;;   ;; (setq dashboard-icon-type 'all-the-icons)
;;   ;; (setq dashboard-set-heading-icons t)
;;   ;; (setq dashboard-set-file-icons t)
;;   (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
;;   (setq dashboard-items '((bookmarks . 10)
;; 						  (projects . 20)))
;;   ;; (add-to-list 'dashboard-items '(agenda) t)
;;   (dashboard-setup-startup-hook))

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

(global-set-key [f5] 'my/refresh-revert)

(use-package vertico
  ;; :straight (vertico
  ;; 			 :type git
  ;; 			 :host github
  ;; 			 :repo "minad/vertico")
  :after orderless
  ;; (evil-collection-vertico-setup)
  ;; :after 'evil-collection
  ;; :ensure t
  :demand t
  :init
  (vertico-mode)
  ;; (vertico-mouse-mode)
  (setq vertico-scroll-margin 1)
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
		("C-n" . vertico-next-group)
		("C-p" . vertico-previous-group)
		("C-S-j" . vertico-scroll-down)
		("C-S-k" . vertico-scroll-up)))

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

(use-package flymake
  :config
  (defun my/flymake-diagnostics-at-point ()
	(interactive)
	;; (momentary-string-display (flymake-diagnostics (point)) (point)))
	(let ((text (flymake-diagnostic-text (car (flymake-diagnostics (point))))))
	  (put-text-property 0 (length text) 'face 'font-lock-warning-face text)
	  (save-excursion
		(end-of-line)
		(momentary-string-display (format " %s" text) (point) nil ""))))
  :bind
  ("C-c d" . flymake-show-buffer-diagnostics)
  ("C-c D" . flymake-show-project-diagnostics)
  (:map evil-normal-state-map
		("[ d" . flymake-goto-prev-error)
		("] d" . flymake-goto-next-error)
		:map flymake-mode-map
		("C-c C-d" . my/flymake-diagnostics-at-point))
  :hook
  (lisp-mode . flymake-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :after vertico
  :bind (:map minibuffer-local-map
         ("M-S-a" . marginalia-cycle))
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


(use-package consult
  :straight (consult
			 :type git
			 :host github
			 :repo "minad/consult")
  :ensure t
  :after evil
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
  (define-prefix-command 'my-consult-prefix)
  (global-set-key (kbd "C-s") 'my-consult-prefix)
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
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ("C-s b" . consult-buffer)
  ("C-s m" . consult-bookmark)
  ("C-s j" . consult-line)
  ("C-s J" . consult-line-multi)
  ("C-s i" . consult-imenu)
  ("C-s I" . consult-imenu-multi)
  ("C-s d" . consult-flymake)
  ("C-s D" . my/consult-flymake-t)
  ("C-c m" . consult-man)
  (:map evil-normal-state-map
		("<leader> m" . consult-bookmark)
		("<leader> s" . my/normal-rg)
		("<leader> S" . consult-ripgrep)
		("<leader> j" . consult-line)
		("<leader> J" . consult-line-multi)
		("<leader> i" . consult-imenu)
		("<leader> I" . consult-imenu-multi)
		("<leader> d" . consult-flymake)
		("<leader> D" . my/consult-flymake-t))
  (:map evil-visual-state-map
		("<leader> s" . my/visual-rg)))

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

;; DEPRECATED--use eglot (I just have this for a rainy day)
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

;; eglot config -- BUILT IN WOW!
(use-package eglot
  :ensure nil
  ;; :after evil
  ;; :demand t
  :init
  (define-prefix-command 'my-lsp-prefix)
  (evil-global-set-key 'normal (kbd "<leader> l") 'my-lsp-prefix)
  (evil-global-set-key 'visual (kbd "<leader> l") 'my-lsp-prefix)
  :config
  (setq eglot-autoshutdown t)
  (setq eglot-report-progress nil)
  (setq eglot-events-buffer-size 0)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq read-process-output-max (* 1024 1024))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  :bind
  (:map evil-normal-state-map
		("<leader> l r" . eglot-rename)
		("<leader> l q" . eglot-code-actions-quickfix)
		("<leader> l a" . eglot-code-actions)
		;; ("<leader> l i" . eglot-inlay-hints-mode)
		("<leader> l =" . eglot-format-buffer)
		:map evil-visual-state-map
		("<leader> l a" . eglot-code-actions)
		("<leader> l = " . eglot-format)))

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

;;;;;;;;;;;;;;;;
;; AESTHETICS ;;
;;;;;;;;;;;;;;;;

(use-package rainbow-mode
  :diminish rainbow-mode
  :hook
  (emacs-lisp-mode . rainbow-mode)
  (text-mode . rainbow-mode)
  (lisp-mode . rainbow-mode))

(use-package doom-themes
  :straight (doom-themes
			 :type git
			 :host github
			 :repo "doomemacs/themes")
  :config
  (setq doom-themes-enable-bold t
		doom-themes-enable-italic t)
  ;; load theme in `local.el' now
  (doom-themes-visual-bell-config)
  ;; (doom-themes-neotree-config) ;; enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
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


;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq server-name "server")
(server-start)
(setq initial-scratch-message "\
;; M-x list-packages then 'U' and 'x' to update packages
;; M-x projectile-switch-project
;; F1 for help
;;
;; DON'T FORGET TO SMILE

")
;; (setq initial-buffer-choice (lambda () (get-buffer "*scratch*")))
;; (setq initial-buffer-choice #'stratch-buffer)
(setq gc-cons-threshod (* 2 1000 1000))
