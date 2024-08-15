;; (defvar unreal-engine nil "place variable for unreal-engine-mode for easier yasnippet integration ")

(declare-function yas-activate-extra-mode "ext:yansippet")
(declare-function yas-deactivate-extra-mode "ext:yansippet")

(make-variable-buffer-local
 (defvar unreal-engine-module-api nil
   "the API name of the current module.  For snippets"))

(defvar unreal-engine--attributes
  '(("UCLASS")
	("UDELEGATE")
	("UENUM")
	("UFUNCTION")
	("UINTERFACE")
	("UMETA")
	("UPARAM")
	("UPROPERTY")
	("USTRUCT"))
  "FUCK IT WE BALL")

(defvar unreal-engine--delegates
  '(("DECLARE_DELEGATE")
	("DECLARE_EVENT")
	("DECLARE_DERIVED_EVENT")
	("DECLARE_DYNAMIC_DELEGATE")
	("DECLARE_MULTICAST_DELEGATE")
	("DECLARE_DYNAMIC_MULTICAST_DELEGATE"))
  "FUCK IT WE BALL.")

(defvar unreal-engine--generated-body-macro
  '(("GENERATED_BODY")
	("GENERATED_IINTERFACE_BODY")
	("GENERATED_UCLASS_BODY")
	("GENERATED_UINTERFACE_BODY")
	("GENERATED_USTRUCT_BODY"))
  "FUCK IT WE BALL.")

(defvar unreal-engine--font-lock-attributes
  '(("UCLASS" . font-lock-preprocessor-face)
	("UDELEGATE" . font-lock-preprocessor-face)
	("UENUM" . font-lock-preprocessor-face)
	("UFUNCTION" . font-lock-preprocessor-face)
	("UINTERFACE" . font-lock-preprocessor-face)
	("UMETA" . font-lock-preprocessor-face)
	("UPARAM" . font-lock-preprocessor-face)
	("UPROPERTY" . font-lock-preprocessor-face)
	("USTRUCT" . font-lock-preprocessor-face))
  "FUCK IT WE BALL.")

(defvar unreal-engine--font-lock-delegates
  '(("DECLARE_DELEGATE" . font-lock-preprocessor-face)
	("DECLARE_EVENT" . font-lock-preprocessor-face)
	("DECLARE_DERIVED_EVENT" . font-lock-preprocessor-face)
	("DECLARE_DYNAMIC_DELEGATE" . font-lock-preprocessor-face)
	("DECLARE_MULTICAST_DELEGATE" . font-lock-preprocessor-face)
	("DECLARE_DYNAMIC_MULTICAST_DELEGATE" . font-lock-preprocessor-face))
  "FUCK IT WE BALL.")

(defvar unreal-engine--font-lock-generated-body-macro
  '(("GENERATED_BODY" . font-lock-preprocessor-face)
	("GENERATED_IINTERFACE_BODY" . font-lock-preprocessor-face)
	("GENERATED_UCLASS_BODY" . font-lock-preprocessor-face)
	("GENERATED_UINTERFACE_BODY" . font-lock-preprocessor-face)
	("GENERATED_USTRUCT_BODY" . font-lock-preprocessor-face))
  "FUCK IT WE BALL.")

(defun unreal-engine-font-lock-add-keywords (&optional mode)
  "Add Unreal Engine keywords into major MODE or current buffer if nil."
  (font-lock-add-keywords mode unreal-engine--font-lock-attributes nil)
  (font-lock-add-keywords mode unreal-engine--font-lock-delegates nil)
  (font-lock-add-keywords mode unreal-engine--font-lock-generated-body-macro nil))

(defun unreal-engine-font-lock-remove-keywords (&optional mode)
  "Remove Unreal Engine keywords from major MODE or current buffer if nil."
  (font-lock-remove-keywords mode unreal-engine--attributes)
  (font-lock-remove-keywords mode unreal-engine--delegates)
  (font-lock-remove-keywords mode unreal-engine--generated-body-macro))

(defun unreal-engine--register-keywords ()
  "Enable colouring of Unreal Engine keywords."
  (unreal-engine-font-lock-add-keywords 'c-mode)
  (font-lock-flush)
  (font-lock-ensure))

(defun unreal-engine--unregister-keywords ()
  "Disable colouring of Unreal Engine keywords."
  (unreal-engine-font-lock-remove-keywords 'c-mode)
  (font-lock-flush)
  (font-lock-ensure))

(defun unreal-engine-mode-enter ()
  "Setup for entering unreal engine mode."
  ;; (unreal-engine--register-keywords))
  (yas-activate-extra-mode 'unreal-engine-mode))

(defun unreal-engine-mode-exit ()
  "Clean-up for entering unreal engine mode."
  ;; (unreal-engine--unregister-keywords)
  (yas-deactivate-extra-mode 'unreal-engine-mode))

(defun unreal-engine-snippets ()
  "go to the dir where the unreal engine snippies are stored.  IF IT EXISTS."
  (interactive)
  (when (boundp 'yas-snippet-dirs)
	  (dired (format "%s/unreal-engine-mode" (nth 0 yas-snippet-dirs)))))


(define-minor-mode unreal-engine-mode
  "Unreal Engine minor mode for ez ya-snippet integration"
  :init-value nil
  ;; :after-hook (if (bound-and-true-p unreal-engine-mode)
  ;; 				  (yas-activate-extra-mode 'unreal-engine-mode)
  ;; 				(yas-deactivate-extra-mode 'unreal-engine-mode))
  ;; :variable unreal-engine
  :ligther " Unreal"
  (if (bound-and-true-p unreal-engine-mode)
	  (unreal-engine-mode-enter)
	(unreal-engine-mode-exit)))

(provide 'unreal-engine-mode)
