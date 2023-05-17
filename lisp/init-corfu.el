;;; init-corfu.el --- corfu completion intergation -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-auto-prefix 2)          ;; Trigger auto completion with 2 chars
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-preselect-first t)      ;; Disable candidate preselection
  (corfu-scroll-margin 5)        ;; Use scroll margin

  :init
  (global-corfu-mode))

(use-package nerd-icons)

(defconst corfu-kind-icon-mapping
  `(
    (array . ,(nerd-icons-codicon "nf-cod-symbol_array" :face 'font-lock-type-face))
    (boolean . ,(nerd-icons-codicon "nf-cod-symbol_boolean" :face 'font-lock-builtin-face))
    (class . ,(nerd-icons-codicon "nf-cod-symbol_class" :face 'font-lock-type-face))
    (color . ,(nerd-icons-codicon "nf-cod-symbol_color" :face 'success) )
    (command . ,(nerd-icons-codicon "nf-cod-terminal" :face 'default) )
    (constant . ,(nerd-icons-codicon "nf-cod-symbol_constant" :face 'font-lock-constant-face) )
    (constructor . ,(nerd-icons-codicon "nf-cod-triangle_right" :face 'font-lock-function-name-face) )
    (enummember . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'font-lock-builtin-face) )
    (enum-member . ,(nerd-icons-codicon "nf-cod-symbol_enum_member" :face 'font-lock-builtin-face) )
    (enum . ,(nerd-icons-codicon "nf-cod-symbol_enum" :face 'font-lock-builtin-face) )
    (event . ,(nerd-icons-codicon "nf-cod-symbol_event" :face 'font-lock-warning-face) )
    (field . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-variable-name-face) )
    (file . ,(nerd-icons-codicon "nf-cod-symbol_file" :face 'font-lock-string-face) )
    (folder . ,(nerd-icons-codicon "nf-cod-folder" :face 'font-lock-doc-face) )
    (interface . ,(nerd-icons-codicon "nf-cod-symbol_interface" :face 'font-lock-type-face) )
    (keyword . ,(nerd-icons-codicon "nf-cod-symbol_keyword" :face 'font-lock-keyword-face) )
    (macro . ,(nerd-icons-codicon "nf-cod-symbol_misc" :face 'font-lock-keyword-face) )
    (magic . ,(nerd-icons-codicon "nf-cod-wand" :face 'font-lock-builtin-face) )
    (method . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face) )
    (function . ,(nerd-icons-codicon "nf-cod-symbol_method" :face 'font-lock-function-name-face) )
    (module . ,(nerd-icons-codicon "nf-cod-file_submodule" :face 'font-lock-preprocessor-face) )
    (numeric . ,(nerd-icons-codicon "nf-cod-symbol_numeric" :face 'font-lock-builtin-face) )
    (operator . ,(nerd-icons-codicon "nf-cod-symbol_operator" :face 'font-lock-comment-delimiter-face) )
    (param . ,(nerd-icons-codicon "nf-cod-symbol_parameter" :face 'default) )
    (property . ,(nerd-icons-codicon "nf-cod-symbol_property" :face 'font-lock-variable-name-face) )
    (reference . ,(nerd-icons-codicon "nf-cod-references" :face 'font-lock-variable-name-face) )
    (snippet . ,(nerd-icons-codicon "nf-cod-symbol_snippet" :face 'font-lock-string-face) )
    (string . ,(nerd-icons-codicon "nf-cod-symbol_string" :face 'font-lock-string-face) )
    (struct . ,(nerd-icons-codicon "nf-cod-symbol_structure" :face 'font-lock-variable-name-face) )
    (text . ,(nerd-icons-codicon "nf-cod-text_size" :face 'font-lock-doc-face) )
    (typeparameter . ,(nerd-icons-codicon "nf-cod-list_unordered" :face 'font-lock-type-face) )
    (type-parameter . ,(nerd-icons-codicon "nf-cod-list_unordered" :face 'font-lock-type-face) )
    (unit . ,(nerd-icons-codicon "nf-cod-symbol_ruler" :face 'font-lock-constant-face) )
    (value . ,(nerd-icons-codicon "nf-cod-symbol_field" :face 'font-lock-builtin-face) )
    (variable . ,(nerd-icons-codicon "nf-cod-symbol_variable" :face 'font-lock-variable-name-face) )
    (t . ,(nerd-icons-codicon "nf-cod-code" :face 'font-lock-warning-face))))


(defsubst nerd-icon--metadata-get (metadata type-name)
  "Get METADATA for keyword TYPE-NAME from the completion properties."
  (or
   (plist-get completion-extra-properties (intern (format ":%s" type-name)))
   (cdr (assq (intern type-name) metadata))))

(defsubst nerd-icon-formatted (kind)
  "Get icon for KIND."
  (concat " " (alist-get kind corfu-kind-icon-mapping) " "))

(defun nerd-icon-margin-formatter (metadata)
  "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
  (if-let ((kind-func (nerd-icon--metadata-get metadata "company-kind")))
      (lambda (cand)
  (if-let ((kind (funcall kind-func cand)))
      (nerd-icon-formatted kind)
    (nerd-icon-formatted t)))))

(add-to-list 'corfu-margin-formatters #'nerd-icon-margin-formatter)

;; Add extensions
(use-package cape
  :init
  (setq cape-dict-case-fold t)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(provide 'init-corfu)

;;; init-corfu.el ends here
