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

(require 'color)

(defsubst rgb-blend (rgb1 rgb2 frac)
  "Return a fractional blend between two colors RGB1 and RGB2.
Each is a 3 element list.  The fractional blend point is the
float FRAC."
  (apply #'color-rgb-to-hex
         (cl-mapcar (lambda (a b)
                      (+ (* a frac) (* b (- 1.0 frac))))
                    rgb1 rgb2)))

(defsubst icon-face-spec (face)
  "Compute icon face spec from FACE."
  (let* ((default-bg (frame-parameter nil 'background-color))
         (fg (or (face-foreground face) (frame-parameter nil 'foreground-color)))
         (bg (rgb-blend (color-name-to-rgb fg) (color-name-to-rgb default-bg) 0.12)))
    `(:foreground ,fg :background ,bg)))

(defun codicon (name face)
  "Get cod icon NAME with FACE foreground and blended background."
  (nerd-icons-codicon name :face (icon-face-spec face)))

(defconst corfu-kind-icon-mapping
  `(
    (array . ,(codicon "nf-cod-symbol_array" 'font-lock-type-face))
    (boolean . ,(codicon "nf-cod-symbol_boolean" 'font-lock-builtin-face))
    (class . ,(codicon "nf-cod-symbol_class" 'font-lock-type-face))
    (color . ,(codicon "nf-cod-symbol_color" 'success) )
    (command . ,(codicon "nf-cod-terminal" 'default) )
    (constant . ,(codicon "nf-cod-symbol_constant" 'font-lock-constant-face) )
    (constructor . ,(codicon "nf-cod-triangle_right" 'font-lock-function-name-face) )
    (enummember . ,(codicon "nf-cod-symbol_enum_member" 'font-lock-builtin-face) )
    (enum-member . ,(codicon "nf-cod-symbol_enum_member" 'font-lock-builtin-face) )
    (enum . ,(codicon "nf-cod-symbol_enum" 'font-lock-builtin-face) )
    (event . ,(codicon "nf-cod-symbol_event" 'font-lock-warning-face) )
    (field . ,(codicon "nf-cod-symbol_field" 'font-lock-variable-name-face) )
    (file . ,(codicon "nf-cod-symbol_file" 'font-lock-string-face) )
    (folder . ,(codicon "nf-cod-folder" 'font-lock-doc-face) )
    (interface . ,(codicon "nf-cod-symbol_interface" 'font-lock-type-face) )
    (keyword . ,(codicon "nf-cod-symbol_keyword" 'font-lock-keyword-face) )
    (macro . ,(codicon "nf-cod-symbol_misc" 'font-lock-keyword-face) )
    (magic . ,(codicon "nf-cod-wand" 'font-lock-builtin-face) )
    (method . ,(codicon "nf-cod-symbol_method" 'font-lock-function-name-face) )
    (function . ,(codicon "nf-cod-symbol_method" 'font-lock-function-name-face) )
    (module . ,(codicon "nf-cod-file_submodule" 'font-lock-preprocessor-face) )
    (numeric . ,(codicon "nf-cod-symbol_numeric" 'font-lock-builtin-face) )
    (operator . ,(codicon "nf-cod-symbol_operator" 'font-lock-comment-delimiter-face) )
    (param . ,(codicon "nf-cod-symbol_parameter" 'default) )
    (property . ,(codicon "nf-cod-symbol_property" 'font-lock-variable-name-face) )
    (reference . ,(codicon "nf-cod-references" 'font-lock-variable-name-face) )
    (snippet . ,(codicon "nf-cod-symbol_snippet" 'font-lock-string-face) )
    (string . ,(codicon "nf-cod-symbol_string" 'font-lock-string-face) )
    (struct . ,(codicon "nf-cod-symbol_structure" 'font-lock-variable-name-face) )
    (text . ,(codicon "nf-cod-text_size" 'font-lock-doc-face) )
    (typeparameter . ,(codicon "nf-cod-list_unordered" 'font-lock-type-face) )
    (type-parameter . ,(codicon "nf-cod-list_unordered" 'font-lock-type-face) )
    (unit . ,(codicon "nf-cod-symbol_ruler" 'font-lock-constant-face) )
    (value . ,(codicon "nf-cod-symbol_field" 'font-lock-builtin-face) )
    (variable . ,(codicon "nf-cod-symbol_variable" 'font-lock-variable-name-face) )
    (t . ,(codicon "nf-cod-code" 'font-lock-warning-face))))

(defsubst nerd-icon--metadata-get (metadata type-name)
  "Get METADATA for keyword TYPE-NAME from the completion properties."
  (or
   (plist-get completion-extra-properties (intern (format ":%s" type-name)))
   (cdr (assq (intern type-name) metadata))))

(defsubst nerd-icon-formatted (kind)
  "Get icon for KIND."
  (let* ((icon (alist-get kind corfu-kind-icon-mapping))
         (icon-face (get-text-property 0 'face icon))
         (icon-bg (plist-get icon-face :inherit))
         (icon-pad (propertize " " 'face (append '(:height 0.5) icon-bg)))
         (item-pad (propertize " " 'face '(:height 0.5))))
    (concat icon-pad icon icon-pad item-pad)))

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
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
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
