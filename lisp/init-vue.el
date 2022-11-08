;;; init-vue.el --- Vue setup
;;; Code:

;;; Commentary:
;;


(defun close-sgml-tag-no-duplicate ()
    "Close current element.
Depending on context, inserts a matching close-tag, or closes
the current start-tag or the current comment or the current cdata, ..."
  (interactive)
  (pcase (car (sgml-lexical-context))
    ('comment   (insert " -->"))
    ('cdata   (insert "]]>"))
    ('pi  (insert " ?>"))
    ('jsp   (insert " %>"))
    ('tag   (insert " />"))
    ('text
     (let ((context (save-excursion (sgml-get-context)))
           (close-tag (if (eq (char-after) ?>) "" ">")))
       (if context
           (progn
             (insert "</" (sgml-tag-name (car (last context))) close-tag)
             (indent-according-to-mode)))))
    (_
     (error "Nothing to close"))))

(use-package vue-mode
  :load-path "~/.emacs.d/site-lisp/vue"
  :init
  (setq-default vue-tag-relative-indent nil)
  (advice-add #'sgml-close-tag :override #'close-sgml-tag-no-duplicate)
  )


(use-package add-node-modules-path
  :commands add-node-modules-path
  :hook (vue-mode . add-node-modules-path))
(require 'project)

(defun find-prj-root (dir)
  "Find the vue project root of DIR."
  (file-truename(locate-dominating-file dir "package.json")))


(defun vue-lsp-bridge-hook ()
  "Add lsp-bridge hooks."
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(vue-mode . "volar"))
  (setq lsp-bridge-enable-log nil)
  (setq lsp-bridge-multi-lang-server-mode-list nil)
  (setq lsp-bridge-multi-lang-server-extension-list nil)
  (setq-local lsp-bridge-get-project-path-by-filepath 'find-prj-root)
  (lsp-bridge-mode)
  )


(add-hook 'vue-mode-hook #'flycheck-mode)


(with-eval-after-load "flycheck"
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  (flycheck-add-mode 'javascript-eslint 'vue-mode))

;; If use lsp-bridge
(with-eval-after-load "lsp-bridge"
  (add-hook 'vue-mode-hook #'vue-lsp-bridge-hook))


;; If use eglot


(with-eval-after-load 'eglot
  (setq-default eglot-events-buffer-size 0)

  (defclass eglot-vls (eglot-lsp-server) ()
    :documentation "Vue Language Server.")

  (cl-defmethod eglot-initialization-options ((server eglot-vls))
    "Passes through required vetur SERVER initialization options to EGLOT-VLS."
    nil
    )

  (add-to-list 'eglot-server-programs
               '(vue-mode . (eglot-vls . ("vls" "--stdio" "--max-old-space-size=4096"))))
  )



;; If use lsp-mode
(with-eval-after-load "lsp-mode"
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (add-hook 'vue-mode-hook #'lsp-deferred))




(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))




(provide 'init-vue)

;;; init-vue.el ends here
