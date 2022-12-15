;;; init-vue.el --- Vue setup
;;; Code:

;;; Commentary:
;;


(use-package web-mode)
(define-derived-mode vue-mode web-mode "Vue" "Major mode for vue sfc.")


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
  (require 'init-eglot)
  (add-hook 'vue-mode-hook #'set-project-root-for-npm)
  (setq-default eglot-events-buffer-size 0)

  (defclass eglot-vls (eglot-lsp-server) ()
    :documentation "Vue Language Server.")

  (cl-defmethod eglot-initialization-options ((server eglot-vls))
    "Passes through required vetur SERVER initialization options to EGLOT-VLS."
    nil)

  (add-to-list 'eglot-server-programs
               '(vue-mode . (eglot-vls . ("vls" "--stdio"))))
  )



;; If use lsp-mode
(with-eval-after-load "lsp-mode"
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (add-hook 'vue-mode-hook #'lsp-deferred))




(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))




(provide 'init-vue)

;;; init-vue.el ends here
