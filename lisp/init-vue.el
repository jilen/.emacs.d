;;; init-vue.el --- Vue setup
;;; Code:

;;; Commentary:
;;



(use-package vue-mode
  :load-path "~/.emacs.d/site-lisp/vue/")

(use-package web-mode)
(use-package add-node-modules-path)
(require 'project)

(setq vue-tag-relative-indent nil)

(defun find-prj-root (dir)
  "Find the vue project root of DIR."
  (file-truename(locate-dominating-file dir "package.json")))


(defun vue-lsp-bridge-hook ()
  "Add lsp-bridge hooks."
  (setq-local lsp-bridge-get-project-path-by-filepath 'find-prj-root))



(add-hook 'vue-mode-hook #'add-node-modules-path)
(add-hook 'vue-mode-hook #'flycheck-mode)


(with-eval-after-load "flycheck"
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  (flycheck-add-mode 'javascript-eslint 'vue-mode))

;; If use lsp-bridge
(with-eval-after-load "lsp-bridge"
  (add-hook 'vue-mode-hook #'vue-lsp-bridge-hook))


;; If use eglot

(defun typescript-file-path ()
    (f-join (locate-dominating-file (buffer-file-name (current-buffer)) "package.json") "node_modules/typescript/lib/tsserverlibrary.js"))

(with-eval-after-load "eglot"

  (cl-defmethod project-root ((project (head vue-module)))
    (cdr project))

  (defun vue-prj-root (dir)
    "Locate Vue root from DIR."
    (if (boundp 'eglot-lsp-context)
        (when-let ((root (locate-dominating-file dir "package.json")))
          (cons 'vue-module root))
      (project-try-vc dir)))

  (defun set-project-root-for-vue ()
    "Set prj root for eglot."
    (add-hook 'project-find-functions #'vue-prj-root))



  (add-hook 'vue-mode-hook #'set-project-root-for-vue)

  (defclass eglot-vls (eglot-lsp-server) ()
    :documentation "Vue Language Server.")

  (cl-defmethod eglot-initialization-options ((server eglot-vls))
    "Passes through required vetur SERVER initialization options to VLS."
    `(:typescript
      (:serverPath ,(typescript-file-path))
      :languageFeatures
      (:references t
                   :definition t
                   :implementation t
                   :typeDefinition t
                   :callHierarchy t
                   :hover t
                   :rename t
                   :signatureHelp t
                   :codeAction t
                   :workspaceSymbol: t
                   :completion (:defaultTagNameCase t))

      :documentFeatures
      (:selectionRange t :foldingRange t :documentSymbol t)
      )
    )

  (add-to-list 'eglot-server-programs
               '(typescript-mode . (eglot-vls . ("vue-language-server" "--stdio"))))
  (add-to-list 'eglot-server-programs
               '(vue-mode . (eglot-vls . ("vue-language-server" "--stdio"))))

  )



;; If use lsp-mode
(with-eval-after-load "lsp-mode"
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (add-hook 'vue-mode-hook #'lsp-deferred))




(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))




(provide 'init-vue)

;;; init-vue.el ends here
