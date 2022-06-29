;;; init-vue.el --- Vue setup
;;; Code:

;;; Commentary:
;;

(require 'vue-mode)
(use-package web-mode)
(use-package add-node-modules-path)
(require 'project)

(setq vue-tag-relative-indent nil)

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


(add-hook 'vue-mode-hook #'add-node-modules-path)
(add-hook 'vue-mode-hook #'flycheck-mode)
(add-hook 'vue-mode-hook #'set-project-root-for-vue)

(with-eval-after-load "flycheck"
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  (flycheck-add-mode 'javascript-eslint 'vue-mode))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(setq gc-cons-threshold 200000000)

;; (with-eval-after-load "eglot"
;;   (defclass eglot-vls (eglot-lsp-server) ()
;;     :documentation "Vue Language Server.")

;;   (cl-defmethod eglot-initialization-options ((server eglot-vls))
;;     "Passes through required vetur SERVER initialization options to VLS."
;;     '(:typescript
;;       (:serverPath "")
;;       :languageFeatures
;;       (:references t
;;                    :definition t
;;                    :implementation t
;;                    :typeDefinition t
;;                    :callHierarchy t
;;                    :hover t
;;                    :rename t
;;                    :signatureHelp t
;;                    :codeAction t
;;                    :workspaceSymbol: t
;;                    :completion (:defaultTagNameCase t))

;;       :documentFeatures
;;       (:selectionRange t :foldingRange t :documentSymbol t)
;;       )
;;     )

;;   (add-to-list 'eglot-server-programs
;;                '(typescript-mode . (eglot-vls . ("vue-language-server" "--stdio"))))
;;   (add-to-list 'eglot-server-programs
;;                '(vue-mode . (eglot-vls . ("vue-language-server" "--stdio")))))

(use-package "lsp-mode"
  :hook (vue-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

(provide 'init-vue)

;;; init-vue.el ends here
