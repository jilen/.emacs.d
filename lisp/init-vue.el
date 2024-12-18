;;; init-vue.el --- Vue setup
;;; Code:

;;; Commentary:
;;

(use-package web-mode)

(define-derived-mode vue-mode web-mode "Vue" "Vue-SFC.")

(with-eval-after-load 'nerd-icons
  (add-to-list 'nerd-icons-mode-icon-alist '(vue-mode nerd-icons-sucicon "nf-seti-vue" :face nerd-icons-lgreen)))

(use-package add-node-modules-path
  :commands add-node-modules-path
  :hook (vue-mode . add-node-modules-path))
(require 'project)


(add-hook 'vue-mode-hook #'flycheck-mode)


(with-eval-after-load "flycheck"
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  (flycheck-add-mode 'javascript-eslint 'vue-mode))

;; If use eglot
(setq eglot-ignored-server-capabilities '(:signatureHelpProvider))
(require 'f)
(defun get-ts-sdk ()
  "Get ts sdk path."
  (let ((eglot-lsp-context t))
    (f-join (project-root (project-current)) "node_modules/typescript/lib")))

(with-eval-after-load "eglot"
  (require 'init-eglot)
  (setq-default eglot-events-buffer-size 0)
  (cl-defmethod eglot-initialization-options (server)
    "Passes through required vetur SERVER initialization options to vue-language-server."
    `(:typescript
      (:tsdk ,(get-ts-sdk))
      :vue
      (:hybridMode :json-false)
      :languageFeatures (:completion
                         (:defaultTagNameCase "both"
                                              :defaultAttrNameCase "kebabCase"
                                              :getDocumentNameCasesRequest :json-false
                                              :getDocumentSelectionRequest :json-false)
                         :diagnostics
                         (:getDocumentVersionRequest :json-false))
      :documentFeatures
      (:selectionRange t :foldingRange t :linkedEditingRange t :documentSymbol t :documentColor t)
      :serverMode 0
      :diagnosticModel 1
      :textDocumentSync 2))

  (add-to-list 'eglot-server-programs
               '(vue-mode . ("vue-language-server" "--stdio"))))

;; If use lsp-bridge
(with-eval-after-load "lsp-bridge"
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(vue-mode . "volar"))
  (add-hook 'vue-mode-hook 'lsp-bridge-mode))

;; If use lsp-mode
(with-eval-after-load "lsp-mode"
  (add-hook 'vue-mode-hook #'lsp-deferred))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(with-eval-after-load "vue"
  (add-to-list 'apheleia-mode-alist '(vue-mode . prettier)))

(provide 'init-vue)

;;; init-vue.el ends here
