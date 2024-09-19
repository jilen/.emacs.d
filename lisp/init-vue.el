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

(require 'f)
(defun get-ts-sdk ()
  "Get ts sdk path."
  (let ((eglot-lsp-context t))
    (f-join (project-root (project-current)) "node_modules/typescript/lib")))

(with-eval-after-load 'eglot
  (require 'init-eglot)
  (setq-default eglot-events-buffer-size 0)

  (cl-defmethod eglot-initialization-options (server)
    "Passes through required vetur SERVER initialization options to EGLOT-VLS."
    `(:typescript
      (:tsdk ,(get-ts-sdk))))

  (add-to-list 'eglot-server-programs
               '(vue-mode . ("vue-language-server" "--stdio"))))

(defun vue-lsp-bridge-hook ()
  "Add lsp-bridge hooks."
  (corfu-mode -1)
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(vue-mode . "volar"))
  (setq lsp-bridge-enable-log nil)
  (lsp-bridge-mode))

;; If use lsp-bridge
(with-eval-after-load "lsp-bridge"
  (add-hook 'vue-mode-hook #'vue-lsp-bridge-hook))

;; If use lsp-mode
(with-eval-after-load "lsp-mode"
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (add-hook 'vue-mode-hook #'lsp-deferred))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(with-eval-after-load "vue"
  (add-to-list 'apheleia-mode-alist '(vue-mode . prettier)))

(provide 'init-vue)

;;; init-vue.el ends here
