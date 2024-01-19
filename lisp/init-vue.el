;;; init-vue.el --- Vue setup
;;; Code:

;;; Commentary:
;;


(define-derived-mode vue-mode web-mode "Vue" "Vue-SFC.")

(with-eval-after-load 'nerd-icons
  (add-to-list 'nerd-icons-mode-icon-alist '(vue-mode nerd-icons-sucicon "nf-seti-vue" :face nerd-icons-lgreen)))

(use-package add-node-modules-path
  :commands add-node-modules-path
  :hook (vue-mode . add-node-modules-path))
(require 'project)

(defun find-prj-root (dir)
  "Find the vue project root of DIR."
  (file-truename(locate-dominating-file dir "package.json")))


(add-hook 'vue-mode-hook #'flycheck-mode)


(with-eval-after-load "flycheck"
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  (flycheck-add-mode 'javascript-eslint 'vue-mode))

;; If use eglot


(defun get-ts-sdk ()
  (let ((eglot-lsp-context t))
    (f-join (project-root (project-current)) "node_modules/typescript/lib")))

(with-eval-after-load 'eglot
  (require 'init-eglot)
  (add-hook 'vue-mode-hook #'set-project-root-for-npm)
  (setq-default eglot-events-buffer-size 0)

  (cl-defmethod eglot-initialization-options (server)
    "Passes through required vetur SERVER initialization options to EGLOT-VLS."
    `(:typescript
      (:tsdk ,(get-ts-sdk))))

  (add-to-list 'eglot-server-programs
               '((vue-mode typescript-ts-mode) . ("vue-language-server" "--stdio"))))



;; If use lsp-mode
(with-eval-after-load "lsp-mode"
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
  (add-hook 'typescript-mode-hook #'lsp-deferred)
  (add-hook 'vue-mode-hook #'lsp-deferred))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(provide 'init-vue)

;;; init-vue.el ends here
