;;; init-vue.el --- Vue setup
;;; Code:

;;; Commentary:
;;

(require 'vue-mode)
(use-package web-mode)
(use-package add-node-modules-path)

(require 'project)

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

(with-eval-after-load "eglot"
  (defclass eglot-vls (eglot-lsp-server) ()
    :documentation "Vue Language Server.")

  (cl-defmethod eglot-initialization-options ((server eglot-vls))
    "Passes through required vetur SERVER initialization options to VLS."
    '(:vetur
      (:validation
       (:template t :style t :script t))))
  (add-to-list 'eglot-server-programs
               '(vue-mode . (eglot-vls . ("vls")))))



(provide 'init-vue)

;;; init-vue.el ends here
