;;; init-eglot.scala --- Eglot -- lsp client for emacs setup. -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'eglot)

(defun run-with-extra-root-marker (orig-fun &rest args)
  "Apply ARGS to ORIG-FUN with extra root dirs."
  (if (boundp 'eglot-lsp-context)
      (let ((project-vc-extra-root-markers `("package.json" ".bsp" "build.sc")))
        (apply orig-fun args))
    (apply orig-fun args)))

(require 'project)
(advice-add 'project-current :around #'run-with-extra-root-marker)

(use-package eglot-booster
  :load-path "~/.emacs.d/site-lisp/eglot-booster/"
  :after eglot
  :init
  (add-hook 'after-init-hook #'eglot-booster-mode))

(use-package eldoc-box
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t))


(provide 'init-eglot)

;;; init-eglot.el ends here
