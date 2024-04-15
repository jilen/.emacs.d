;;; init-eglot.scala --- Eglot -- lsp client for emacs setup. -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'eglot)

(defvar mode-root-marker-alist
  `((vue-mode . "package.json")
    (js-ts-mode "package.json")
    (scala-mode ".bsp/")
    (typescript-ts-mode "package.json")))

(defun project-find-with-marker (dir)
  "Find project of DIR with marker files."
  (if (boundp 'eglot-lsp-context)
      (when-let* ((marker (alist-get major-mode mode-root-marker-alist))
                  (root (locate-dominating-file dir marker)))
        (list 'vc 'Git root))))

(require 'project)

(add-hook 'project-find-functions #'project-find-with-marker)

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
