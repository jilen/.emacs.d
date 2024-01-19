;;; init-eglot.scala --- Eglot -- lsp client for emacs setup. -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; (use-package eglot)

(if (version<= "29" emacs-version)
    (require 'eglot)
  (use-package eglot))

(defun npm-prj-root (dir)
  "Locate Vue root from DIR."
  (if (and (boundp 'eglot-lsp-context) (symbol-value 'eglot-lsp-context))
      (when-let ((root (locate-dominating-file dir "package.json")))
        (list 'vc 'Git root))
    (project-try-vc dir)))

(defun set-project-root-for-npm ()
  "Set prj root for eglot."
  (add-hook 'project-find-functions #'npm-prj-root))

(with-eval-after-load "js-mode"
  (add-hook 'js-mode-hook #'set-project-root-for-npm))
(with-eval-after-load "typescript-mode"
  (add-hook 'typescript-mode #'set-project-root-for-npm))

(use-package eglot-booster
  :load-path "~/.emacs.d/site-lisp/eglot-booster/"
  :after eglot
  :init
  (add-hook 'after-init-hook #'eglot-booster-mode))


(provide 'init-eglot)

;;; init-eglot.el ends here
