;;; init-eglot.scala --- Eglot -- lsp client for emacs setup.

;;; Commentary:
;;

;;; Code:

;; (use-package eglot)

(if (version<= "29" emacs-version)
    (require 'eglot)
  (use-package eglot))

(use-package eldoc-box
  :init
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t))

(defun npm-prj-root (dir)
  "Locate Vue root from DIR."
  (if (boundp 'eglot-lsp-context)
      (when-let ((root (locate-dominating-file dir "package.json")))
        (if (version<= "29.0" emacs-version)
            (list 'vc 'Git root)
          (list 'vc root)))
    (project-try-vc dir)))

(defun set-project-root-for-npm ()
  "Set prj root for eglot."
  (add-hook 'project-find-functions #'npm-prj-root))

(with-eval-after-load "js-mode"
  (add-hook 'js-mode-hook #'set-project-root-for-npm))
(with-eval-after-load "typescript-mode"
  (add-hook 'typescript-mode #'set-project-root-for-npm))


(provide 'init-eglot)

;;; init-eglot.el ends here
