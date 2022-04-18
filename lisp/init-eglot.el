;;; init-eglot.scala --- Eglot -- lsp client for emacs setup.

;;; Commentary:
;;

;;; Code:

(use-package eglot)

(use-package eldoc-box
  :config
  (set-face-foreground 'eldoc-box-body (face-foreground 'region))
  (set-face-background 'eldoc-box-border (face-background 'region))
  (with-eval-after-load "eglot"
    (add-hook 'eglot--managed-mode-hook #'eldoc-box-hover-at-point-mode t)))

(provide 'init-eglot)

;;; init-eglot.el ends here
