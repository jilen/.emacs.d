;;; init-eglot.scala --- Eglot -- lsp client for emacs setup.

;;; Commentary:
;;

;;; Code:

;; (use-package eglot)

(if (version<= "29" emacs-version)
    (require 'eglot)
  (use-package eglot))

(use-package eldoc-box
  :config
  (set-face-foreground 'eldoc-box-body (face-foreground 'region))
  (set-face-background 'eldoc-box-border (face-background 'region))
  )

(provide 'init-eglot)

;;; init-eglot.el ends here
