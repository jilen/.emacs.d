;;; init-flycheck.el --- Flycheck setup. some are stolen from doom emacs

;;; Commentary:
;;
(use-package flycheck)


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
