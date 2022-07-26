;;; init-lsp-bridge.el --- Lsp bridge setup

;;; Commentary:
;;

;;; Code:

(use-package yasnippet
  :config
  (yas-global-mode 1))

(defconst lsp-bridge-mode-hooks '(vue-mode-hook) "Modes that use lsp-bridge.")

(use-package lsp-bridge
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/lsp-bridge/"
  :init
  (dolist (mode lsp-bridge-mode-hooks)
    (add-hook mode #'lsp-bridge-mode))
  (setq lsp-bridge-enable-log t)
  (setq lsp-bridge-enable-debug t))

(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here
