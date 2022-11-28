;;; init-lsp-bridge.el --- Lsp bridge setup

;;; Commentary:
;;

;;; Code:


(use-package markdown-mode)

(use-package yasnippet
  :config
  (yas-global-mode 1))


(use-package lsp-bridge
  :ensure nil
  :init
  :load-path "~/.emacs.d/site-lisp/lsp-bridge/"
  )



(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here
