;;; init-lsp-bridge.el --- Lsp bridge setup

;;; Commentary:
;;

;;; Code:


(use-package markdown-mode)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(defun disable-corfu ()
  "Disable corfu mode."
  (if (boundp 'corfu-mode)
      (corfu-mode -1)))

(use-package lsp-bridge
  :ensure nil
  :custom
  (acm-enable-doc t)
  (acm-enable-doc-markdown-render t)
  (lsp-bridge-enable-log nil)
  (lsp-bridge-enable-signature-help t)
  (lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (lsp-bridge-tsdk-path "/usr/lib/node_modules/typescript/lib/")
  :init
  (add-hook 'lsp-bridge-mode-hook #'disable-corfu)
  :load-path "~/.emacs.d/site-lisp/lsp-bridge/")


(provide 'init-lsp-bridge)

;;; init-lsp-bridge.el ends here
