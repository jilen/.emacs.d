;;; init-lsp-proxy.el --- Lsp-proxy mode


;;; Commentary:
;;

;;; Code:
(use-package yasnippet)
(use-package ht)
(use-package lsp-proxy
  :load-path "~/.emacs.d/site-lisp/lsp-proxy"
  :init
  ;; (setopt lsp-proxy-log-level 3)
  (setopt lsp-proxy-enable-hover-eldoc t)
  (setopt lsp-proxy-enable-symbol-highlighting nil)
  :config
  ;; TypeScript and JavaScript modes
  (add-hook 'tsx-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'js-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'typescript-mode-hook #'lsp-proxy-mode)
  (add-hook 'typescript-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'js-mode-hook #'lsp-proxy-mode)
  (add-hook 'javascript-mode-hook #'lsp-proxy-mode))


(require 'lsp-proxy)  ; Ensure loaded

(provide 'init-lsp-proxy)

;;; init-lsp-proxy.el ends here
