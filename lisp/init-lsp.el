;;; init-lsp.el --- Init lsp mode



;;; Commentary:
;;

;;; Code:

(setenv "LSP_USE_PLISTS" "true")
(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq read-process-output-max (* 3 1024 1024)) ;; 1mb
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . lsp-mode-setup-completion))


(use-package "lsp-ui"
  :init
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable t)
  (setq lsp-lens-enable nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-position 'at-point))

(provide 'init-lsp)

;;; init-lsp.el ends here
