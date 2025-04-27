;;; init-lsp.el --- Init lsp mode



;;; Commentary:
;;

;;; Code:
(use-package lsp-mode
  :custom
  (lsp-enable-snippet nil)
  (lsp-completion-provider :none) ;; use corfu
  (lsp-enable-on-type-formatting nil)
  (read-process-output-max (* 3 1024 1024)) ;; 1mb
  :init
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . lsp-mode-setup-completion)
  )


(use-package "lsp-ui"
  :init
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-lens-enable nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-position 'at-point)
  )

;; Use Eldoc-box instead of lsp-ui-doc
(use-package eldoc-box
  :config
  (add-hook 'lsp-mode-hook #'eldoc-box-hover-at-point-mode t))

(provide 'init-lsp)

;;; init-lsp.el ends here
