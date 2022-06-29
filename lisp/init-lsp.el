;;; init-lsp.el --- Init lsp mode



;;; Commentary:
;;

;;; Code:

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!

  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
   :hook
   (lsp-completion-mode . lsp-mode-setup-completion))

(use-package "lsp-ui"
  :init
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t))

(provide 'init-lsp)

;;; init-lsp.el ends here
