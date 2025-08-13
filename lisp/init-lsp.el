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
  (lsp-eldoc-render-all t)
  :init
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . lsp-mode-setup-completion))


(use-package lsp-ui
  :init
  (setopt lsp-log-io t)
  (setopt lsp-ui-sideline-enable nil)
  (setopt lsp-ui-doc-enable nil) ;; use eldoc
  (setopt lsp-enable-text-document-color nil)
  ;; (setq lsp-lens-enable nil)
  (setopt lsp-enable-indentation nil)
  (setopt lsp-ui-doc-show-with-cursor t)
  (setopt lsp-ui-doc-delay 0.5)
  (setopt lsp-ui-doc-use-childframe t)
  (setopt lsp-ui-doc-position 'at-point))

;; Use Eldoc-box instead of lsp-ui-doc
(use-package eldoc-box
  :config
  (add-hook 'lsp-mode-hook #'eldoc-box-hover-at-point-mode t))

;; --- Flycheck configuration ---

(defun setup-lsp-next-checkers ()
  "Configure flycheck to run other checkers after the 'lsp' checker.
This function is intended to be added to `flycheck-mode-hook`."
  (when-let ((checker (cl-find-if (lambda (checker)
                                    (and (not (eq checker 'lsp))
                                         (flycheck-checker-supports-major-mode-p checker major-mode)))
                                  flycheck-checkers)))
    (flycheck-add-next-checker 'lsp `(info . ,checker))))

(add-hook 'lsp-diagnostics-mode-hook #'setup-lsp-next-checkers)

;; --- End Flycheck configuration ---


(provide 'init-lsp)

;;; init-lsp.el ends here
