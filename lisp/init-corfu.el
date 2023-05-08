;;; init-corfu.el --- corfu completion intergation

;;; Commentary:
;;

;;; Code:

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-auto-prefix 2)          ;; Trigger auto completion with 2 chars
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-preselect-first t)      ;; Disable candidate preselection
  (corfu-scroll-margin 5)        ;; Use scroll margin

  :init
  (global-corfu-mode)
  )

(use-package kind-icon
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package company-svg-icon
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/company-svg-icon"
  :config
  (advice-add 'kind-icon-formatted :override #'company-svg--get-kind-icon))

;; Add extensions
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (setq cape-dict-case-fold t)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(provide 'init-corfu)

;;; init-corfu.el ends here
