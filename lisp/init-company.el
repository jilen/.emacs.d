;;; init-company.el --- Company completion setup

;;; Commentary:
;;


(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-dabbrev-downcase nil)
  :config
  (global-company-mode))

(use-package company-svg-icon
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/company-svg-icon"
  :config
  (setq company-format-margin-function #'company-svg-icon-format-margin-function))

(provide 'init-company)

;;; init-company.el ends here
