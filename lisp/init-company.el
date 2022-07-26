;;; init-company.el --- Company completion setup

;;; Commentary:
;;


(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-dabbrev-downcase nil)
  :config
  (global-company-mode))

(provide 'init-company)

;;; init-company.el ends here
