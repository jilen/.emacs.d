;;; init-org.el --- Org mode tweaks

;;; Commentary:
;;

;;; Code

(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode))

(provide 'init-org)

;;; init-org.el ends here
