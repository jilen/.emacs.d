;;; init-git.el --- Magit support

;;; Commentary:
;; 

;;; Code:

(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(provide 'init-git)

;;; init-git.el ends here
