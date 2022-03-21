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
  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  )

(provide 'init-git)

;;; init-git.el ends here
