;;; init-org.el --- Org mode tweaks

;;; Commentary:
;;

;;; Code:

(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode))

(defvar agendar-base-dir "~/Workspaces/tasks/"
  "Location store agenda files."
  )

(defun agendar-dirs ()
  "Get agendar dirs."
  (let* ((base-dir (symbol-value 'agendar-base-dir))
         (year (format-time-string "%Y"))
         (year-base-dir (concat base-dir year)))
    (list year-base-dir))
  )

(setq org-agenda-files (agendar-dirs))
(setq recentf-exclude (org-agenda-files))

(use-package org-super-agenda)

(provide 'init-org)

;;; init-org.el ends here
