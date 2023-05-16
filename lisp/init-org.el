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
(require 'recentf)
(setq recentf-exclude (agendar-dirs))

(use-package org-super-agenda)

(setq org-latex-pdf-process '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

;; Allow mermaid inline graph.
(use-package ob-mermaid)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (mermaid . t))) ; this line activates dot

(setq org-latex-listings 'minted)

(provide 'init-org)

;;; init-org.el ends here
