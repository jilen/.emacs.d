;;; init-org.el --- Org mode tweaks

;;; Commentary:
;;

;;; Code:

(use-package org-modern
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(when (file-directory-p "~/Workspaces/tasks/")
  (defconst agendar-base-dir '("~/Workspaces/tasks/")
    "Location store agenda files.")
  (setq org-agenda-files agendar-base-dir)
  )

(require 'recentf)
(setq recentf-exclude org-agenda-files)

(setq org-latex-pdf-process '("tectonic -Z shell-escape %f"))
(setq org-plantuml-exec-mode 'jar)
(setq org-plantuml-jar-path "~/.local/bin/plantuml-pdf.jar")



;; Allow mermaid inline graph.
(use-package ob-mermaid)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (plantuml . t)
   (mermaid . t))) ; this line activates dot

(setq org-latex-listings 'minted)

(provide 'init-org)

;;; init-org.el ends here
