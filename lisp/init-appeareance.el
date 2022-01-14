;;; init-appeareance.el --- Setup themes and other appearance staffs

;;; Commentary:
;;


(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(set-face-attribute 'default nil :height 140 :weight 'medium)

(use-package dashboard
  :custom
  (dashboard-image-banner-max-width 600)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-projects-backend 'project-el)
  (dashboard-startup-banner (concat dotfiles-dir "logo.png"))
  (dashboard-center-content t)
  
  :config
  (dashboard-setup-startup-hook))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-one-light t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode)
  (dired-mode . dired-hide-details-mode))


(provide 'init-appeareance)

;;; init-appeareance.el ends here
