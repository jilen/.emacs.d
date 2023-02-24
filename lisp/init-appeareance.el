;;; init-appeareance.el --- Setup themes and other appearance staffs

;;; Commentary:
;;

;;; Code:

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;; Specify font for Chinese characters
(cl-loop for font in '("LXGW WenKai Mono")
         when (font-installed-p font)
         return (set-fontset-font t '(#x4e00 . #x9fff) font))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))


(use-package dashboard
  :custom
  (dashboard-image-banner-max-width 600)
  (dashboard-set-heading-icons t)
  (dashboard-projects-backend 'project-el)
  (dashboard-set-file-icons t)
  (dashboard-week-agenda t)
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     agenda))
  (dashboard-banner-logo-title "\nIf someone ever tells me it's a mistake to have hope, well, then\nI'll just tell them they're wrong. \nAnd I'll keep telling them 'til they believe!\nNo matter how many times it takes.")
  (dashboard-startup-banner (concat dotfiles-dir "logo.svg"))

  :config
  (dashboard-setup-startup-hook))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))


;; Theme setup.
(use-package ef-themes
  :config
  (load-theme 'ef-duo-light t))


(use-package doom-modeline
  :init
  (setq doom-modeline-height (* (default-font-height) 2))
  :config
  (doom-modeline-mode)
  )

;; Font setting.
(set-frame-font "Iosevka Comfy")
(defconst preferred-font-height 140)
(defconst preferred-line-number-height (- preferred-font-height 20))
(set-face-attribute 'default nil
                    :weight 'semilight
                    :height preferred-font-height)
(set-face-attribute 'line-number nil :height preferred-line-number-height )
(set-face-attribute 'line-number-current-line nil :height preferred-line-number-height)

(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (add-to-list 'all-the-icons-data/alltheicons-alist '("sc" . "\xe908"))
  (add-to-list 'all-the-icons-extension-icon-alist '("sc"  all-the-icons-alltheicon  "scala"  :face all-the-icons-red)))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle))
  )

(use-package all-the-icons-dired
  :init
  :hook
  (dired-mode . all-the-icons-dired-mode)
  (dired-mode . dired-hide-details-mode))


(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(show-paren-mode 1)

(provide 'init-appeareance)

;;; init-appeareance.el ends here
