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
  (dashboard-items '((recents  . 5)
                     (projects . 5)))
  (dashboard-banner-logo-title "\nIf someone ever tells me it's a mistake to have hope, well, then\nI'll just tell them they're wrong. \nAnd I'll keep telling them 'til they believe!\nNo matter how many times it takes.")
  (dashboard-startup-banner (concat dotfiles-dir "logo.png"))

  :config
  (dashboard-setup-startup-hook))

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Theme, modeline setup
(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend)
        modus-themes-mode-line '(accented borderless))
  :config
  (load-theme 'modus-operandi t))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

;; Font setting.
(defconst preferred-font-height 120)
(defconst preferred-line-number-height (- preferred-font-height 20))
(set-face-attribute 'default nil :height preferred-font-height :weight 'regular)
(set-face-attribute 'line-number nil :height preferred-line-number-height)
(set-face-attribute 'line-number-current-line nil :height preferred-line-number-height)

(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (add-to-list 'all-the-icons-data/alltheicons-alist '("sc" . "\xe908"))
  (add-to-list 'all-the-icons-extension-icon-alist '("sc"  all-the-icons-alltheicon  "scala"  :face all-the-icons-red))
  )

(use-package dirvish
  :ensure t
  :custom
  (dirvish-bookmark-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  ;; (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  ;; Don't worry, Dirvish is still performant even you enable all these attributes
  (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode)
  )

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(show-paren-mode 1)

(use-package minibuffer-header
  :config
  (minibuffer-header-mode)
  )

(provide 'init-appeareance)

;;; init-appeareance.el ends here
