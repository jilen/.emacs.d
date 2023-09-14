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
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
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
(use-package modus-themes
  :config
  (load-theme 'modus-operandi-deuteranopia t))

(use-package doom-modeline
  ;; Enable mood-line
  :init
  (setq doom-modeline-height (truncate (* (frame-char-height) 1.8)))
  :config
  (doom-modeline-mode))

(use-package indent-bars
  :load-path "~/.emacs.d/site-lisp/indent-bars/"
  :init
  (setq
   indent-bars-pattern "."
   indent-bars-width-frac 0.15
   indent-bars-display-on-blank-lines t
   indent-bars-color '(highlight :face-bg t :blend 0.2)
   indent-bars-highlight-current-depth '(:face default :blend 0.4))
  :hook ((prog-mode) . indent-bars-mode))

(defconst preferred-font-height 120)
(defconst preferred-line-number-height (- preferred-font-height 10))
(set-face-attribute
 'default nil
 :height preferred-font-height)

(set-face-attribute 'line-number nil :height preferred-line-number-height )
(set-face-attribute
 'line-number-current-line nil
 :weight (face-attribute 'default :weight)
 :height preferred-line-number-height)

(use-package nerd-icons)



(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)))

(use-package nerd-icons-dired
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . nerd-icons-dired-mode))


(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package color-identifiers-mode
  :init
  (add-hook 'after-init-hook 'global-color-identifiers-mode))

(show-paren-mode 1)

(provide 'init-appeareance)

;;; init-appeareance.el ends here
