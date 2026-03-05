;;; init-appeareance.el --- Setup themes and other appearance staffs

;;; Commentary:
;;

;;; Code:

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq default-frame-alist '((undecorated . t)))

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

(global-display-line-numbers-mode)

(setq auto-window-vscroll nil)

;; Theme setup.
(use-package ef-themes
  :config
  (load-theme 'ef-light t))


(use-package eldoc-box
  :hook (prog-mode . eldoc-box-hover-at-point-mode)
  :config
  (custom-set-faces
   '(eldoc-box-border ((t (:inherit child-frame-border))))
   ))


(use-package doom-modeline
  :init
  ;; (setopt doom-modeline-height 30)
  :hook (after-init . doom-modeline-mode))

(use-package indent-bars
  :config
  (require 'indent-bars-ts)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.3))
  (indent-bars-highlight-current-depth '(:blend 0.8))
  (indent-bars-pattern "...")
  (indent-bars-width-frac 0.1)
  (indent-bars-treesit-support t)
  (indent-bars-display-on-blank-lines nil)
  :hook ((yaml-mode sgml-mode) . indent-bars-mode))

(defun setup-indent-bars ()
  "Enable indentbars."
  (unless (derived-mode-p 'web-mode)
    (setq-local indent-bars-display-on-blank-lines t))
  (indent-bars-mode 1))

(add-hook 'prog-mode-hook #'setup-indent-bars)

(use-package lin
  :init
  (setq lin-mode-hooks
        '(bongo-mode-hook
          dired-mode-hook
          elfeed-search-mode-hook
          git-rebase-mode-hook
          grep-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          ledger-report-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          mu4e-headers-mode-hook
          notmuch-search-mode-hook
          notmuch-tree-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          pdf-outline-buffer-mode-hook
          proced-mode-hook
          tabulated-list-mode-hook))
  :config
  (lin-global-mode 1))

(use-package nerd-icons
  :init
  (dolist (entry '(
                   ("sc" nerd-icons-devicon "nf-dev-scala" :face nerd-icons-red)
                   ("mill" nerd-icons-devicon "nf-dev-scala" :face nerd-icons-red)
                   ))
    (add-to-list 'nerd-icons-extension-icon-alist entry))
  (add-to-list 'nerd-icons-mode-icon-alist '(scala-ts-mode nerd-icons-devicon "nf-dev-scala" :face nerd-icons-red)))

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

(show-paren-mode 1)


(provide 'init-appeareance)

;;; init-appeareance.el ends here
