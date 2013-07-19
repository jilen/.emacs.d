;;;gui related settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(setq inhibit-startup-message t)


;;theme
(load-theme 'tango-dark t)

;;backup directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;paren-showing
(require 'paren)
    (set-face-background 'show-paren-match-face (face-background 'default))
    (set-face-foreground 'show-paren-match-face "#def")
    (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
