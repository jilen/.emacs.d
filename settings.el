;;;gui related settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(electric-pair-mode 1)
(ido-mode t)
(setq inhibit-startup-message t)


;;theme
(load-theme 'solarized-dark t)

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

;;semantic-mode settings
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode
				  (global-semantic-highlight-edits-mode
				   (if window-system 1 -1))
				  (global-semantic-show-unmatched-syntax-mode 1)
				  (global-semantic-show-parser-state-mode 1)))
(semantic-mode 1)
