;;;gui related settings
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)


;;theme
(load-theme 'monokai t)

;;backup directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;global semantic mode
