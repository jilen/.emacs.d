;;;emacs eclim module init file
(require 'eclim)
(global-eclim-mode)
(require 'eclimd)


(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)


(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

