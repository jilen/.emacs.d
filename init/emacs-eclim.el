;;;emacs eclim module init file
(require 'eclim)
(global-eclim-mode)
(require 'eclimd)



(custom-set-variables
 '(eclim-eclipse-dirs '("~/Dev/eclipse")))
(custom-set-variables
 '(eclim-executable "/home/jilen/Dev/eclipse/plugins/org.eclim_2.2.6/bin/eclim"))

(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)

;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
