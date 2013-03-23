;; MMM-Mode
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 0)
;; Set up an mmm group for fancy html editing
(mmm-add-mode-ext-class 'html-mode nil 'html-js)
(mmm-add-mode-ext-class 'html-mode nil 'html-css)
(mmm-add-mode-ext-class 'html-mode nil 'html-css-attribute)
