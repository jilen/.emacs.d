;;; init-codestyle.el --- codestyle setting



;;; Commentary:
;;

;;; Code:

(set-default 'indent-tabs-mode nil)

;; The fact that we have to do this is also quite embarrassing.
(setq sentence-end-double-space nil)

(global-whitespace-mode 1)
(setq whitespace-style '(face
                         tabs spaces trailing lines space-before-tab newline
                         empty space-after-tab
                         tab-mark
                         missing-newline-at-eof))

;; Set default indentation for various languages (add your own!)
(setq-default tab-width 2)
;; Javascript
(setq-default js2-basic-offset 2)
;; JSON
(setq-default js-indent-level 2)
;; Coffeescript
(setq coffee-tab-width 2)
;; Typescript
(setq typescript-indent-level 2
      typescript-expr-indent-offset 2)
;; Python
(setq-default py-indent-offset 2)
;; XML
(setq-default nxml-child-indent 2)
;; CSS
(setq-default css-indent-offset 2)
;; C
(setq-default c-basic-offset 2)
;; HTML etc with web-mode
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-style-padding 0
              web-mode-script-padding 0)

(provide 'init-codestyle)

;;; init-codestyle.el ends here
