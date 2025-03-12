;;; init-codestyle.el --- codestyle setting



;;; Commentary:
;;

;;; Code:

(set-default 'indent-tabs-mode nil)

;; The fact that we have to do this is also quite embarrassing.
(setq sentence-end-double-space nil)

(global-whitespace-mode 1)
(setq whitespace-line-column 120)
(setq whitespace-style '(face
                         tabs spaces trailing lines space-before-tab newline
                         empty space-after-tab
                         tab-mark
                         missing-newline-at-eof))
;; 默认2格缩进
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default c-basic-offset 2)
(setq-default css-indent-offset 2)
(setq-default sh-basic-offset 2)
(setq-default standard-indent 2)
(setq-default indent-tabs-mode nil)
(setq-default py-indent-offset 2)
(setq-default nxml-child-indent 2)
(setq-default css-indent-offset 2)
(setq-default c-basic-offset 2)

;; Web Mode
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-style-padding 0
              web-mode-script-padding 0)



(provide 'init-codestyle)

;;; init-codestyle.el ends here
