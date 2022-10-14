;;; init-javascript.el --- JavaScript utitlies

;;; Commentary:
;;


;;; Code:

(use-package js2-mode
  :custom
  (js2-strict-missing-semi-warning nil)
  (js-switch-indent-offset 2)
  :mode "\\.js\\'")

(define-derived-mode json-mode js-mode "Json")
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(provide 'init-javascript)

;;; init-javascript.el ends here
