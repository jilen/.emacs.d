;;; init-javascript.el --- JavaScript utitlies

;;; Commentary:
;;

(use-package add-node-modules-path)

(use-package js2-mode
  :custom
  (js2-strict-missing-semi-warning nil)
  (js-switch-indent-offset 2)
  :mode "\\.js\\'"
  :hook (js-mode . add-node-modules-path))

(provide 'init-javascript)

;;; init-javascript.el ends here
