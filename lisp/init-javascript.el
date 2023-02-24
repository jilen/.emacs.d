;;; init-javascript.el --- JavaScript utitlies

;;; Commentary:
;;


;;; Code:



(if (version< emacs-version "29")
    (progn
      (use-package js2-mode
        :custom
        (js2-strict-missing-semi-warning nil)
        (js-switch-indent-offset 2)
        :mode "\\.js\\'")
      (use-package add-node-modules-path
        :hook (js-mode . add-node-modules-path)))

  (progn
    (setq-default js-switch-indent-offset 2)
    (add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))
    (use-package add-node-modules-path
      :hook (js-ts-mode . add-node-modules-path)))
  )




(provide 'init-javascript)

;;; init-javascript.el ends here
