;;; init-scheme.el --- Scheme dev env setup.

;;; Commentary:
;;

;;; Code:

(use-package geiser-chez
  :init
  (setq geiser-chez-binary "chez"))

(provide 'init-scheme)

;;; init-scheme.el ends here
