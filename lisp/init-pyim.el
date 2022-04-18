;;; init-pyim.el --- Chinese input support

;;; Commentary:
;;
;;; Code:

(use-package posframe)
(use-package pyim
  :config
  (setq pyim-page-tooltip 'posframe)
  (require 'pyim-liberime)
  (setq pyim-default-scheme 'rime-quanpin)
  (global-set-key (kbd "C-\\") 'toggle-input-method)
  (setq default-input-method "pyim"))

(use-package pyim-basedict
  :config
  (pyim-basedict-enable))


(provide 'init-pyim)

;;; init-pyim.el ends here
