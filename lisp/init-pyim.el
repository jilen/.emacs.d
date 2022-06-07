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
  (require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置`
  (pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
  (global-set-key (kbd "C-\\") 'toggle-input-method)
  (setq default-input-method "pyim"))

(use-package pyim-basedict
  :config
  (pyim-basedict-enable))


(provide 'init-pyim)

;;; init-pyim.el ends here
