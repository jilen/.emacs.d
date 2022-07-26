;;; init-typescript.el --- Typescript Dev setup

;;; Commentary:
;;

;;; Code:


(use-package typescript-mode)
(use-package add-node-modules-path
  :hook (typescript-mode))

(provide 'init-typescript)

;;; init-typescript.el ends here
