;;; init-typescript.el --- Typescript Dev setup

;;; Commentary:
;;

;;; Code:


(use-package typescript-mode
  :mode "\\.tsx\\'"
  )
(use-package add-node-modules-path
  :hook (typescript-mode))

(provide 'init-typescript)

;;; init-typescript.el ends here
