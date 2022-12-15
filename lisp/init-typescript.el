;;; init-typescript.el --- Typescript Dev setup

;;; Commentary:
;;

;;; Code:


(if (version<=  emacs-version "29.0")
    (use-package typescript-mode))


(use-package add-node-modules-path
  :hook (typescript-mode . add-node-modules-path))

(provide 'init-typescript)

;;; init-typescript.el ends here
