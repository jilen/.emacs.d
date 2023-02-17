;;; init-typescript.el --- Typescript Dev setup

;;; Commentary:
;;

;;; Code:


(if (version<=  emacs-version "29.0")
    (use-package typescript-mode)
  (progn
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))
  )


(use-package add-node-modules-path
  :hook (typescript-mode . add-node-modules-path))

(provide 'init-typescript)

;;; init-typescript.el ends here
