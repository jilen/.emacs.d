;;; init-php.el --- Php setup

;;; Commentary:
;;

(defun php-init ()
  (setq c-basic-offset 2))

(use-package php-mode
  :mode "\\.php\\'"
  :hook (php-mode . php-init)
  )

(provide 'init-php)

;;; init-php.el ends here
