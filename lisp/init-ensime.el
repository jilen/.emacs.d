;;; init-ensime.el --- Ensime support

;;; Commentary:
;;
;;; Code:


(use-package ensime-mode
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/ensime/lisp"
  :commands ensime-mode
  :hook
  (scala-mode . ensime-mode)
  :bind
  (:map ensime-mode-map
        ("M-." . ensime-jump-to-definition)
        ("C-c C-i t" . ensime-type-at-point)
        ("C-c C-i s" . ensime-symbol-at-point)
        ("C-c C-r i" . ensime-import-symbol-at-point))
  :config
  (with-eval-after-load "cape"
  (add-to-list 'completion-at-point-functions (cape-company-to-capf #'ensime-company))))

(provide 'init-ensime)

;;; init-ensime.el ends here
