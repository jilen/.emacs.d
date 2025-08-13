;;; init-flycheck.el --- Flycheck setup. some are stolen from doom emacs

;;; Commentary:
;;

;;; Code:

(use-package flycheck
  :preface

  (defun mp-flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
                  (format "%s: %s"
                          (let ((level (flycheck-error-level err)))
                            (pcase level
                              ('info (propertize "I" 'face 'flycheck-error-list-info))
                              ('error (propertize "E" 'face 'flycheck-error-list-error))
                              ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                              (_ level)))
                          (flycheck-error-message err))
                  :thing (or (flycheck-error-id err)
                             (flycheck-error-group err))
                  :face 'font-lock-doc-face))
       flycheck-errors)))

  (defun mp-flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'mp-flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))

  :hook ((flycheck-mode . mp-flycheck-prefer-eldoc))
  :config
  (global-flycheck-mode))

;; (use-package flyover
;;   :init
;;   (setopt flyover-text-tint-percent 100)
;;   (setopt flyover-text-tint 'lighter)
;;   (setopt flyover-use-theme-colors t)
;;   (setopt flyover-show-virtual-line t)
;;   (setopt flyover-virtual-line-type 'arrow)
;;   (setopt flyover-show-at-eol t)

;;   (setopt flyover-levels '(error warning info))
;;   (add-hook 'flycheck-mode-hook #'flyover-mode))



(provide 'init-flycheck)

;;; init-flycheck.el ends here
