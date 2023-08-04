;;; init-codeium.el --- Codeium AI assist

;;; Commentary:
;;

(use-package codeium
  :load-path "~/.emacs.d/site-lisp/codeium/"
   :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)

    (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
  )
(provide 'init-codeium)

;;; init-codeium.el ends here
