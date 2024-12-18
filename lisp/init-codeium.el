;;; init-codeium.el --- Codeium AI assist

;;; Commentary:
;;

;; we recommend using use-package to organize your init.el
(use-package codeium
  :load-path "~/.emacs.d/site-lisp/codeium"
  ;; if you use straight
  ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
  ;; otherwise, make sure that the codeium.el file is on load-path

  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

  :config
  (setq use-dialog-box nil) ;; do not use popup boxes

  ;; if you don't want to use customize to save the api-key
  ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; alternatively for a more extensive mode-line
  ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (defun my-codeium/document/text ()
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
          ;; if you change the text, you should also change the cursor_offset
          ;; warning: this is measured by UTF-8 encoded bytes
          (defun my-codeium/document/cursor_offset ()
            (codeium-utf8-byte-length
             (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
          (setq codeium/document/text 'my-codeium/document/text)
          (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))))
(provide 'init-codeium)
;;; init-codeium.el ends here
