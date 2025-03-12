;;; init-assist.el --- Codeium AI assist

;;; Commentary:
;;


(use-package codeium
  :load-path "~/.emacs.d/site-lisp/codeium/"

  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  :config
  (setq codeium/metadata/api_key (getenv "CODEIUM_API_KEY"))
  (setq use-dialog-box nil) ;; do not use popup boxes

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion)))))


(use-package aidermacs
  :load-path "~/.emacs.d/site-lisp/aidermacs"
  :bind (("C-c a" . aidermacs-transient-menu))

  :config
  (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-backend 'vterm)
  (aidermacs-auto-commits nil)
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "deepseek/deepseek-chat")
  (aidermacs-architect-model "r1"))

(provide 'init-assist)
;;; init-assist.el ends here
