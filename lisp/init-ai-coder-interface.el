;;; init-ai-coder-interface.el --- Add code agent interface

;;; Commentary:
;;

;;; Code:

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

;; (use-package ai-code-interface
;;   :vc (:url "git@github.com:tninja/ai-code-interface.el.git" :branch "main")
;;   :config
;;   (ai-code-set-backend  'claude-code-ide) ;; use claude-code-ide as backend
;;   ;; Enable global keybinding for the main menu
;;   (global-set-key (kbd "C-c a") #'ai-code-menu)
;;   ;; Optional: Set up Magit integration for AI commands in Magit popups
;;   (with-eval-after-load 'magit
;;     (ai-code-magit-setup-transients)))

(provide 'init-ai-coder-interface)

;;; init-ai-coder-interface.el ends here
