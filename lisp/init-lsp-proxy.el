;;; init-lsp-proxy.el --- Lsp-proxy mode


;;; Commentary:
;;

;;; Code:
(use-package yasnippet)
(use-package ht)
(use-package lsp-proxy
  :load-path "/usr/share/emacs/site-lisp/lsp-proxy"
  :config
  ;; TypeScript and JavaScript modes
  (add-hook 'tsx-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'js-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'typescript-mode-hook #'lsp-proxy-mode)
  (add-hook 'typescript-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'js-mode-hook #'lsp-proxy-mode)
  (add-hook 'javascript-mode-hook #'lsp-proxy-mode))


(require 'lsp-proxy)  ; Ensure loaded

(defun lsp-proxy--handle-vue-tsserver-request (orig-fun method &rest args)
  "Intercept tsserver/request and forward via execute-command if possible."
  (if (eq method 'tsserver/request)
      (let* ((msg (car args))
             (id (nth 0 msg))
             (command (nth 1 msg))
             (payload (nth 2 msg)))
        ;; Forward using execute-command to current LSP process
        (lsp-proxy--execute-command
         "typescript.tsserverRequest"
         (list :command command :arguments payload)
         ;; No server ID — just use default connection
         "ts_ls"
         :success-fn (lambda (response)
                       (lsp-proxy--notify
                        'tsserver/response
                        (list (list id (plist-get response :body)))))
         :error-fn (lambda (_err)
                     (lsp-proxy--notify
                      'tsserver/response
                      (list (list id nil))))))
    ;; Not our message — pass through
    (apply orig-fun method args)))

(advice-add 'lsp-proxy--handle-notification :around #'lsp-proxy--handle-vue-tsserver-request)


(provide 'init-lsp-proxy)

;;; init-lsp-proxy.el ends here
