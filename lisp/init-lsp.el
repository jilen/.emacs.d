;;; init-lsp.el --- Init lsp mode



;;; Commentary:
;;

;;; Code:
(use-package lsp-mode
  :custom
  (lsp-enable-snippet nil)
  (lsp-completion-provider :none) ;; use corfu
  (lsp-enable-on-type-formatting nil)
  :init
  (setq read-process-output-max (* 3 1024 1024)) ;; 1mb
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . lsp-mode-setup-completion))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)


(defun make-doc-frame ()
  "Create the child frame and return it."
  (lsp-ui-doc--delete-frame)
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (name-buffer (lsp-ui-doc--make-buffer-name))
         (buffer (get-buffer name-buffer))
         (params (append lsp-ui-doc-frame-parameters
                         `((name . "")
                           (child-frame-border-width . 1)
                           (default-minibuffer-frame . ,(selected-frame))
                           (minibuffer . ,(minibuffer-window))
                           (left-fringe . 1)
                           (right-fringe . 1)
                           (cursor-type . nil)
                           (lsp-ui-doc--no-focus . t))))
         (window (display-buffer-in-child-frame
                  buffer
                  `((child-frame-parameters . ,params))))
         (frame (window-frame window)))
    (with-current-buffer buffer
      (lsp-ui-doc-frame-mode 1))
    (set-frame-parameter nil 'lsp-ui-doc-buffer buffer)
    (set-window-dedicated-p window t)
    (run-hook-with-args 'lsp-ui-doc-frame-hook frame window)
    (when lsp-ui-doc-use-webkit
      (define-key (current-global-map) [xwidget-event]
                  (lambda ()
                    (interactive)
                    (let ((xwidget-event-type (nth 1 last-input-event)))
                      ;; (when (eq xwidget-event-type 'load-changed)
                      ;;   (lsp-ui-doc--move-frame (lsp-ui-doc--get-frame)))
                      (when (eq xwidget-event-type 'javascript-callback)
                        (let ((proc (nth 3 last-input-event))
                              (arg (nth 4 last-input-event)))
                          (funcall proc arg))))))
      (lsp-ui-doc--webkit-run-xwidget))
    frame))
(use-package "lsp-ui"
  :config
  (advice-add 'lsp-ui-doc--make-frame :override #'make-doc-frame)
  :init
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable t)
  (setq lsp-lens-enable nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-position 'at-point))

(provide 'init-lsp)

;;; init-lsp.el ends here
