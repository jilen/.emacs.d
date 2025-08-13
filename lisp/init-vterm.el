;;; init-vterm.el --- Libvterm intergration

;;; Commentary:
;;

(use-package vterm)

(defun vterm-project-run (cmd)
  "Run CMD in a vterm buffer dedicated to the current project."
  (interactive "sRun in vterm: ")
  (let* ((project-root (project-root (project-current)))
         (buffer-name (format "*vterm-%s*" (project-name (project-current)))))
    (with-current-buffer (get-buffer-create buffer-name)
      (unless (get-buffer-window buffer-name)
        (switch-to-buffer-other-window buffer-name))
      (unless (derived-mode-p 'vterm-mode)
        (let ((default-directory project-root)
              (vterm-buffer-name buffer-name))
          (vterm)))
      (vterm-send-string cmd)
      (vterm-send-string "
"))))

(global-set-key (kbd "C-c t") #'vterm-project-run)


(provide 'init-vterm)

;;; init-vterm.el ends here
