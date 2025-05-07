;;; init-lsp.el --- Init lsp mode



;;; Commentary:
;;

;;; Code:
(use-package lsp-mode
  :custom
  (lsp-enable-snippet nil)
  (lsp-completion-provider :none) ;; use corfu
  (lsp-enable-on-type-formatting nil)
  (read-process-output-max (* 3 1024 1024)) ;; 1mb
  (lsp-eldoc-render-all t)
  :init
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . lsp-mode-setup-completion))

;; Define the alist for custom project root remapping
(defvar my-lsp-root-remap-alist
  '(;; Example entries:
    ;; Key: a substring to look for in the file path (e.g., a directory name or relative path)
    ;; Value: the desired project root path relative to the part of the path before the key
    ;; Example: If file is /path/to/repo/prjA/shared/src/file.el
    ;; Key "prjA/shared" is found.
    ;; Part before key is /path/to/repo/
    ;; New root becomes /path/to/repo/ + "prjA/server" = /path/to/repo/prjA/server
    ;; ("/path/to/a/specific/subfolder/" . "/path/to/the/actual/project/root/") ;; This format might be less useful with the new logic
    ;; ("/another/directory/prefix/" . "/its/desired/root/")
    ("saytu-pro/shared" . "saytu-pro/server"))
  "Alist mapping directory substrings to specific LSP project roots.
When `lsp--calculate-root` is called for a file, this alist is checked first.
If the file's path contains a key (directory substring) in this alist,
the corresponding value (project root) is used to construct the root path
by replacing the key part with the value.
Otherwise, the original `lsp--calculate-root` function is called.")

;; Implement the advice function for `lsp--calculate-root`
(defun my-lsp-calculate-root-remap (orig-fun &rest args)
  "Advice for `lsp--calculate-root` to use `my-lsp-root-remap-alist`.
Looks up the file in `my-lsp-root-remap-alist` first. If a match is found,
calculates the new root by taking the part of the file path before the
matched prefix and appending the value. Otherwise, calls ORIG-FUN with ARGS."
  (let ((file-name (cadr args))) ;; file-name is the second argument to lsp--calculate-root
    (cl-dolist (entry my-lsp-root-remap-alist
                      ;; If loop finishes without finding a match, call original function
                      (apply orig-fun args))
      (let ((prefix (car entry))
            (root (cdr entry)))
        ;; Check if the file-name contains the prefix string
        (when (and (stringp prefix) (stringp file-name))
          (let ((match-index (string-search prefix file-name)))
            (when match-index
              ;; Found a match, calculate the new root
              ;; The new root is the part of file-name before the prefix + the root value
              (let ((part-before-prefix (substring file-name 0 match-index)))
                (message "Using custom LSP root '%s' for file '%s' based on prefix '%s'"
                         (concat part-before-prefix root) file-name prefix) ;; Optional: add a message
                (cl-return (concat part-before-prefix root)))))))))) ;; Return the calculated root and exit dolist

;; Add the advice to `lsp--calculate-root`
(advice-add 'lsp--calculate-root :around #'my-lsp-calculate-root-remap)


(use-package "lsp-ui"
  :init
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil) ;; use eldoc
  (setq lsp-lens-enable nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 0.5)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-position 'at-point)
  )

;; Use Eldoc-box instead of lsp-ui-doc
(use-package eldoc-box
  :config
  (add-hook 'lsp-mode-hook #'eldoc-box-hover-at-point-mode t))

;; --- Flycheck configuration ---


(defun setup-lsp-next-checkers ()
  "Configure flycheck to run other checkers after the 'lsp' checker.
This function is intended to be added to `flycheck-mode-hook`."
  (lsp-diagnostics-lsp-checker-if-needed)
  (when-let ((checker (cl-find-if (lambda (checker)
                                    (and (not (eq checker 'lsp))
                                         (flycheck-checker-supports-major-mode-p checker major-mode)))
                                  flycheck-checkers)))
    (flycheck-add-next-checker 'lsp checker)))

(add-hook 'lsp-managed-mode-hook #'setup-lsp-next-checkers)

;; --- End Flycheck configuration ---


(provide 'init-lsp)

;;; init-lsp.el ends here
