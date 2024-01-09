;;; init-eglot.scala --- Eglot -- lsp client for emacs setup. -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; (use-package eglot)

(if (version<= "29" emacs-version)
    (require 'eglot)
  (use-package eglot))

(defun npm-prj-root (dir)
  "Locate Vue root from DIR."
  (if (boundp 'eglot-lsp-context)
      (when-let ((root (locate-dominating-file dir "package.json")))
        (if (version<= "29.0" emacs-version)
            (list 'vc 'Git root)
          (list 'vc root)))
    (project-try-vc dir)))

(defun set-project-root-for-npm ()
  "Set prj root for eglot."
  (add-hook 'project-find-functions #'npm-prj-root))

(with-eval-after-load "js-mode"
  (add-hook 'js-mode-hook #'set-project-root-for-npm))
(with-eval-after-load "typescript-mode"
  (add-hook 'typescript-mode #'set-project-root-for-npm))


(eval-when-compile (require 'cl-lib))
(require 'seq)
(require 'eglot)
(require 'jsonrpc)

(defun eglot-booster-plain-command (com)
  "Test if command COM is a plain eglot server command."
  (and (consp com)
       (not (integerp (cadr com)))
       (not (seq-intersection '(:initializationOptions :autoport) com))))

(defun eglot-booster ()
  "Boost plain eglot server programs with emacs-lsp-booster.
The emacs-lsp-booster program must be compiled and available on
variable `exec-path'.  Only local stdin/out based lsp servers can
be boosted."
  (interactive)
  (unless (executable-find "emacs-lsp-booster")
    (user-error "The emacs-lsp-booster program is not installed"))
  (if (get 'eglot-server-programs 'lsp-booster-p)
      (message "eglot-server-programs already boosted.")
    (let ((cnt 0)
    (orig-read (symbol-function 'jsonrpc--json-read))
    (boost '("emacs-lsp-booster" "--json-false-value" ":json-false" "--")))
      (dolist (entry eglot-server-programs)
  (cond
   ((functionp (cdr entry))
    (cl-incf cnt)
    (let ((fun (cdr entry)))
      (setcdr entry (lambda (&rest r) ; wrap function
          (let ((res (apply fun r)))
            (if (eglot-booster-plain-command res)
          (append boost res)
        res))))))
   ((eglot-booster-plain-command (cdr entry))
    (cl-incf cnt)
    (setcdr entry (append boost (cdr entry))))))
      (defalias 'jsonrpc--json-read
  (lambda ()
    (or (and (= (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
        (funcall orig-read))))
      (message "Boosted %d eglot-server-programs" cnt))
    (put 'eglot-server-programs 'lsp-booster-p t)))

(defun eglot-booster-reset ()
  (put 'eglot-server-programs 'lsp-booster-p nil))

(eglot-booster)

(provide 'init-eglot)

;;; init-eglot.el ends here
