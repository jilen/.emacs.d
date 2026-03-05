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

  (defun mp-flycheck-setup-next-checker (primary-checker &optional preferred-checker)
    "Run a checker after PRIMARY-CHECKER.
If PREFERRED-CHECKER supports `major-mode', use it. Otherwise pick the first
checker that supports `major-mode' and is not PRIMARY-CHECKER."
    (when (and (flycheck-valid-checker-p primary-checker)
               (bound-and-true-p flycheck-mode))
      (when-let ((next-checker
                  (or (and preferred-checker
                           (flycheck-valid-checker-p preferred-checker)
                           (flycheck-checker-supports-major-mode-p preferred-checker major-mode)
                           preferred-checker)
                      (cl-find-if
                       (lambda (checker)
                         (and (not (eq checker primary-checker))
                              (flycheck-checker-supports-major-mode-p checker major-mode)))
                       flycheck-checkers))))
        (flycheck-add-next-checker primary-checker `(info . ,next-checker)))))

  (defun mp-flycheck-biome-config-exists-p ()
    "Whether there is a Biome config for the current buffer."
    (and buffer-file-name
         (or (locate-dominating-file buffer-file-name "biome.json")
             (locate-dominating-file buffer-file-name "biome.jsonc"))))

  (defun mp-flycheck-biome--find-working-directory (_checker)
    "Look for a working directory to run Biome in."
    (when buffer-file-name
      (or (locate-dominating-file buffer-file-name "node_modules")
          (mp-flycheck-biome-config-exists-p))))

  :hook ((flycheck-mode . mp-flycheck-prefer-eldoc))
  :config
  (flycheck-define-checker javascript-biome
    "A Biome checker for JavaScript and TypeScript."
    :command ("biome"
              "lint"
              "--skip-parse-errors"
              "--reporter=checkstyle"
              source-original)
    :error-parser flycheck-parse-checkstyle
    :enabled (lambda () (mp-flycheck-biome-config-exists-p))
    :modes (js-ts-mode
            typescript-ts-mode
            tsx-ts-mode
            web-mode
            vue-mode)
    :working-directory mp-flycheck-biome--find-working-directory
    :verify
    (lambda (_)
      (let* ((default-directory
              (flycheck-compute-working-directory 'javascript-biome))
             (have-config (mp-flycheck-biome-config-exists-p)))
        (list
         (flycheck-verification-result-new
          :label "config file"
          :message (if have-config "found" "missing")
          :face (if have-config 'success 'warning))))))

  (add-to-list 'flycheck-checkers 'javascript-biome)
  (global-flycheck-mode))



(provide 'init-flycheck)

;;; init-flycheck.el ends here
