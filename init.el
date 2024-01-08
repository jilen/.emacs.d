;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) (file-chase-links load-file-name))))
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq max-lisp-eval-depth 10000)


;; Bootstrap config


(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-package)
(require 'init-editing)
(require 'init-exec-path)
(require 'init-appeareance)
(require 'init-git)
(require 'init-project)
(require 'init-consult)
(require 'init-node-path)
(require 'init-corfu)
(require 'init-codestyle)
(require 'init-ligatures)
(require 'init-flycheck)
(require 'init-javascript)
(require 'init-scala)
(require 'init-vue)
(require 'init-typescript)
(require 'init-php)
(require 'init-pyim)
(require 'init-yaml)
(require 'init-ensime)
(require 'init-markdown)
(require 'init-org)
;; (require 'init-codeium)
;; (require 'init-lsp)
(require 'init-eglot)
(require 'init-kotlin)


(provide 'init)

;;; init.el ends here
