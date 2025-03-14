;;; init-scala.el --- Scala development env

;;; Commentary:
;;

;;; Code:

(use-package scala-ts-mode
  :load-path "~/.emacs.d/site-lisp/scala-ts-mode/"
  :custom
  (treesit-font-lock-level 4)
  :mode "\\.scala\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-ts-mode))
  (add-to-list 'major-mode-remap-alist '(scala-mode . scala-ts-mode)))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  (setq-default sbt:program-options '("-Djline.terminal=none" "-Dsbt.supershell=false" "-Dquill.macro.log=false"))
  (add-hook 'sbt-mode-hook
            (lambda ()
              (setq prettify-symbols-alist
                    `((,(expand-file-name (directory-file-name default-directory)) . ?⌂)
                      (,(expand-file-name "~") . ?~)))
              (prettify-symbols-mode t)))

  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs '(scala-ts-mode . ("metals")))
  )

(setq compilation-read-command nil)

(require 'ansi-color)
(defun colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'colorize-compilation)


(defun sbt-compile ()
  "Compile sbt project."
  (interactive)
  (sbt-command "compile"))


(with-eval-after-load "apheleia"
  (add-to-list 'apheleia-formatters
               '(scalafmt . ("apheleia-from-project-root"
                             ".scalafmt.conf" "scalafmt" "--assume-filename"
                             (or (apheleia-formatters-local-buffer-file-name)
                                 (apheleia-formatters-mode-extension)
                                 ".scala")
                             "--non-interactive" "--stdout" "--stdin")))
  (add-to-list 'apheleia-mode-alist '(scala-ts-mode . scalafmt))
  (add-to-list 'apheleia-mode-alist '(scala-mode . scalafmt)))

(require 'project)
(defun setup-compile ()
  "Compile project command."
  (when (and (project-current) (file-exists-p (concat (project-root (project-current)) "build.sc")))
    (setq-local compile-command "env TERM=dumb mill  --disable-prompt -s __:^TestModule.compile" )))

(add-hook 'scala-mode-hook #'setup-compile)
(add-hook 'scala-ts-mode-hook #'setup-compile)

(require 'sbt-mode-project)

(defun scalafmt (p)
  "Format specified file.  P is the file path."
  (shell-command (concat "scalafmt " p)))

(defun scalariform (c f)
  "Format specified file. C is the config, F is the file path."
  (shell-command (concat "scalariform -q " "--preferenceFile=" c " " f)))


(defun format-project ()
  "Format project."
  (interactive)
  (let ((default-directory (locate-dominating-file (buffer-file-name (current-buffer)) ".scalafmt.conf")))
    (cond
     ((file-exists-p ".scalafmt.conf") (scalafmt (buffer-file-name (current-buffer))))
     ((file-exists-p ".scalariform.conf") (scalariform ".scalariform.conf" (buffer-file-name (current-buffer)))))))

(defun compile-project()
  "Compile project, sbt/mill."
  (interactive)
  (if (file-exists-p (concat (project-root (project-current)) "build.sc"))
      (project-compile)
    (sbt-compile)))

(global-set-key (kbd "C-c b b") 'compile-project)
(global-set-key (kbd "C-c b f") 'format-project)
;;(global-set-key (kbd "C-c b p") 'sbt-publish-local)

;; Prevent things like flycheck run against sbt file
(define-derived-mode sbt-build-mode scala-mode ".sbt")
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . sbt-build-mode))





;;; Scala3 flycheck checker, comment this if not using scalac 3

(defconst error-first-line-re "Error: \\([^\\:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)")

(require 'flycheck)
(defun extract-errors (lines errors)
  "Extract errors from LINES, init ERRORS should be nil."
  (let* ((filter-lines (seq-drop-while (lambda(s) (not (string-match error-first-line-re s))) lines))
         (first-line (car filter-lines)))
    (if (null first-line)
        errors
      (progn (string-match error-first-line-re first-line)
             (let ((file (match-string 1 first-line))
                   (row (string-to-number (match-string 2 first-line)))
                   (col (string-to-number (match-string 3 first-line)))
                   (msg (car (seq-drop filter-lines 3))))
               (extract-errors (seq-drop filter-lines 4) (cons (flycheck-error-new-at row col 'error msg ) errors)))))
    )
  )

(defun scala3-error-parser (output checker buffer)
  "Parse scala3 compiler output errors from OUTPUT."
  (extract-errors (string-lines output) nil))

(flycheck-define-checker scala3
  "A Scala syntax checker using the Scala compiler.
See URL `https://www.scala-lang.org/'."
  :command ("scalac3" "-Ystop-after:parser" "-source:3.5" "-color:never" source)
  :error-parser scala3-error-parser
  :modes scala-mode
  :next-checkers ((warning . scala-scalastyle)))

(add-to-list 'flycheck-disabled-checkers 'scala)
(add-to-list 'flycheck-checkers 'scala3)

(use-package web-mode
  :mode (("\\.scala\\.html$" . web-mode)))



(provide 'init-scala)

;;; init-scala.el ends here
