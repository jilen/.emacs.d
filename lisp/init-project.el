;;; init-project.el --- Use project.el for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package project
             :bind-keymap
             ("C-c p" . project-prefix-map))

(eval-after-load "project"
  '(defun project-switch-project (dir)
     "\"Switch\" to another project by running an Emacs command.
When called in a program, it will use the project corresponding
to directory DIR."
     (interactive (list (project-prompt-project-dir)))
     (let ((default-directory dir)
           (project-current-inhibit-prompt t))
       (call-interactively 'project-find-file))))

(provide 'init-project)
;;; init-project.el ends here
