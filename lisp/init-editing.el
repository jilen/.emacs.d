;;; init-editing.el --- Edit behavior of personal state

;;; Commentary:
;; 
(use-package auto-sudoedit
  :config
  (auto-sudoedit-mode 1))

(use-package multiple-cursors
  :commands multiple-cursors-mode
  :config
  ;; MC has `mc-hide-unmatched-lines-mode' bound to C-', which interferes
  ;; with our ability to add more cursors, so we'll just clear the binding.
  ;; TODO: add `mc-hide-unmatched-lines-mode' back somewhere else?
  (bind-keys :map mc/keymap
             ("C-'" . nil))
  :bind (("<insert>" . mc/mark-next-like-this)
         ("S-<insert>" . mc/mark-previous-like-this)
         ("C-'" . mc/mark-more-like-this-extended)
         ("C-\"" . mc/mark-all-like-this-dwim)
         ("C-M-'" . mc/edit-lines)))

(global-set-key (kbd "M-j") 'join-line)

;; Automatically insert matching braces and do other clever
;; things pertaining to braces and such.
(electric-pair-mode 1)

;; Overwrite selection for input
(delete-selection-mode t)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.cache/emacs/backup/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(setq auto-save-file-name-transforms
      `((".*" "~/.cache/emacs/autosave" t)))



(use-package expand-region
  :commands er/expand-region
  :bind ("C-=" . er/expand-region))

(eval-after-load 'semantic
  (add-hook 'semantic-mode-hook
            (lambda ()
              (dolist (x (default-value 'completion-at-point-functions))
                (when (string-prefix-p "semantic-" (symbol-name x))
                  (remove-hook 'completion-at-point-functions x))))))

(setq split-width-threshold nil)
(setq split-height-threshold nil)

;; Use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init-editing)

;;; init-editing.el ends here
