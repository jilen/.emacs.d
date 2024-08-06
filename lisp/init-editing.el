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

(setq split-width-threshold nil)
(setq split-height-threshold nil)

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)


;; Use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Auto revert if file changed
(global-auto-revert-mode)

;; Make other-window command much more usable
(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x o" . ace-window)
  ("C-x C-o" . ace-swap-window))

(use-package which-key
  :config
  (which-key-mode))

(setq
 locale-coding-system 'utf-8
 set-terminal-coding-system 'utf-8-unix
 set-keyboard-coding-system 'utf-8
 set-selection-coding-system 'utf-8
 prefer-coding-system 'utf-8)

(when (boundp 'mac-carbon-version-string)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq mac-pass-command-to-system nil))

(use-package vundo)



(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(provide 'init-editing)

;;; init-editing.el ends here
