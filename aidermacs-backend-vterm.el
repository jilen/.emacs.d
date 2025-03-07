;;; aidermacs-backend-vterm.el --- VTerm backend for aidermacs.el -*- lexical-binding: t; -*-
;; Author: Mingde (Matthew) Zeng <matthewzmd@posteo.net>
;; Version: 1.0.0
;; Keywords: ai emacs agents llm aider ai-pair-programming, convenience, tools
;; URL: https://github.com/MatthewZMD/aidermacs
;; Originally forked from: Kang Tu <tninja@gmail.com> Aider.el
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file implements the VTerm backend for Aidermacs, providing
;; an interface for interacting with the Aider process within a
;; VTerm buffer. It leverages the VTerm package for a more
;; feature-rich terminal experience.
;;
;; Key features include:
;; - Integration with the VTerm package for terminal emulation.
;; - Asynchronous output processing using timers and advices.
;; - Custom keybindings for multi-line input.
;; - Management of the Aider process within a VTerm buffer.
;;
;;; Code:

(require 'vterm nil 'noerror)
(require 'cl-lib)

;; we want to ensure these two variables are dynamic binding
(defvar vterm-shell)
(defvar vterm-buffer-name)

;; Forward declaration to avoid compiler warnings
(declare-function vterm--render "vterm")
(declare-function vterm--get-prompt-point "vterm")
(declare-function vterm-other-window "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-insert "vterm")

(declare-function aidermacs--prepare-for-code-edit "aidermacs")
(declare-function aidermacs--cleanup-temp-buffers "aidermacs")
(declare-function aidermacs--show-ediff-for-edited-files "aidermacs")
(declare-function aidermacs--detect-edited-files "aidermacs")
(declare-function aidermacs--store-output "aidermacs")
(declare-function aidermacs--is-aidermacs-buffer-p "aidermacs")
(declare-function aidermacs-get-buffer-name "aidermacs")

(defvar-local aidermacs--vterm-active-timer nil
  "Store the active timer for vterm output processing.")

(defvar-local aidermacs--vterm-last-check-point nil
  "Store the last position checked in the vterm buffer.")


(defvar-local aidermacs-vterm-check-interval 0.5
  "Interval in seconds between checks for command completion in vterm.")


(defcustom aidermacs-vterm-multiline-newline-key "S-<return>"
  "Key binding to enter a newline without sending in vterm."
  :type 'string
  :group 'aidermacs)

(defvar aidermacs-prompt-regexp)

(defun aidermacs--vterm-check-finish-sequence-repeated (proc orig-filter start-point)
  "Check for the finish sequence in PROC's buffer.
PROC is the process to check.  ORIG-FILTER is the original process filter.
START-POINT is the starting position for output capture.
If the finish sequence is detected, store the output via
`aidermacs--store-output`, restore ORIG-FILTER, and return t."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      ;; Render vterm if the function is available
      (when (fboundp 'vterm--render)
        (condition-case nil
            (vterm--render)
          (error nil)))
      ;; Get current prompt point
      (let* ((prompt-point (condition-case nil
                               (vterm--get-prompt-point)
                             (error (point-max))))
             ;; Only check if we have a new prompt or haven't checked this position yet
             (last-check (or aidermacs--vterm-last-check-point start-point))
             (should-check (> prompt-point last-check))
             ;; Simplified pattern that just looks for a shell prompt
             (expected aidermacs-prompt-regexp))

        ;; Update the last check point
        (setq aidermacs--vterm-last-check-point prompt-point)

        (when should-check
          (let* ((seq-start (or (save-excursion
                                  (goto-char prompt-point)
                                  (condition-case nil
                                      (search-backward "\n" nil t)
                                    (error nil)))
                                (point-min)))
                 ;; Only get the prompt line, not the whole sequence (limit to 200 chars)
                 (prompt-line-end (min (+ seq-start 200) (point-max)))
                 (prompt-line (buffer-substring-no-properties seq-start prompt-line-end)))

            ;; If we found a shell prompt indicating output finished
            (when (string-match-p expected prompt-line)
              (let ((output (buffer-substring-no-properties start-point seq-start)))
                (setq-local aidermacs--vterm-processing-command nil)
                (aidermacs--store-output (string-trim output))
                (let ((edited-files (aidermacs--detect-edited-files)))
                ;; Check if any files were edited and show ediff if needed
                  (if edited-files
                      (aidermacs--show-ediff-for-edited-files edited-files)
                    (aidermacs--cleanup-temp-buffers))
                  ;; Restore the original process filter now that we've finished processing
                  ;; this command's output. This returns vterm to its normal behavior.
                  (set-process-filter proc orig-filter)
                  (aidermacs--maybe-cancel-active-timer (process-buffer proc)))))))))))

(defvar-local aidermacs--vterm-processing-command nil
  "Flag to indicate if we're currently processing a command.")

(defun aidermacs--vterm-output-advice (orig-fun &rest args)
  "Capture vterm output until the finish sequence appears.
ORIG-FUN is the original function being advised.  ARGS are its arguments.
This sets a temporary process filter that checks for the finish sequence
after each output chunk, reducing the need for timers."
  (if (and (aidermacs--is-aidermacs-buffer-p)
           (not (string-empty-p aidermacs--last-command)))
      (let* ((start-point (condition-case nil
                              (vterm--get-prompt-point)
                            (error (point-min))))
             (proc (get-buffer-process (current-buffer)))
             (orig-filter (process-filter proc)))

        ;; Store the command for tracking in the correct buffer
        (with-current-buffer (process-buffer proc)
          (when (and args (car args) (stringp (car args)))
            (setq aidermacs--last-command (car args)))
          ;; Set flag that we're processing a command
          (setq aidermacs--vterm-processing-command t)
          ;; Initialize tracking variables
          (setq aidermacs--vterm-last-check-point nil)
          ;; Cancel any existing timer first
          (aidermacs--maybe-cancel-active-timer)

          ;; Create a new timer immediately to start checking for command completion
          (setq aidermacs--vterm-active-timer
                (run-with-timer
                 aidermacs-vterm-check-interval
                 aidermacs-vterm-check-interval
                 (lambda ()
                   ; Check if we're still in a valid state
                   (aidermacs--vterm-check-finish-sequence-repeated proc orig-filter start-point))))
          (apply orig-fun args)))
    (apply orig-fun args)))

(defun aidermacs--maybe-cancel-active-timer (&optional buffer)
  "Cancel the active timer if it exists.
Use BUFFER if provided, otherwise retrieve it from `aidermacs-get-buffer-name'"
  (with-current-buffer (get-buffer (or buffer (aidermacs-get-buffer-name)))
    (when (timerp aidermacs--vterm-active-timer)
      (cancel-timer aidermacs--vterm-active-timer)
      (setq aidermacs--vterm-active-timer nil))))

(defun aidermacs-run-vterm (program args buffer-name)
  "Create a vterm-based buffer and run aidermacs program.
PROGRAM is the command to run.  ARGS is a list of command line arguments.
BUFFER-NAME is the name for the vterm buffer."
  (unless (require 'vterm nil t)
    (error "Vterm package is not available"))
  (unless (get-buffer buffer-name)
    (let* ((mode (if (eq (frame-parameter nil 'background-mode) 'dark)
                     "--dark-mode"
                   "--light-mode"))
           (cmd (mapconcat #'identity (append (list program mode) args) " "))
           (vterm-buffer-name buffer-name)
           (vterm-shell cmd))
      (with-current-buffer (vterm-other-window)
        (setq-local vterm-max-scrollback 1000
                    aidermacs--vterm-active-timer nil
                    aidermacs--vterm-last-check-point nil)
        (advice-add 'vterm-send-return :around #'aidermacs--vterm-output-advice)
        (advice-add 'vterm-send-return :before #'aidermacs--vterm-capture-keyboard-input)
        (advice-add 'vterm--self-insert :after #'aidermacs--cleanup-temp-files-on-interrupt-vterm)
        ;; Set up multi-line keybinding
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map (current-local-map))
          (define-key map (kbd aidermacs-vterm-multiline-newline-key) #'aidermacs-vterm-insert-newline)
          (use-local-map map))
        ;; Add cleanup hook
        (add-hook 'kill-buffer-hook #'aidermacs--vterm-cleanup nil t))))
  buffer-name)

(defun aidermacs--send-command-vterm (buffer command)
  "Send command to the aidermacs vterm buffer.
BUFFER is the target buffer to send to.  COMMAND is the text to send."
  (with-current-buffer buffer
    ;; Cancel any existing timer to prevent resource leaks
    (aidermacs--maybe-cancel-active-timer)
    ;; Only process if we have a non-empty command
    (when (and command (not (string-empty-p (string-trim command))))
      ;; Set processing flag to true before sending command
      (setq-local aidermacs--vterm-processing-command t)
      ;; Store the command for tracking
      (setq-local aidermacs--last-command command)
      ;; Send the command
      (vterm-send-string command)
      (vterm-send-return))))

(defun aidermacs-vterm-insert-newline ()
  "Insert a newline in vterm without sending the command."
  (interactive)
  (vterm-insert "\n"))

(defun aidermacs--vterm-capture-keyboard-input ()
  "Capture keyboard input in vterm."
  (when (aidermacs--is-aidermacs-buffer-p)
    ;; Get the current line content which should be the command
    ;; TODO: current line may not be enough
    (save-excursion
      (let* ((prompt-point (condition-case nil
                               (vterm--get-prompt-point)
                             (error (point-min))))
             (command (buffer-substring-no-properties
                       prompt-point
                       (line-end-position))))
        (when (not (string-empty-p command))
          (setq-local aidermacs--last-command command)
          ;; Always prepare for potential edits
          (aidermacs--prepare-for-code-edit))))))

(defun aidermacs--vterm-cleanup ()
  "Clean up vterm resources when buffer is killed."
  (aidermacs--maybe-cancel-active-timer)
  (setq-local aidermacs--vterm-last-check-point nil)
  (setq-local aidermacs--vterm-processing-command nil)
  (advice-remove 'vterm-send-return #'aidermacs--vterm-capture-keyboard-input))

(defun aidermacs--cleanup-temp-files-on-interrupt-vterm (&rest _args)
  "Run `aidermacs--cleanup-temp-buffers' after interrupting a vterm subjob.
_ARGS are the arguments."
  (when (and (aidermacs--is-aidermacs-buffer-p)
             (equal (this-command-keys) "\C-c\C-c"))
    ;; Reset processing flag when user interrupts
    (setq-local aidermacs--vterm-processing-command nil)
    ;; Cancel any active timer
    (aidermacs--maybe-cancel-active-timer)
    (aidermacs--cleanup-temp-buffers)))

(provide 'aidermacs-backend-vterm)
;;; aidermacs-backend-vterm.el ends here
