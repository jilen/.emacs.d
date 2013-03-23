(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/modules/yasnippet/dict")
(ac-config-default)
(define-globalized-minor-mode global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
			   (auto-complete-mode 1))
                       ))
(global-auto-complete-mode t)
