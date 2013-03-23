;;rainbow mode
(require 'rainbow-mode)
(add-hook 'css-mode-hook
	  (lambda()
	    (rainbow-mode t)))
(add-hook 'html-mode-hook
	  (lambda()
	    (rainbow-mode t)))
