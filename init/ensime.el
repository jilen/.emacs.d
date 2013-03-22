;;ensime -- scala emacs mode
(require 'ensime)

;;hook to start ensime
(add-hook 'scala-mode 'ensime-mode-hook)
(add-hook 'scala-mode2 'ensime-scala-mode-hook)
