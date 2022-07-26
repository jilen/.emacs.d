;;; init-treesitter.el --- Treesitter setup.

;;; Commentary:
;;

;;; Code:

(use-package tree-sitter)
(use-package tree-sitter-langs
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package ts-fold
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/ts-fold")

(provide 'init-treesitter)

;;; init-treesitter.el ends here
