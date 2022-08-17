;;; init-treesitter.el --- Treesitter setup.

;;; Commentary:
;;

;;; Code:

(use-package tree-sitter)
(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))


(provide 'init-treesitter)

;;; init-treesitter.el ends here
