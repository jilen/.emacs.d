;;; init-treesitter.el --- Treesitter setup.

;;; Commentary:
;;

;;; Code:

(use-package tree-sitter)
(use-package tree-sitter-langs)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(provide 'init-treesitter)

;;; init-treesitter.el ends here
