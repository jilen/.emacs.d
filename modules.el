;;; modules --- all modules, possible hosted on github, using git submodule feature

;;; Commentary:
;;; all modules are loaded here

;;; Code:

(defun module-dir (module)
  (concat "~/.emacs.d/modules/" module))

(defun module-init-file (module)
  (concat "~/.emacs.d/init/"
	  (concat module ".el")))

;;;add module to load path, and load init file
(defun load-module (module) 
  (add-to-list 'load-path (module-dir module))
  (load-file (module-init-file module)))

(load-module "yasnippet")
(load-module "auto-complete")
(load-module "php-mode")
(load-module "s")
(load-module "dash")
(load-module "flycheck")
(load-module "haskell-mode")
(provide 'modules)
;;; modules.el ends here
