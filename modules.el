;;; modules --- all modules, possible hosted on github, using git submodule feature

;;; Commentary:
;;; all modules are loaded here

;;; Code:

(defun module-dir (module)
  (concat "~/.emacs.d/modules/" module))

(defun module-init-file (module)
  (concat "~/.emacs.d/init/"
	  (concat module ".el")))

(defun add-module-path (elisp)
  (add-to-list 'load-path (module-dir elisp)))
(defun init-module (module)
  (load-file (module-init-file module)))

;;;add module to load path, and load init file
(defun load-module (module) 
  (add-module-path module)
  (init-module module))


(load-module "yasnippet")
(load-module "auto-complete")
(load-module "php-mode")
(load-module "s")
(load-module "dash")
(load-module "flycheck")
(load-module "haskell-mode")
(load-module "scala-mode2")

;;ensime's el files are located in a sub directory
(add-module-path "ensime/elisp")
(init-module "ensime")


(provide 'modules)
;;; modules.el ends here
