;;; modules --- all modules, possible hosted on github, using git submodule feature

;;; Commentary:
;;; all modules are loaded here

;;; Code:

(defun module-dir (module)
  (concat "~/.emacs.d/modules/" module))

(defun module-init-file (module)
  (concat "~/.emacs.d/init/"
	  (concat module ".el")))

(defun add-module-path (module)
  (cond
   ((file-exists-p (module-dir (concat module "/elisp")))
    (add-to-list 'load-path (module-dir(concat module "/elisp"))))
   ((file-exists-p (module-dir module))
     (add-to-list 'load-path (module-dir module)))))


(defun init-module (module)
  (cond
   (
    (file-exists-p (module-init-file module))
    (load-file (module-init-file module)))))

;;;add module to load path, and load init file
(defun load-module (module) 
  (add-module-path module)
  (init-module module))

;;utility library
(load-module "popup")
(load-module "fuzzy")
(load-module "s")
(load-module "dash")

;;modes
(load-module "yasnippet")
(load-module "company-mode")
(load-module "php-mode")
(load-module "flycheck")
(load-module "haskell-mode")
(load-module "scala-mode2")
(load-module "undo-tree")
(load-module "mmm-mode")
(load-module "rainbow-mode")
(load-module "rainbow-delimiters")
(load-module "markdown-mode")
(load-module "emacs-eclim")
(load-module "jdibug")
(provide 'modules)
;;; modules.el ends here
