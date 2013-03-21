;;;all modules are loaded here

;;get module dir
(defun module-dir (module)
  (concat "~/.emacs.d/modules/" module))

;;get module init file name
(defun module-init-file (module)
  (concat "~/.emacs.d/init/"
	  (concat module ".el")))

(defun load-module (module) 
  (add-to-list 'load-path (module-dir module))
  (load-file (module-init-file module)))

(load-module "yasnippet")

