;;; init-node-path.el --- Add node_modules executable path

;;; Commentary:
;;

;;; Code:

(use-package add-node-modules-path
  :custom
  (add-node-modules-path-command "echo \"$(npm root)/.bin\""))

(provide 'init-node-path)

;;; init-node-path.el ends here
