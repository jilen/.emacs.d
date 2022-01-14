;;; init-package.el --- Base package setup

;;; Commentary:
;; setup eamcs to use `use-package`

(require 'package)
(setq package-user-dir (concat dotfiles-dir "elpa"))
(setq package-archives '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

(provide 'init-package)

;;; init-package.el ends here
