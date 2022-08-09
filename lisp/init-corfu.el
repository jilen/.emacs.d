;;; init-corfu.el --- corfu completion intergation

;;; Commentary:
;;

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-auto-prefix 2)          ;; Trigger auto completion with 2 chars
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil) ;; Disable current candidate preview
  (corfu-preselect-first t)      ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin

  :init
  (global-corfu-mode))


(use-package svg-lib
  :init
  (setq svg-lib-icon-collections
        '(("bootstrap" .
           "https://icons.getbootstrap.com/icons/%s.svg")
          ("simple" .
           "https://mirror.ghproxy.com/https://raw.githubusercontent.com/simple-icons/simple-icons/develop/icons/%s.svg")
          ("material" .
           "https://mirror.ghproxy.com/https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg")
          ("octicons" .
           "https://mirror.ghproxy.com/https://raw.githubusercontent.com/primer/octicons/master/icons/%s-24.svg")
          ("boxicons" .
           "https://boxicons.com/static/img/svg/regular/bx-%s.svg"))
        ))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :init
  (setq kind-icon-default-style '(:height 0.66 :stroke 0 :padding 0 :margin -0.6))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Add extensions
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))


(use-package corfu-doc
  :init
  (setq corfu-doc-display-within-parent-frame nil)
  :hook (corfu-mode . corfu-doc-mode))

(provide 'init-corfu)

;;; init-corfu.el ends here
