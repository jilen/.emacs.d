;; Wrapper that requires all the relevant components
(require 'elog)
(require 'tree-mode)
(require 'jdwp)
(require 'jdi)
(require 'jdibug-expr)
(require 'jdibug-ui)
(require 'jdibug-run)
(require 'jdibug-menu)

(defconst jdibug-major-version "0"
  "Major version of this release")

(defconst jdibug-minor-version "5"
  "Minor version of this release")

(defconst jdibug-version (concat jdibug-major-version "." jdibug-minor-version)
  "Version of this release")


(provide 'jdibug)
