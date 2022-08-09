;;; kind-icon-company.el --- Kind-icon for company-mode

;;; Commentary:
;;

;;; Code:

(require 'company)

(defcustom company-svg-icon-mapping
  '((array . (:icon "code-brackets" :face font-lock-type-face))
    (boolean . (:icon "circle-half-full" :face font-lock-builtin-face))
    (class . (:icon "view-grid-plus-outline" :face font-lock-type-face))
    (color . (:icon "palette" :face success))
    (constant . (:icon "lock-remove-outline" :face font-lock-constant-face))
    (constructor . (:icon "table-column-plus-after" :face font-lock-function-name-face))
    (enum  . (:icon "format-list-checks" :face font-lock-builtin-face))
    (enum . (:icon "format-list-bulleted-square" :face font-lock-builtin-face))
    (event . (:icon "lightning-bolt-outline" :face font-lock-warning-face))
    (field . (:icon "application-braces-outline" :face font-lock-variable-name-face))
    (file . (:icon "file-document-outline" :face font-lock-string-face))
    (folder . (:icon "folder" :face font-lock-doc-face))
    (interface . (:icon "application-brackets-outline" :face font-lock-type-face))
    (keyword . (:icon "key-variant" :face font-lock-keyword-face))
    (macro . (:icon "lambda" :face font-lock-keyword-face))
    (method . (:icon "function-variant" :face font-lock-function-name-face))
    (function . (:icon "function" :face font-lock-function-name-face))
    (module . (:icon "file-code-outline" :face font-lock-preprocessor-face))
    (numeric . (:icon "numeric" :face font-lock-builtin-face))
    (operator . (:icon "plus-minus" :face font-lock-comment-delimiter-face))
    (param . (:icon "cog" :face default))
    (property . (:icon "application-parentheses-outline" :face font-lock-variable-name-face))
    (reference . (:icon "variable-box" :face font-lock-variable-name-face))
    (snippet . (:icon "note-text-outline" :face font-lock-string-face))
    (string . (:icon "sticker-text-outline" :face font-lock-string-face))
    (struct . (:icon "code-braces" :face font-lock-variable-name-face))
    (text . (:icon "script-text-outline" :face shadow))
    (type  . (:icon "format-list-bulleted-type" :face font-lock-type-face))
    (unit . (:icon "ruler-square" :face shadow))
    (value . (:icon "plus-circle-outline" :face font-lock-builtin-face))
    (variable . (:icon "variable" :face font-lock-variable-name-face))
    (t . (:icon "crosshairs-question" :face shadow)))
  "Mapping of kinds.")

(defcustom icon-url-pattern
  "https://ghproxy.com/https://raw.githubusercontent.com/Templarian/MaterialDesign/master/svg/%s.svg"
  "Url pattern to fetch svg icon."
  )

(defvar company-svg-icon-cache (make-hash-table :test 'equal))
(defvar company-svg-icon-dir (expand-file-name "icons" (file-name-directory load-file-name)))
(defvar company-svg-icon-width 3)



(defun company-svg-icon-filepath (collection name)
  (concat (file-name-as-directory company-svg-icon-dir) (format "%s_%s.svg" collection name)))

(defun company-svg-icon-fetch-all ()
  (interactive)
  (dolist (icon company-svg-icon-mapping)
    (let* ((collection "material")
           (name (plist-get (cdr icon) :icon))
           (url (format icon-url-pattern name))
           (filename (company-svg-icon-filepath collection name)))
      (with-temp-buffer
        (url-insert-file-contents url)
        (write-region (point-min) (point-max) filename)))))

(defun company-svg-icon-parse (collection name)
  (with-temp-buffer
    (insert-file-contents (company-svg-icon-filepath collection name))
    (xml-parse-region (point-min) (point-max))))

(defun company-svg-icon-convert-to-svg-color (color-name)
  "Convert Emacs COLOR-NAME to #rrggbb form.
If COLOR-NAME is unknown to Emacs, then return COLOR-NAME as-is."
  (let ((rgb-color (color-name-to-rgb color-name)))
    (if rgb-color
        (apply #'color-rgb-to-hex (append rgb-color '(2)))
      color-name)))

(defun company-svg-icon (collection name &optional fg-color bg-color zoom)
  (let* ((root (company-svg-icon-parse collection name))

         ;; Read original viewbox
         (viewbox (cdr (assq 'viewBox (xml-node-attributes (car root)))))
         (viewbox (mapcar 'string-to-number (split-string viewbox)))
         (view-x (nth 0 viewbox))
         (view-y (nth 1 viewbox))
         (view-width (nth 2 viewbox))
         (view-height (nth 3 viewbox))

         ;; Set icon size (in pixels) to 4x1 characters
         (svg-width  (* (window-font-width)  company-svg-icon-width))
         (svg-height (* (window-font-height) 1))

         ;; Zoom the icon by using integer factor only
         (zoom (max 1 (truncate (or zoom 1))))
         (svg-width  (* svg-width zoom))
         (svg-height (* svg-height zoom))

         (svg-viewbox (format "%f %f %f %f" view-x view-y view-width view-height))
         (fg-color (company-svg-icon-convert-to-svg-color
                    (or (when (facep fg-color)
                          (face-foreground fg-color nil t))
                        fg-color (face-attribute 'default :foreground))))
         (bg-color (company-svg-icon-convert-to-svg-color
                    (or (when (facep bg-color)
                          (face-background bg-color nil t))
                        bg-color "transparent")))
         (svg (svg-create svg-width svg-height
                          :viewBox svg-viewbox
                          :stroke-width 0
                          :fill fg-color)))
    (svg-rectangle svg
                   view-x view-y view-width view-height
                   :fill bg-color)

    (dolist (item (xml-get-children (car root) 'path))
      (let* ((attrs (xml-node-attributes item))
             (path (cdr (assoc 'd attrs)))
             (fill (or (cdr (assoc 'fill attrs)) fg-color)))
        (svg-node svg 'path :d path :fill fill)))
    (svg-image svg :ascent 'center :scale 1)))


(defun company-svg-icon-build (collection name fg-color bg-color)
  (if (image-type-available-p 'svg)
      (let* ((icon-key (format "%s_%s" collection name))
             (icon-text (gethash icon-key company-svg-icon-cache)))
        (unless icon-text
          (setq icon-text (propertize
                           (apply #'concat (make-list company-svg-icon-width "-"))
                           'display (company-svg-icon collection name fg-color bg-color)))
          (puthash icon-key icon-text company-svg-icon-cache))
        icon-text)
    ""))


(defsubst company-svg-icon--rgb-blend (rgb1 rgb2 frac)
  "Return a fractional blend between two colors RGB1 and RGB2.
Each is a 3 element list.  The fractional blend point is the
float FRAC."
  (apply #'color-rgb-to-hex
         (cl-mapcar (lambda (a b)
                      (+ (* a frac) (* b (- 1.0 frac))))
                    rgb1 rgb2)))

(defsubst get-fg-or-default (face)
  "Get foreground color of FACE or default face if no fg."
  (let* ((face-fg (face-foreground face))
         (ensure-fg (if face-fg face-fg (face-foreground 'default))))
    ensure-fg)
  )

;;;###autoload
(defun company-svg-icon-format-margin-function (candidate selected)
  "Company kind icon (caculated from CANDIDATE, SELECTED) margin function."
  (let*
      ((kind (company-call-backend 'kind candidate))
       (icon-info-from-kind (alist-get kind company-svg-icon-mapping))
       (icon-info (if icon-info-from-kind icon-info-from-kind (alist-get 't company-svg-icon-mapping)))
       (icon-name (plist-get  icon-info :icon))
       (icon-face (plist-get icon-info :face))
       (icon-fg (get-fg-or-default icon-face))
       (default-bg (face-background 'company-tooltip))
       (icon-bg (company-svg-icon--rgb-blend (color-name-to-rgb icon-fg) (color-name-to-rgb default-bg) 0.12)))
    (message (format "show icon with face: %s, fg: %s bg: %s" icon-face icon-bg icon-fg))
    (company-svg-icon-build "material" icon-name icon-fg icon-bg)))

(provide 'company-svg-icon)

;;; company-svg-icon.el ends here
