;;; vue-mode.el --- Emacs major mode for Vue -*- lexical-binding:t -*-
;;
;; Adapated from : https://github.com/leafOfTree/vue-mode
;; Package-Requires: ((emacs "29" with treesit))

;; This file is NOT part of GNU Emacs.
;; You can redistribute it and/or modify it under the terms of
;; the GNU Lesser General Public License v3.0.

;;; Commentary:

;; This major mode includes JavaScript/CSS and other language modes
;; as submode in html-mode. Mainly based on mhtml-mode.

;;; Advice:

;; pug-compute-indentation: Take care of prog-indentation-context
;; emmet-detect-style-tag-and-attr: Generic style tag begin as "<style"

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'sgml-mode)
(require 'js)
(require 'css-mode)
(require 'prog-mode)
(require 'subr-x)

(declare-function flyspell-generic-progmode-verify "ext:flyspell")
(declare-function emmet-check-if-between "ext:emmet-mode")
(declare-function pug-forward-through-whitespace "ext:pug-mode")
(declare-function vue--pug-compute-indentation-advice "vue-mode")

;;; Submode variables:
(defvar vue--css-submode)
(defvar vue--js-submode)
(defvar vue--pug-submode)
(defvar pug-indent-function)
(defvar pug-mode-syntax-table)
(defvar pug-mode-map)
(defvar vue--coffee-submode)
(defvar coffee-mode-syntax-table)
(defvar coffee-mode-map)
(defvar vue--sass-submode)
(defvar sass-mode-syntax-table)
(defvar sass-mode-map)
(defvar vue--typescript-submode)
(defvar emmet-use-style-tag-and-attr-detection)

(defvar vue-block-re "\\({[#:/]\\).*\\(}\\)$" "Block regexp.")
(defvar vue-expression-re "\\({\\)[^}]+\\(}\\)" "Expression regexp.")
(defvar vue--directive-prefix
  '("on" "bind" "use" "in" "out" "transition" "animate" "class")
  "Directive prefixes.")
(defvar vue-directive-prefix-re
  (concat (regexp-opt vue--directive-prefix) ":[^=/> ]+")
  "Directive prefixes regexp.")
(defvar vue--block-keywords '("if" "else" "each" "await" "then" "catch" "as")
  "Block keywords.")
(defvar vue--font-lock-html-keywords
  `((,vue-block-re
     (1 font-lock-type-face)
     (2 font-lock-type-face)
     (,(regexp-opt vue--block-keywords 'words)
      (goto-char (match-end 1)) nil (0 font-lock-keyword-face)))
    (,vue-expression-re
     (1 font-lock-type-face)
     (2 font-lock-type-face))
    (,vue-directive-prefix-re
     (0 font-lock-type-face)))
  "Font lock keywords in the html section.")
(defvar font-lock-beg)
(defvar font-lock-end)
(defvar vue--syntax-propertize
  (syntax-propertize-rules
   ("<template.*pug.*>"
    (0 (ignore
        (goto-char (match-end 0))
        ;; Don't apply in a comment.
        (unless (syntax-ppss-context (syntax-ppss))
    (unless (boundp 'vue--pug-submode)
      (vue--load-pug-submode))
    (when (boundp 'vue--pug-submode)
      (vue--syntax-propertize-submode vue--pug-submode end))))))
   ("<script.*coffee.*>"
    (0 (ignore
  (goto-char (match-end 0))
  ;; Don't apply in a comment.
  (unless (syntax-ppss-context (syntax-ppss))
    (unless (boundp 'vue--coffee-submode)
      (vue--load-coffee-submode))
    (when (boundp 'vue--coffee-submode)
      (vue--syntax-propertize-submode vue--coffee-submode end))))))
   ("<style.*sass.*>"
    (0 (ignore
        (goto-char (match-end 0))
        ;; Don't apply in a comment.
        (unless (syntax-ppss-context (syntax-ppss))
          (unless (boundp 'vue--sass-submode)
            (vue--load-sass-submode))
          (when (boundp 'vue--sass-submode)
            (vue--syntax-propertize-submode vue--sass-submode end))))))
   ("<script.*ts.*>"
    (0 (ignore
        (goto-char (match-end 0))
        ;; Don't apply in a comment.
        (unless (syntax-ppss-context (syntax-ppss))
          (unless (boundp 'vue--typescript-submode)
            (vue--load-typescript-submode))
          (when (boundp 'vue--typescript-submode)
            (vue--syntax-propertize-submode vue--typescript-submode end))))))
   ("<style.*?>"
    (0 (ignore
        (goto-char (match-end 0))
        ;; Don't apply in a comment.
        (unless (syntax-ppss-context (syntax-ppss))
          (vue--syntax-propertize-submode vue--css-submode end)))))
   ("<script.*?>"
    (0 (ignore
        (goto-char (match-end 0))
        ;; Don't apply in a comment.
        (unless (syntax-ppss-context (syntax-ppss))
          (vue--syntax-propertize-submode vue--js-submode end)))))
   sgml-syntax-propertize-rules)
  "Vue syntax propertize rules.")

(defcustom vue-basic-offset sgml-basic-offset
  "Specifies the basic indentation level for .vue."
  :type 'integer
  :set (lambda (symbol value)
         (customize-set-variable 'sgml-basic-offset value)
         (customize-set-variable 'css-indent-offset value)
         (customize-set-variable 'js-indent-level value)
         (set-default symbol value))
  :group 'sgml)

(defcustom vue-tag-relative-indent t
  "How <script> and <style> bodies are indented relative to the tag.

When t, indentation looks like:

  <script>
    code();
  </script>

When nil, indentation of the script body starts just below the
tag, like:

  <script>
  code();
  </script>

When `ignore', the script body starts in the first column, like:

  <script>
code();
  </script>"
  :group 'sgml
  :type '(choice (const nil) (const t) (const ignore))
  :safe 'symbolp
  :version "26.1")

(defcustom vue-display-submode-name nil
  "Whether to display submode name in the status line."
  :group 'sgml
  :type '(choice (const nil) (const t))
  :safe 'symbolp
  :version "26.1")

(cl-defstruct vue--submode
  name      ; Name of this submode.
  end-tag     ; HTML end tag.
  syntax-table          ; Syntax table.
  propertize            ; Propertize function.
  indent-function       ; Indent function that overrides the submode one.
  keymap                ; Keymap.
  ;; Captured locals that are set when entering a region.
  crucial-captured-locals
  ;; Other captured local variables; these are not set when entering a
  ;; region but let-bound during certain operations, e.g.,
  ;; indentation.
  captured-locals
  (excluded-locals
   () :documentation "Local variables that are not to be captured."))

(defconst vue--crucial-variable-prefix
  (regexp-opt '("comment-"
                "uncomment-"
                "electric-indent-"
                "smie-"
                "forward-sexp-function"
                "completion-"
                "major-mode"
                ))
  "Regexp matching the prefix of \"crucial\" buffer-locals we want to capture.")

(defconst vue--variable-prefix
  (regexp-opt '("font-lock-"
                "indent-line-function"
                "typescript--"
                "haml-"
                ))
  "Regexp matching the prefix of buffer-locals we want to capture.")

(defun vue--construct-submode (mode &rest args)
  "Computes the buffer-local variables in submode MODE with ARGS passed to it."
  (let ((captured-locals nil)
        (crucial-captured-locals nil)
        (submode (apply #'make-vue--submode args)))
    (with-temp-buffer
      (funcall mode)
      ;; Make sure font lock is all set up.
      (font-lock-set-defaults)
      ;; This has to be set to a value other than the vue-mode
      ;; value, to avoid recursion.
      (unless (variable-binding-locus 'font-lock-fontify-region-function)
        (setq-local font-lock-fontify-region-function
                    #'font-lock-default-fontify-region))
      (dolist (iter (buffer-local-variables))
        (let ((variable-name (symbol-name (car iter))))
          (when (string-match vue--crucial-variable-prefix variable-name)
            (push iter crucial-captured-locals))
          (when (string-match vue--variable-prefix variable-name)
            (unless (member (car iter)
                            (vue--submode-excluded-locals submode))
              (push iter captured-locals)))))
      (setf (vue--submode-crucial-captured-locals submode)
            crucial-captured-locals)
      (setf (vue--submode-captured-locals submode)
            captured-locals))
    submode))

(defun vue--mark-buffer-locals (submode)
  "Make buffer local variables from SUBMODE."
  (dolist (iter (vue--submode-captured-locals submode))
    (make-local-variable (car iter))))

(defvar-local vue--crucial-variables nil
  "List of all crucial variable symbols.")

(defun vue--mark-crucial-buffer-locals (submode)
  "Make crucial buffer local variables from SUBMODE."
  (dolist (iter (vue--submode-crucial-captured-locals submode))
    (make-local-variable (car iter))
    (push (car iter) vue--crucial-variables)))

(defconst vue--css-submode
  (vue--construct-submode 'css-mode
                            :name "CSS"
                            :end-tag "</style>"
                            :syntax-table css-mode-syntax-table
                            :propertize css-syntax-propertize-function
                            :keymap css-mode-map))

(defconst vue--js-submode
  (vue--construct-submode 'js-mode
                            :name "JavaScript"
                            :end-tag "</script>"
                            :syntax-table js-mode-syntax-table
                            :propertize #'js-syntax-propertize
                            :keymap js-mode-map))

;;; Pug mode
(defun vue--load-pug-submode ()
  "Load `pug-mode' and patch it."
  (when (require 'pug-mode nil t)
    (customize-set-variable 'pug-tab-width vue-basic-offset)
    (defconst vue--pug-submode
      (vue--construct-submode 'pug-mode
         :name "Pug"
         :end-tag "</template>"
         :syntax-table pug-mode-syntax-table
         :excluded-locals '(font-lock-extend-region-functions)
         :keymap pug-mode-map))

    (defun vue--pug-compute-indentation-advice (orig-fun &rest args)
      "Calculate the maximum sensible indentation for the current line.

Ignore ORIG-FUN and ARGS."
      (ignore orig-fun args)
      (save-excursion
  (beginning-of-line)
  (if (bobp) 0
    (pug-forward-through-whitespace t)
    (+ (current-indentation)
       (or (funcall pug-indent-function)
     ;; Take care of prog-indentation-context
     (car prog-indentation-context)
     0)))))

    (advice-add 'pug-compute-indentation
    :around
    #'vue--pug-compute-indentation-advice)

    (vue--mark-buffer-locals vue--pug-submode)
    (vue--mark-crucial-buffer-locals vue--pug-submode)
    (setq vue--crucial-variables (delete-dups vue--crucial-variables))))

;;; Coffee mode
(defun vue--load-coffee-submode ()
  "Load `coffee-mode' and patch it."
  (when (require 'coffee-mode nil t)
    (customize-set-variable 'coffee-tab-width vue-basic-offset)
    (defconst vue--coffee-submode
      (vue--construct-submode 'coffee-mode
         :name "Coffee"
         :end-tag "</script>"
         :syntax-table coffee-mode-syntax-table
         :keymap coffee-mode-map))))

;;; Sass mode
(defun vue--load-sass-submode ()
  "Load `sass-mode' and patch it."
  (when (require 'sass-mode nil t)
    (customize-set-variable 'sass-tab-width vue-basic-offset)
    (defconst vue--sass-submode
      (vue--construct-submode 'sass-mode
         :name "Sass"
         :end-tag "</style>"
         :syntax-table sass-mode-syntax-table
         :excluded-locals '(font-lock-extend-region-functions)
         :keymap sass-mode-map))))

;;; TypeScript mode
(defun vue--load-typescript-submode ()
  "Load `typescript-mode' and patch it."
  (when (require 'typescript-ts-mode nil t)
    (customize-set-variable 'typescript-ts-mode-indent-offset vue-basic-offset)
    (defconst vue--typescript-submode
      (vue--construct-submode 'typescript-ts-mode
                                 :name "TypeScript"
                                 :end-tag "</script>"
                                 :syntax-table typescript-ts-mode--syntax-table
                                 :indent-function #'treesit-indent
                                 :keymap typescript-ts-mode-map))))

(defmacro vue--with-locals (submode &rest body)
  "Bind SUBMODE local variables and then run BODY."
  (declare (indent 1))
  `(cl-progv
       (when ,submode
         (mapcar #'car (vue--submode-captured-locals ,submode)))
       (when ,submode
         (mapcar #'cdr (vue--submode-captured-locals ,submode)))
     (cl-progv
   (when ,submode
           (mapcar #'car (vue--submode-crucial-captured-locals ,submode)))
   (when ,submode
           (mapcar #'cdr (vue--submode-crucial-captured-locals ,submode)))
       ,@body)))

(defun vue--submode-lighter ()
  "Mode-line lighter indicating the current submode."
  ;; The end of the buffer has no text properties, so in this case
  ;; back up one character, if possible.
  (let* ((where (if (and (eobp) (not (bobp)))
                    (1- (point))
                  (point)))
         (submode (get-text-property where 'vue-submode)))
    (if submode
        (vue--submode-name submode)
      nil)))

(defun vue--get-mode-name ()
  "Get mode name for the status line"
  (if vue-display-submode-name
      (concat "Vue/" (or (vue--submode-lighter) "HTML"))
    "Vue"))

(defvar-local vue--last-submode nil
  "Record the last visited submode.
This is used by `vue--pre-command'.")

(defvar-local vue--stashed-crucial-variables nil
  "Alist of stashed values of the crucial variables.")

(defun vue--stash-crucial-variables ()
  "Stash crucial variables of current buffer."
  (setq vue--stashed-crucial-variables
        (mapcar (lambda (sym)
                  (cons sym (buffer-local-value sym (current-buffer))))
                vue--crucial-variables)))

(defun vue--map-in-crucial-variables (alist)
  "Set all crucial variables in ALIST."
  (dolist (item alist)
    (set (car item) (cdr item))))

(defun vue--pre-command ()
  "Run pre- and post-hook for each command if current submode is changed."
  (let ((submode (get-text-property (point) 'vue-submode)))
    (unless (eq submode vue--last-submode)
      ;; If we're entering a submode, and the previous submode was
      ;; nil, then stash the current values first.  This lets the user
      ;; at least modify some values directly.  FIXME maybe always
      ;; stash into the current mode?
      (when (and submode (not vue--last-submode))
        (vue--stash-crucial-variables))
      (vue--map-in-crucial-variables
       (if submode
           (vue--submode-crucial-captured-locals submode)
         vue--stashed-crucial-variables))
      (setq vue--last-submode submode))))

;;; Syntax propertize
(defun vue--syntax-propertize-submode (submode end)
  "Set text properties from point to END or `end-tag' before END in SUBMODE."
  (save-excursion
    (when (search-forward (vue--submode-end-tag submode) end t)
      (setq end (match-beginning 0))))
  (set-text-properties (point)
                       end
                       (list 'vue-submode submode
                             'syntax-table (vue--submode-syntax-table submode)
                             ;; We want local-map here so that we act
                             ;; more like the sub-mode and don't
                             ;; override minor mode maps.
                             'local-map (vue--submode-keymap submode)))
  (when (vue--submode-propertize submode)
    (funcall (vue--submode-propertize submode) (point) end))
  (goto-char end))


(defun vue-syntax-propertize (start end)
  "Vue syntax propertize function for text between START and END."

  ;; First remove our special settings from the affected text.  They
  ;; will be re-applied as needed.
  (remove-list-of-text-properties start
                                  end
                                  '(syntax-table local-map vue-submode))
  (goto-char start)
  (when (= emacs-major-version 26)
    ;; Be sure to look back one character, because START won't yet have
    ;; been propertized.
    (unless (bobp)
      (let ((submode (get-text-property (1- (point)) 'vue-submode)))
  (if submode
      (vue--syntax-propertize-submode submode end)
    (sgml-syntax-propertize (point) end))))
    (funcall vue--syntax-propertize (point) end))
  (when (> emacs-major-version 26)
    (unless (bobp)
      (let ((submode (get-text-property (1- (point)) 'vue-submode)))
  (when submode
    (vue--syntax-propertize-submode submode end))))
    (apply #'sgml-syntax-propertize (list (point) end vue--syntax-propertize))))

;;; Indentation
(defun vue-indent-line ()
  "Indent the current line as HTML, JS, or CSS, according to its context."
  (interactive)
  (let ((submode (save-excursion
                   (back-to-indentation)
                   (get-text-property (point) 'vue-submode))))
    (if submode
        (save-restriction
          (let* ((region-start
                  (or (previous-single-property-change (point) 'vue-submode)
                      (point)))
                 (base-indent (save-excursion
                                (goto-char region-start)
                                (sgml-calculate-indent))))
            (cond
             ((not vue-tag-relative-indent)
              (setq base-indent (- base-indent vue-basic-offset)))
             ((eq vue-tag-relative-indent 'ignore)
              (setq base-indent 0)))
            (narrow-to-region region-start (point-max))
            (let ((prog-indentation-context (list base-indent)))
              (vue--with-locals submode
                ;; indent-line-function was rebound by
                ;; vue--with-locals.
                (funcall (or (vue--submode-indent-function submode)
                             indent-line-function))))))
      ;; HTML.
      (vue-html-indent-line))))

(defun vue-html-indent-line ()
  "Indent HTML within Vue."
  (interactive)
  (let* ((savep (point))
   (block-offset (vue--html-block-offset))
   (indent-col
    (save-excursion
      (back-to-indentation)
      (when (>= (point) savep) (setq savep nil))
      (sgml-calculate-indent))))
    (when block-offset
      (save-excursion
  (forward-line -1)
  (setq indent-col (+ block-offset (current-indentation)))))
    (if (or (null indent-col) (< indent-col 0))
  'noindent
      (if savep
    (save-excursion (indent-line-to indent-col))
  (indent-line-to indent-col)))))

(defun vue--html-block-offset ()
  "Indentation offset of Vue blocks like {#if...}, {#each...}."
  (cond ((or (vue--previous-block "beginning")
       (vue--previous-block "middle"))
         vue-basic-offset)
        ((or (vue--current-block "middle")
             (vue--current-block "end")
             (and (vue--previous-block "end")
                  (vue--current-tag "end")))
         (- 0 vue-basic-offset))
        ((or (vue--previous-block "end")
             (and (vue--previous-block "end")
                  (vue--current-tag "start")))
         0)))

(defun vue--current-tag (type)
  "Search current line to find tag of TYPE(beginning or end)."
  (interactive)
  (let ((bound (save-excursion
     (beginning-of-line)
     (point)))
  (tag-re (cond ((equal type "beginning")
           ;; "<\\w+")
           "^[\t ]*<\\w+")
          ((equal type "end")
           "^[\t ]*</\\w+"))))
    (when tag-re
      (save-excursion
  (end-of-line)
  (re-search-backward tag-re bound t)))))

(defun vue--previous-block (type)
  "Search previous line to find block of TYPE(beginning, middle or end)."
  (let ((bound (vue--beginning-of-previous-line))
  (block-re (cond ((equal type "beginning")
       (vue--beginning-of-block-re))
      ((equal type "middle")
       (vue--middle-of-block-re))
      ((equal type "end")
       (vue--end-of-block-re)))))
    (when block-re
      (save-excursion
  (beginning-of-line)
  (re-search-backward block-re bound t)))))

(defun vue--current-block (type)
  "Search current line to find block of TYPE(beginning, middle or end)."
  (let ((bound (save-excursion
     (beginning-of-line)
     (point)))
  (block-re (cond ((equal type "beginning")
       (vue--beginning-of-block-re))
      ((equal type "middle")
       (vue--middle-of-block-re))
      ((equal type "end")
       (vue--end-of-block-re)))))
    (when block-re
      (save-excursion
  (end-of-line)
  (re-search-backward block-re bound t)))))

(defun vue--beginning-of-block-re ()
  "Regexp of beginning of block."
  (concat
   "{#\\("
   (string-join vue--block-keywords "\\|")
   "\\)"))

(defun vue--middle-of-block-re ()
  "Regexp of middle of block."
  (concat
   "{:\\("
   (string-join vue--block-keywords "\\|")
   "\\)"))

(defun vue--end-of-block-re ()
  "Regexp of end of block."
  (concat
   "{/\\("
   (string-join vue--block-keywords "\\|")
   "\\)"))

(defun vue--beginning-of-previous-line ()
  "Beginning of previous non-blank line."
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward "\n[:space:]")
    (beginning-of-line)
    (point)))

;;; Font lock
(defun vue--extend-font-lock-region ()
  "Extend the font lock region according to HTML sub-mode needs.

This is used via `font-lock-extend-region-functions'.  It ensures
that the font-lock region is extended to cover either whole
lines, or to the spot where the submode changes, whichever is
smallest."
  (let ((orig-beg font-lock-beg)
        (orig-end font-lock-end))
    ;; The logic here may look odd but it is needed to ensure that we
    ;; do the right thing when trying to limit the search.
    (save-excursion
      (goto-char font-lock-beg)
      ;; previous-single-property-change starts by looking at the
      ;; previous character, but we're trying to extend a region to
      ;; include just characters with the same submode as this
      ;; character.
      (unless (eobp) (forward-char))
      (setq font-lock-beg (previous-single-property-change
                           (point)
                           'vue-submode
                           nil
                           (line-beginning-position)))
      (unless (eq (get-text-property font-lock-beg 'vue-submode)
                  (get-text-property orig-beg 'vue-submode))
        (cl-incf font-lock-beg))

      (goto-char font-lock-end)
      (unless (bobp)
        (backward-char))
      (setq font-lock-end (next-single-property-change
                           (point)
                           'vue-submode
                           nil
                           (line-beginning-position 2)))
      (unless (eq (get-text-property font-lock-end 'vue-submode)
                  (get-text-property orig-end 'vue-submode))
        (cl-decf font-lock-end)))

    ;; Also handle the multiline property -- but handle it here, and
    ;; not via font-lock-extend-region-functions, to avoid the
    ;; situation where the two extension functions disagree.
    ;; See bug#29159.
    (font-lock-extend-region-multiline)

    (or (/= font-lock-beg orig-beg)
        (/= font-lock-end orig-end))))

(defun vue--submode-fontify-one-region (submode beg end &optional loudly)
  "Fontify the text between BEG and END with SUBMODE locals bound.

If LOUDLY is non-nil, print status messages while fontifying."
  (if submode
      (vue--with-locals submode
        (save-restriction
          (font-lock-fontify-region beg end loudly)))
    (font-lock-set-defaults)
    (font-lock-default-fontify-region beg end loudly)))

(defun vue--submode-fontify-region (beg end loudly)
  "Fontify the text between BEG and END.

If LOUDLY is non-nil, print status message while fontifying."
  (syntax-propertize end)
  (let ((orig-beg beg)
        (orig-end end)
        (new-beg beg)
        (new-end end))
    (while (< beg end)
      (let ((submode (get-text-property beg 'vue-submode))
            (this-end (next-single-property-change beg 'vue-submode nil end)))
        (let ((extended (vue--submode-fontify-one-region
                         submode beg this-end loudly)))
          ;; If the call extended the region, take note.  We track the
          ;; bounds we were passed and take the union of any extended
          ;; bounds.
          (when (and (consp extended)
                     (eq (car extended) 'jit-lock-bounds))
            (setq new-beg (min new-beg (cadr extended)))
            ;; Make sure that the next region starts where the
            ;; extension of this region ends.
            (setq this-end (cddr extended))
            (setq new-end (max new-end this-end))))
        (setq beg this-end)))
    (when (or (/= orig-beg new-beg)
              (/= orig-end new-end))
      (cons 'jit-lock-bounds (cons new-beg new-end)))))

;;; Emmet mode
(defun vue--emmet-detect-style-tag-and-attr-advice (orig-fun &rest args)
  "Detect style tag begin as `<style'.

Ignore ORIG-FUN and ARGS."
  (ignore orig-fun args)
  (let* ((style-attr-end "[^=][\"']")
   (style-attr-begin "style=[\"']")
   (style-tag-end "</style>")
   ;; Generic style tag begin
   (style-tag-begin "<style"))
    (and emmet-use-style-tag-and-attr-detection
   (or
    (emmet-check-if-between style-attr-begin style-attr-end)
    (emmet-check-if-between style-tag-begin style-tag-end)))))


;;; Flyspell
(defun vue--flyspell-check-word ()
  "Flyspell check word."
  (let ((submode (get-text-property (point) 'vue-submode)))
    (if submode
        (flyspell-generic-progmode-verify)
      t)))

(defun vue-unload-function ()
  "Unload advices from vue.

Called by `unload-feature'."
  (advice-remove 'emmet-detect-style-tag-and-attr
     #'vue--emmet-detect-style-tag-and-attr-advice)

  (advice-remove 'pug-compute-indentation
     #'vue--pug-compute-indentation-advice))

;;;###autoload
(define-derived-mode vue-mode html-mode
  '((:eval (vue--get-mode-name)))
  "Major mode based on `html-mode', but works with embedded JS and CSS.

Code inside a <script> element is indented using the rules from
`js-mode'; and code inside a <style> element is indented using
the rules from `css-mode'."
  (setq-local sgml-quick-keys 'close)
  (sgml-electric-tag-pair-mode)
  (setq-local indent-line-function #'vue-indent-line)
  (setq-local syntax-propertize-function #'vue-syntax-propertize)
  (setq-local font-lock-fontify-region-function
              #'vue--submode-fontify-region)
  (setq-local font-lock-extend-region-functions
              '(vue--extend-font-lock-region))

  ;; Attach this to both pre- and post- hooks just in case it ever
  ;; changes a key binding that might be accessed from the menu bar.
  (add-hook 'pre-command-hook #'vue--pre-command nil t)
  (add-hook 'post-command-hook #'vue--pre-command nil t)

  (font-lock-add-keywords 'vue-mode vue--font-lock-html-keywords)

  ;; Make any captured variables buffer-local.
  (vue--mark-buffer-locals vue--css-submode)
  (vue--mark-buffer-locals vue--js-submode)

  (vue--mark-crucial-buffer-locals vue--css-submode)
  (vue--mark-crucial-buffer-locals vue--js-submode)
  (setq vue--crucial-variables (delete-dups vue--crucial-variables))

  ;; Hack
  (js--update-quick-match-re)

  ;; Integrate other packages
  (advice-add
   'emmet-detect-style-tag-and-attr
   :around
   #'vue--emmet-detect-style-tag-and-attr-advice)


  ;; This is sort of a prog-mode as well as a text mode.
  (run-hooks 'prog-mode-hook))

(put 'vue-mode 'flyspell-mode-predicate #'vue--flyspell-check-word)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(provide 'vue-mode)

;;; vue-mode.el ends here
