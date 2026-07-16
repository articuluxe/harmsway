;;; sendai-theme.el --- A cool blue color theme -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Jim Porter
;; URL: https://sr.ht/~jimporter/sendai-theme/
;; Version: 0.1.1-git
;; Keywords: theme
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A cool blue theme for Emacs, aiming for medium levels of contrast to
;; maintain readability without straining your eyes with garish colors.

;;; Code:

(require 'color)

(eval-and-compile
  (when (< emacs-major-version 29)
    (require 'subr-x)))                 ; For `when-let*'

(deftheme sendai "A cool blue color theme.")

(defvar sendai--face-classes
  '((true-color ((class color) (min-colors 16777216))       . 0)
    (256-color  ((class color) (min-colors 256))            . 1)
    (tty-color  ((class color) (min-colors 256) (type tty)) . 2))
  "An alist of possible face classes.
Keys are descriptive names and map to conses of face classes and their
corresponding index into `sendai--color' vectors.")

(defgroup sendai-theme nil
  "Sendai-theme options."
  :group 'faces)

(defcustom sendai-inherit-tty-colors nil
  "Use basic TTY colors when in xterm-256color mode."
  :type 'boolean
  :group 'sendai-theme)

(defcustom sendai-default-class nil
  "The default face class to use when setting non-`defface' colors.
Some theme settings only support a single color value.  When using
`emacsclient' in multiple settings (e.g. GUI and TTY frames), this can
cause colors to look inconsistent.  With the default value of nil,
this uses the setting matching the theme was initially loaded.  With
any other value, force the theme to use the colors associated with
that face class.

For example, when using `emacs --daemon' in a GUI environment, you
likely want to set this to `true-color'."
  :type '(choice (const :tag "Default"    nil)
                 (const :tag "True color" true-color)
                 (const :tag "256 color"  256-color)
                 (const :tag "TTY color"  tty-color))
  :group 'sendai-theme)

(defcustom sendai-tab-padding
  (if (>= emacs-major-version 28) '(4 . 2) 2)
  "The default padding around tabs in `tab-bar-mode' and `tab-line-mode'.
This is passed to the `:box' face attribute and can be either an
integer (to use the same width in all directions) or a pair of
integers (to use separate widths on the X and Y axes)."
  :type '(choice integer (cons integer integer))
  :group 'sendai-theme)

;; Define some faces.  Note that the actual styling is done below.  Doing things
;; this way makes it easier to define each face with multiple face specs for
;; better `emacsclient' support.

(defface sendai-hl-todo-error nil
  "A face used to highlight error-level TODO-like keywords."
  :group 'sendai-theme
  :group 'hl-todo)

(defface sendai-hl-todo-warning nil
  "A face used to highlight warning-level TODO-like keywords."
  :group 'sendai-theme
  :group 'hl-todo)

(defface sendai-hl-todo-success nil
  "A face used to highlight success-level TODO-like keywords."
  :group 'sendai-theme
  :group 'hl-todo)

(defface sendai-hl-todo-info nil
  "A face used to highlight info-level TODO-like keywords."
  :group 'sendai-theme
  :group 'hl-todo)

(eval-and-compile
  (defun sendai--color (color-true color-256 &optional color-tty)
    "Generate a color spec holding values for various environments.
COLOR-TRUE is the color to use in true color (24-bit) modes, COLOR-256
the color to use in 8-bit modes, and COLOR-TTY the color to use in
8-bit TTY modes.  If COLOR-TTY isn't specified, COLOR-256 is used for
TTY modes instead."
    `[sendai--color ,color-true ,color-256 ,(or color-tty color-256)])

  (defconst sendai-palette
    `((bg-darker       ',(sendai--color "#161c23" "#1c1c1c"))
      (bg-dark         ',(sendai--color "#1c242e" "#262626"))
      (bg-primary      ',(sendai--color "#232c38" "#303030"))
      (bg-light        ',(sendai--color "#364454" "#444444"))
      (bg-lighter      ',(sendai--color "#4a5b71" "#5f5f5f" "black"))

      (fg-darker       ',(sendai--color "#7b96b7" "#5f87af" "brightblack"))
      (fg-dark         ',(sendai--color "#a2b6d0" "#87afd7"))
      (fg-primary      ',(sendai--color "#c9d6e9" "#d7d7d7" "white"))
      (fg-light        ',(sendai--color "#f0f6fe" "#ffffff" "brightwhite"))

      (red-darker      ',(sendai--color "#4f282a" 'unspecified))
      (orange-darker   ',(sendai--color "#513825" 'unspecified))
      (yellow-darker   ',(sendai--color "#4e462b" 'unspecified))
      (green-darker    ',(sendai--color "#2b402e" 'unspecified))
      (cyan-darker     ',(sendai--color "#204142" 'unspecified))
      (blue-darker     ',(sendai--color "#273b55" 'unspecified))
      (violet-darker   ',(sendai--color "#36354f" 'unspecified))
      (magenta-darker  ',(sendai--color "#462b3f" 'unspecified))

      (red-dark        ',(sendai--color "#792c32" "#870000"))
      (orange-dark     ',(sendai--color "#7b4d27" "#875f00"))
      (yellow-dark     ',(sendai--color "#796b2f" "#878700"))
      (green-dark      ',(sendai--color "#2a5d31" "#005f00"))
      (cyan-dark       ',(sendai--color "#185d5f" "#005f5f"))
      (blue-dark       ',(sendai--color "#245282" "#005f87"))
      (violet-dark     ',(sendai--color "#484982" "#5f5f87"))
      (magenta-dark    ',(sendai--color "#702e5e" "#5f005f"))

      (red-mid         ',(sendai--color "#ad3641" "#af0000"))
      (orange-mid      ',(sendai--color "#ab6520" "#af5f00"))
      (yellow-mid      ',(sendai--color "#b49635" "#afaf5f"))
      (green-mid       ',(sendai--color "#3d8638" "#5f875f"))
      (cyan-mid        ',(sendai--color "#1f8584" "#008787"))
      (blue-mid        ',(sendai--color "#2d77b6" "#0087af"))
      (violet-mid      ',(sendai--color "#6b63b5" "#5f5faf"))
      (magenta-mid     ',(sendai--color "#9c4080" "#875f87"))

      (red-primary     ',(sendai--color "#d33f4d" "#d70000" "brightred"))
      (orange-primary  ',(sendai--color "#d07a25" "#d78700"))
      (yellow-primary  ',(sendai--color "#ddb63a" "#d7af5f" "brightyellow"))
      (green-primary   ',(sendai--color "#4ba33f" "#5faf5f" "brightgreen"))
      (cyan-primary    ',(sendai--color "#26a19f" "#00afaf" "cyan"))
      (blue-primary    ',(sendai--color "#3491dc" "#5f87d7" "blue"))
      (violet-primary  ',(sendai--color "#8578db" "#8787d7"))
      (magenta-primary ',(sendai--color "#bc4d99" "#af5f87" "magenta"))

      (red-light       ',(sendai--color "#e86569" "#d75f5f" "red"))
      (orange-light    ',(sendai--color "#ee9e5f" "#ffaf5f"))
      (yellow-light    ',(sendai--color "#ded692" "#d7d787" "yellow"))
      (green-light     ',(sendai--color "#7cc36e" "#87af5f" "green"))
      (cyan-light      ',(sendai--color "#4edae5" "#5fd7d7" "brightcyan"))
      (blue-light      ',(sendai--color "#6cc2ff" "#5fafff" "brightblue"))
      (violet-light    ',(sendai--color "#afa2f2" "#afafff"))
      (magenta-light   ',(sendai--color "#db7fbb" "#d787af" "brightmagenta")))
    "An alist of colors defining the Sendai theme's palette.
Each value is a `sendai--color'."))


;; Utility functions

(defun sendai--default-tty-color (color)
  "Modify COLOR to indicate that the TTY-mode color should be the default.
This is useful for inheriting the default foreground/background from
the terminal."
  (setq color (copy-sequence color))
  (aset color (1- (length color)) 'unspecified)
  color)

(defun sendai-active-class ()
  "Get the active face class to use when setting non-`defface' colors."
  (or sendai-default-class
      (let ((colors (display-color-cells)))
        (cond
         ((>= colors 16777216) 'true-color)
         ((>= colors 256) (if (display-graphic-p) '256-color 'tty-color))))))

(defun sendai--subst-p (tree)
  "Return non-nil if TREE has a substitutable color value.
See `sendai--color'."
  (cond ((consp tree)
         (if (proper-list-p tree)
             (seq-some #'sendai--subst-p tree)
           (or (sendai--subst-p (car tree))
               (sendai--subst-p (cdr tree)))))
        ((vectorp tree)
         (or (eq (aref tree 0) 'sendai--color)
             (seq-some #'sendai--subst-p tree)))
        (t nil)))

(defun sendai--subst (tree index)
  "Replace abstract colors with their real values.
TREE is an object containing colors generated by `sendai--color'.
For each occurence of such a color, replace it with the real
value specified by INDEX (starting at one)."
  (cond ((consp tree)
         (if (proper-list-p tree)
             (mapcar (lambda (i) (sendai--subst i index)) tree)
           (let ((a (sendai--subst (car tree) index))
                 (d (sendai--subst (cdr tree) index)))
             (if (and (eq a (car tree)) (eq d (cdr tree)))
                 tree
               (cons a d)))))
        ((vectorp tree)
         (if (eq (aref tree 0) 'sendai--color)
             (aref tree index)
           (apply #'vector (mapcar (lambda (i) (sendai--subst i index))
                                   tree))))
        (t tree)))

(defun sendai--parameterize (class-names display-extra attrs)
  "Parameterize abstract color definitions according to CLASS-NAMES.
ATTRS is a face attribute list, and DISPLAY-EXTRA is an alist of
extra classifications to apply to the resulting list.  If any
classification in DISPLAY-EXTRA conflicts with one in
CLASS-NAMES, that display class is ignored entirely."
  (setq display-extra (sendai--subst display-extra 1))
  (when attrs
    (cond
     ((not (sendai--subst-p attrs))
      `((,display-extra . ,attrs)))
     ((memq display-extra '(default t))
      (error "Abstract color definitions not allowed here"))
     (t
      (or (mapcan
           (lambda (class-name)
             (let* ((entry (alist-get class-name sendai--face-classes))
                    (display-class (car entry)))
               (catch 'conflict
                 ;; Make sure the extra display rules don't conflict with the
                 ;; main display class.
                 (dolist (i display-extra)
                   (when (assq (car i) display-class)
                     (throw 'conflict nil)))
                 `((,(append display-extra display-class)
                    . ,(sendai--subst attrs (1+ (cdr entry))))))))
           class-names)
          (error "Unable to parameterize color definitions"))))))

(defun sendai--filter-face-attrs (attrs)
  "Filter a face attribute list (ATTRS) into constant and substitutable parts."
  (let (const-attrs subst-attrs)
    (while attrs
      (let ((key (pop attrs))
            (value (pop attrs)))
        (if (sendai--subst-p value)
            (setq subst-attrs (append subst-attrs (list key value)))
          (setq const-attrs (append const-attrs (list key value))))))
    (cons const-attrs subst-attrs)))

(defun sendai-make-face (&rest spec)
  "Fill a face spec (SPEC) with real colors."
  (let ((class-names (if sendai-inherit-tty-colors
                         '(true-color tty-color 256-color)
                       '(true-color 256-color))))
    (mapcan
     (lambda (elem)
       (sendai--parameterize class-names (car elem) (cdr elem)))
     spec)))

(defun sendai-face (&rest attrs)
  "Fill a face attribute list (ATTRS) with real color, the easy way.
ATTRS should be a plist of face attributes and their values.  If ATTRS
contains any nonparameterized attributes, this function will split them
out and apply them using the `default' display rule.

ATTRS may also be a full face specification, in which case the
nonparameterized filter above doesn't apply."
  (let ((spec (if (keywordp (car attrs))
                  (let ((filtered (sendai--filter-face-attrs attrs)))
                    `((default . ,(car filtered))
                      (nil . ,(cdr filtered))))
                attrs)))
    (apply #'sendai-make-face spec)))

(defmacro sendai-with-palette (&rest body)
  "Evaluate BODY with all the entries of `sendai-palette' in scope."
  (declare (indent 0))
  `(let ,sendai-palette
     (ignore ,@(mapcar #'car sendai-palette))
     ,@body))

(defmacro sendai-with-concrete-palette (class-name &rest body)
  "Evaluate BODY with the entries of `sendai-palette' bound to concrete colors.
CLASS-NAME is the class to use to concretize colors.  If nil, don't
evaluate BODY."
  (declare (indent 1))
  (let ((class-name-symbol (make-symbol "class-name"))
        (index-symbol (make-symbol "index")))
    `(when-let* ((,class-name-symbol ,class-name))
       (let* ((,index-symbol
               (1+ (cdr (alist-get ,class-name-symbol sendai--face-classes))))
              ,@(mapcar (lambda (i)
                          (list (car i) `(aref ,(cadr i) ,index-symbol)))
                        sendai-palette))
         (ignore ,@(mapcar #'car sendai-palette))
         ,@body))))

(defmacro sendai-set-theme-faces (theme &rest body)
  "Set faces for the specified THEME.
BODY is a list of face specs like (NAME SPEC...) or (NAME ATTRS...)
which `sendai-face' will convert to the final specification."
  (declare (indent 0))
  `(sendai-with-palette
     (custom-theme-set-faces
      ,theme
      ,@(mapcar
         (lambda (face)
           (let ((name (car face))
                 (spec (cdr face)))
             (list '\` (list name (list '\, (cons 'sendai-face spec))))))
         body))))

(defmacro sendai-set-faces (&rest body)
  "Set faces for the Sendai theme.
BODY is a list of face specs like (NAME SPEC...) or (NAME ATTRS...)
which `sendai-face' will convert to the final specification."
  `(sendai-set-theme-faces 'sendai ,@body))

(defun sendai--color-gradient (start stop step-number)
  "Return a list with STEP-NUMBER colors from START to STOP.
Like `color-gradient', but works on hex strings."
  (mapcar (lambda (c) (color-rgb-to-hex (car c) (cadr c) (caddr c) 2))
          (color-gradient (color-name-to-rgb start)
                          (color-name-to-rgb stop) step-number)))

(defun sendai--interpolate-colors (colors step-number)
  "Interpolate STEP-NUMBER colors between each successive color in COLORS."
  (let (result)
    (while (cdr colors)
      (push (cons (car colors)
                  (sendai--color-gradient (car colors) (cadr colors)
                                          step-number))
            result)
      (pop colors))
    (push colors result)
    (apply #'nconc (nreverse result))))


;; Face customizations

(sendai-set-faces
  ;; ----------
  ;; Core faces
  ;; ----------

  ;; Basics
  (default :background (sendai--default-tty-color bg-primary)
           :foreground fg-primary)
  (cursor :background fg-primary)
  (shadow :foreground fg-darker)
  (link :foreground blue-primary :underline t)
  (link-visited :foreground violet-primary :underline t)
  (success :foreground green-primary :weight 'bold)
  (warning :foreground orange-primary :weight 'bold)
  (error :foreground red-primary :weight 'bold)
  (escape-glyph :foreground magenta-light :weight 'bold
                :box (when (>= emacs-major-version 28)
                       `(:line-width (1 . -1) :color ,magenta-dark)))
  (homoglyph :foreground cyan-light)
  (nobreak-space :underline `(:color ,magenta-primary))
  (nobreak-hyphen :inherit 'nobreak-space)
  (confusingly-reordered (when (>= emacs-major-version 28)
                           `(((supports :box t))
                             :box (:line-width (-1 . -1) :color ,red-mid)))
                         `(((supports :underline ,red-primary))
                           :underline (:color ,red-primary :style wave))
                         `(nil :foreground ,red-primary))
  (textsec-suspicious (when (>= emacs-major-version 28)
                        `(((supports :box t))
                          :foreground ,orange-primary
                          :box (:line-width (-1 . -1) :color ,red-mid)))
                      `(((supports :underline ,red-primary))
                        :foreground ,orange-primary
                        :underline (:color ,red-primary :style wave))
                      `(nil :foreground ,red-primary))
  (separator-line :foreground fg-darker)
  (help-argument-name :foreground blue-light)
  (help-key-binding :background blue-darker :foreground blue-light
                    :box (when (>= emacs-major-version 28)
                           `(:line-width (1 . -1) :color ,blue-dark)))

  ;; Highlighting
  (fringe :background bg-primary)
  (highlight :background blue-mid :foreground fg-light)
  (region :background blue-dark :foreground fg-primary)
  (secondary-selection :background bg-lighter)
  (match :background yellow-light :foreground bg-primary
         :distant-foreground fg-primary)
  (isearch :background yellow-light :foreground bg-primary)
  (isearch-fail :background red-dark)
  (isearch-group-1 :background green-light :foreground bg-primary)
  (isearch-group-2 :background green-primary :foreground bg-primary)
  (lazy-highlight :background fg-darker :foreground bg-primary)
  (completions-common-part :foreground yellow-primary)
  (completions-highlight :background blue-mid)
  (show-paren-match :background violet-dark :foreground fg-light :weight 'bold
                    :box (when (>= emacs-major-version 28)
                           `(:line-width (-1 . -1) :color ,violet-mid)))
  (blink-matching-paren-offscreen :inherit 'show-paren-match)
  (show-paren-match-expression :background violet-darker)
  (show-paren-mismatch :background red-mid :foreground fg-light :weight 'bold
                       :box (when (>= emacs-major-version 28)
                              `(:line-width (-1 . -1) :color ,red-primary)))
  (hl-line :background bg-light)
  (bookmark-face :foreground magenta-light)
  (trailing-whitespace :background red-primary)
  (pulse-highlight-start-face :background blue-mid)

  ;; Mode line
  (mode-line :background bg-light :foreground fg-light)
  (mode-line-inactive :background bg-darker :foreground fg-dark)
  (mode-line-highlight :box fg-darker)

  ;; Misc UI
  (minibuffer-prompt :foreground blue-primary :weight 'bold)
  (header-line :background bg-lighter :foreground fg-primary)
  (header-line-highlight :background fg-darker :foreground bg-primary)
  (line-number :foreground fg-darker)
  (line-number-current-line :foreground fg-primary)
  (line-number-major-tick :background bg-lighter :foreground fg-dark)
  (line-number-minor-tick :background bg-light :foreground fg-darker)
  (widget-field :background bg-lighter)
  (widget-single-line-field :background bg-lighter)
  (vertical-border :foreground bg-lighter)
  (window-divider :foreground bg-lighter)
  (window-divider-first-pixel :foreground fg-darker)
  (window-divider-last-pixel :foreground bg-darker)
  (tty-menu-enabled-face :background bg-light :foreground fg-light)
  (tty-menu-disabled-face :background bg-light :foreground fg-dark)
  (tty-menu-selected-face :background blue-mid :foreground fg-light)

  ;; Font lock
  (font-lock-builtin-face :foreground blue-light)
  (font-lock-comment-face :foreground fg-darker)
  (font-lock-doc-face :inherit 'font-lock-comment-face)
  (font-lock-doc-markup-face :foreground cyan-primary)
  (font-lock-constant-face :foreground cyan-primary)
  (font-lock-function-name-face :foreground yellow-primary :weight 'bold)
  (font-lock-keyword-face :foreground blue-primary)
  (font-lock-negation-char-face :foreground yellow-light)
  (font-lock-regexp-grouping-construct :foreground yellow-light)
  (font-lock-regexp-grouping-backslash :foreground cyan-primary)
  (font-lock-string-face :foreground green-light)
  (font-lock-type-face :foreground cyan-light)
  (font-lock-variable-name-face :foreground yellow-light)
  (font-lock-warning-face :foreground red-primary)

  ;; -----------------
  ;; Built-in packages
  ;; -----------------

  ;; ansi-color
  (ansi-color-black :background bg-lighter :foreground bg-lighter)
  (ansi-color-red :background red-light :foreground red-light)
  (ansi-color-green :background green-light :foreground green-light)
  (ansi-color-yellow :background yellow-light :foreground yellow-light)
  (ansi-color-blue :background blue-primary :foreground blue-primary)
  (ansi-color-magenta :background magenta-primary :foreground magenta-primary)
  (ansi-color-cyan :background cyan-primary :foreground cyan-primary)
  (ansi-color-white :background fg-primary :foreground fg-primary)
  (ansi-color-bright-black :background fg-darker :foreground fg-darker)
  (ansi-color-bright-red :background red-primary :foreground red-primary)
  (ansi-color-bright-green :background green-primary :foreground green-primary)
  (ansi-color-bright-yellow :background yellow-primary
                            :foreground yellow-primary)
  (ansi-color-bright-blue :background blue-light :foreground blue-light)
  (ansi-color-bright-magenta :background magenta-light
                             :foreground magenta-light)
  (ansi-color-bright-cyan :background cyan-light :foreground cyan-light)
  (ansi-color-bright-white :background fg-light :foreground fg-light)

  ;; calendar
  (calendar-month-header :foreground yellow-primary :weight 'bold)
  (calendar-weekday-header :foreground blue-primary)
  (calendar-weekend-header :foreground blue-light)
  (diary :foreground cyan-light)

  ;; change-log-mode
  (change-log-date :foreground cyan-primary)
  (change-log-name :foreground yellow-light)
  (change-log-email :foreground blue-primary)
  (change-log-file :foreground yellow-primary :weight 'bold)
  (change-log-list :foreground blue-light)
  (change-log-conditionals :foreground green-light)
  (change-log-function :foreground green-light)
  (change-log-acknowledgment :foreground fg-darker)

  ;; comint-mode
  (comint-highlight-prompt :foreground fg-darker :weight 'bold)

  ;; compilation-mode
  (compilation-error :foreground red-primary :weight 'bold
                     :distant-foreground red-light)
  (compilation-warning :foreground orange-primary :weight 'bold
                       :distant-foreground orange-light)
  (compilation-info :foreground blue-primary :weight 'bold
                    :distant-foreground blue-light)
  (compilation-line-number :foreground blue-light)
  (compilation-column-number :foreground blue-light)
  (compilation-mode-line-exit :foreground green-primary :weight 'bold
                              :distant-foreground green-light)
  (compilation-mode-line-fail :foreground red-primary :weight 'bold
                              :distant-foreground red-light)
  (compilation-mode-line-run :foreground cyan-light :weight 'bold)

  ;; cua-mode
  (cua-global-mark :background yellow-primary :foreground bg-primary)
  (cua-rectangle :inherit 'region)
  (cua-rectangle-noselect :inherit 'secondary-selection)

  ;; customize
  (custom-group-tag :foreground yellow-primary :weight 'bold :height 1.2)
  (custom-group-tag-1 :foreground cyan-light :weight 'bold :height 1.2)
  (custom-state :foreground green-primary)
  (custom-changed :foreground yellow-light)
  (custom-modified :foreground yellow-light)
  (custom-themed :foreground yellow-light)
  (custom-set :background green-light :foreground bg-primary)
  (custom-invalid :foreground red-primary :weight 'bold
                  :underline '(:style wave))
  (custom-rogue :foreground orange-primary :weight 'bold
                :underline '(:style wave))
  (custom-variable-tag :foreground blue-light :weight 'bold)
  (custom-variable-obsolete :foreground fg-darker :weight 'bold)
  (custom-button :background fg-primary :foreground bg-primary
                 :box '(:line-width 1 :style released-button))
  (custom-button-mouse :background fg-light :foreground bg-primary
                       :box '(:line-width 1 :style released-button))
  (custom-button-pressed :background fg-primary :foreground bg-primary
                         :box '(:line-width 1 :style pressed-button))
  (custom-button-unraised :inherit 'default :underline t)
  (custom-button-pressed-unraised :inherit 'custom-button-unraised
                                  :foreground violet-primary)

  ;; diff-mode
  (diff-header :foreground fg-primary)
  (diff-file-header :foreground yellow-primary :weight 'bold)
  (diff-hunk-header :foreground cyan-light)
  (diff-error :foreground red-primary :weight 'bold :underline '(:style wave))
  (diff-added :background green-darker)
  (diff-removed :background red-darker)
  (diff-changed :background yellow-darker)
  (diff-changed-unspecified :background yellow-darker)
  (diff-indicator-added :inherit 'diff-added :foreground green-light)
  (diff-indicator-removed :inherit 'diff-removed :foreground red-light)
  (diff-indicator-changed :inherit 'diff-changed :foreground yellow-light)
  (diff-refine-added :background green-dark)
  (diff-refine-removed :background red-dark)
  (diff-refine-changed :background yellow-dark)

  ;; dired
  (dired-header :foreground blue-light :weight 'bold)
  (dired-directory :foreground blue-primary :weight 'bold)
  (dired-symlink :foreground cyan-light)
  (dired-broken-symlink :inherit 'dired-symlink
                        :underline `(:color ,red-primary :style wave))
  (dired-special :foreground yellow-light)
  (dired-perm-write :foreground orange-light)
  (dired-set-id :foreground red-primary)
  (dired-marked :foreground yellow-primary :weight 'bold)
  (dired-mark :inherit 'dired-marked)
  (dired-flagged :foreground red-primary :weight 'bold)

  ;; ediff
  (ediff-even-diff-Ancestor :background blue-darker)
  (ediff-even-diff-A :background blue-darker)
  (ediff-even-diff-B :background blue-darker)
  (ediff-even-diff-C :background blue-darker)
  (ediff-odd-diff-Ancestor :background blue-darker)
  (ediff-odd-diff-A :background blue-darker)
  (ediff-odd-diff-B :background blue-darker)
  (ediff-odd-diff-C :background blue-darker)
  (ediff-current-diff-Ancestor :background yellow-darker)
  (ediff-current-diff-A :background red-darker)
  (ediff-current-diff-B :background green-darker)
  (ediff-current-diff-C :background blue-darker)
  (ediff-fine-diff-Ancestor :background yellow-dark)
  (ediff-fine-diff-A :background red-dark)
  (ediff-fine-diff-B :background green-dark)
  (ediff-fine-diff-C :background blue-dark)

  ;; edmacro
  (edmacro-label :foreground blue-light)

  ;; eglot
  (eglot-highlight-symbol-face `(((supports :box (:color ,blue-dark)))
                                 :background ,blue-darker
                                 :box (:line-width (-1 . -1) :color ,blue-dark))
                               `(nil :background ,blue-darker :slant italic))

  ;; eldoc
  (eldoc-highlight-function-argument :foreground blue-light)

  ;; emacs-authors-mode
  (emacs-authors-default :foreground fg-darker :slant 'italic)
  (emacs-authors-author :foreground yellow-primary :weight 'bold)
  (emacs-authors-descriptor :foreground blue-light)

  ;; emacs-news-mode
  (emacs-news-is-documented :foreground green-light :weight 'bold)
  (emacs-news-does-not-need-documentation :foreground fg-darker :weight 'bold)

  ;; ert
  (ert-test-result-expected :background green-primary :foreground fg-light)
  (ert-test-result-unexpected :background red-primary :foreground fg-light)

  ;; eww
  (eww-form-submit :background fg-primary :foreground bg-primary
                   :box '(:line-width 1 :style released-button))
  (eww-form-file :inherit 'eww-form-submit)
  (eww-form-checkbox
   :background fg-darker :foreground bg-primary
   :box `(:line-width 1 :color ,fg-dark :style released-button))
  (eww-form-select :inherit 'eww-form-checkbox)
  (eww-form-text :background bg-lighter :box `(:line-width 1 :color ,fg-darker))
  (eww-form-textarea :background bg-lighter)
  (eww-valid-certificate :foreground green-light :weight 'bold)
  (eww-invalid-certificate :foreground red-primary :weight 'bold)

  ;; eshell
  (eshell-prompt :foreground fg-darker :weight 'bold)
  (eshell-ls-directory :foreground blue-primary :weight 'bold)
  (eshell-ls-archive :foreground magenta-primary :weight 'bold)
  (eshell-ls-executable :foreground green-light)
  (eshell-ls-product :foreground cyan-primary)
  (eshell-ls-readonly :foreground orange-light)
  (eshell-ls-symlink :foreground cyan-light)
  (eshell-ls-special :foreground yellow-light)
  (eshell-ls-backup :foreground fg-darker)
  (eshell-ls-missing :foreground red-primary :weight 'bold)
  (eshell-ls-clutter :foreground red-primary :weight 'bold)
  (eshell-ls-unreadable :foreground orange-primary)

  ;; flymake
  (flymake-error :underline `(:color ,red-primary :style wave))
  (flymake-warning :underline `(:color ,orange-primary :style wave))
  (flymake-note :underline `(:color ,blue-primary :style wave))

  ;; flyspell
  (flyspell-duplicate :underline `(:color ,orange-primary :style wave))
  (flyspell-incorrect :underline `(:color ,red-primary :style wave))

  ;; gdb
  (breakpoint-enabled :foreground red-primary)
  (breakpoint-disabled :foreground fg-darker)

  ;; hexl-mode
  (hexl-address-region :background blue-darker :foreground fg-primary)
  (hexl-ascii-region :inherit 'hexl-address-region)

  ;; hi-lock-mode
  (hi-yellow :background yellow-primary :foreground bg-primary)
  (hi-pink :background red-light :foreground bg-primary)
  (hi-green :background green-light :foreground bg-primary)
  (hi-blue :background blue-light :foreground bg-primary)
  (hi-salmon :background orange-primary :foreground bg-primary)
  (hi-aquamarine :background cyan-light :foreground bg-primary)
  (hi-black-b :weight 'bold)
  (hi-blue-b :foreground blue-primary :weight 'bold)
  (hi-green-b :foreground green-primary :weight 'bold)
  (hi-red-b :foreground red-primary :weight 'bold)
  (hi-black-hb :foreground fg-light :weight 'bold)

  ;; image-dired
  (image-dired-thumb-header-file-size :foreground blue-light)
  (image-dired-thumb-mark :background yellow-primary)
  (image-dired-thumb-flagged :background red-primary)

  ;; info-mode
  (info-title-1 :height 1.5 :weight 'bold)
  (info-title-2 :height 1.4 :weight 'bold)
  (info-title-3 :height 1.3 :weight 'bold)
  (info-title-4 :height 1.1 :weight 'bold)
  (info-menu-header :weight 'bold)
  (info-menu-star :foreground yellow-primary)
  (info-node :foreground fg-light :weight 'bold :slant 'italic)
  (info-header-xref :foreground blue-light :underline t)

  ;; makefile-mode
  (makefile-space :background red-primary)

  ;; message
  (message-header-name :foreground blue-light)
  (message-header-subject :foreground fg-light :weight 'bold)
  (message-header-to :foreground yellow-light :weight 'bold)
  (message-header-cc :foreground green-light :weight 'bold)
  (message-header-newsgroups :foreground yellow-primary :weight 'bold)
  (message-header-other :foreground fg-primary)
  (message-header-xheader :foreground blue-primary)
  (message-separator :foreground fg-darker)
  (message-mml :foreground yellow-primary)
  (message-cited-text-1 :foreground violet-light)
  (message-cited-text-2 :foreground green-light)
  (message-cited-text-3 :foreground magenta-light)
  (message-cited-text-4 :foreground cyan-light)

  ;; org-mode
  ;; Note: `org-level-N' is inherited from `outline-N'.
  (org-document-info-keyword :inherit 'org-meta-line)
  (org-document-title :foreground blue-light :weight 'bold)
  (org-document-info :foreground blue-light)
  (org-ellipsis :foreground fg-darker)
  (org-hide :foreground bg-primary)
  (org-checkbox :foreground fg-darker :weight 'bold)
  (org-code :inherit 'fixed-pitch :foreground cyan-primary)
  (org-verbatim :inherit 'fixed-pitch :foreground cyan-primary)
  (org-latex-and-related :foreground fg-darker)
  (org-target :background bg-light :foreground fg-dark
              :box (when (>= emacs-major-version 28)
                     `(:line-width (-1 . -1) :color ,fg-darker)))
  (org-footnote :foreground blue-primary :underline t)
  (org-date :foreground violet-primary :underline t)
  (org-sexp-date :foreground cyan-primary)
  (org-todo :foreground red-light :weight 'bold)
  (org-done :foreground green-light :weight 'bold)
  (org-headline-todo :foreground red-primary :weight 'bold)
  (org-headline-done :foreground green-primary)
  (org-tag :foreground yellow-light :weight 'normal)
  (org-tag-group :foreground yellow-light :weight 'normal :slant 'italic)
  (org-priority :foreground orange-light)
  (org-formula :foreground orange-light)
  (org-macro :foreground violet-light)
  (org-block :background blue-darker :foreground fg-primary :extend t)
  (org-quote :slant 'italic)
  (org-verse :slant 'italic)
  (org-table :background blue-darker :foreground blue-light)
  (org-table-header :background blue-dark :foreground fg-primary)
  (org-drawer :foreground blue-light)
  (org-special-keyword :foreground blue-primary)
  (org-date-selected :background yellow-light :foreground bg-primary)
  (org-column :background bg-light
              :box '(:line-width -1 :style released-button))
  (org-column-title :background bg-lighter :weight 'bold)
  (org-agenda-structure :foreground fg-darker)
  (org-agenda-date :foreground blue-primary)
  (org-agenda-date-weekend :foreground blue-light)
  (org-agenda-date-today :foreground yellow-light)
  (org-agenda-current-time :foreground yellow-light)
  (org-agenda-done :foreground violet-primary)
  (org-time-grid :foreground fg-darker)
  (org-scheduled :foreground violet-light)
  (org-scheduled-previously :foreground orange-light)
  (org-scheduled-today :foreground violet-light)
  (org-mode-line-clock-overrun :background red-primary)
  (org-habit-clear-face :background blue-darker :foreground fg-light)
  (org-habit-clear-future-face :background blue-dark :foreground fg-light)
  (org-habit-ready-face :background green-mid :foreground fg-light)
  (org-habit-ready-future-face :background green-primary :foreground fg-light)
  (org-habit-alert-face :background yellow-dark :foreground fg-light)
  (org-habit-alert-future-face :background yellow-mid :foreground fg-light)
  (org-habit-overdue-face :background red-mid :foreground fg-light)
  (org-habit-overdue-future-face :background red-primary :foreground fg-light)
  (org-dispatcher-highlight :inherit 'help-key-binding)

  ;; outline-mode
  (outline-1 :foreground yellow-primary :weight 'bold)
  (outline-2 :foreground cyan-light :weight 'bold)
  (outline-3 :foreground violet-light :weight 'bold)
  (outline-4 :foreground blue-light :weight 'bold)
  (outline-5 :foreground yellow-light :weight 'bold)
  (outline-6 :foreground cyan-light :weight 'bold)
  (outline-7 :foreground violet-light :weight 'bold)
  (outline-8 :foreground blue-light :weight 'bold)

  ;; package
  (package-status-built-in :foreground blue-light)
  (package-status-installed :foreground green-light)
  (package-status-from-source :foreground cyan-primary)
  (package-status-dependency :foreground yellow-light)
  (package-status-held :foreground violet-light)
  (package-status-disabled :foreground fg-darker)
  (package-status-new :foreground fg-light :weight 'bold)
  (package-status-avail-obso :foreground fg-darker)
  (package-status-unsigned :underline `(:color ,red-primary :style wave))
  (package-mark-install-line :background green-darker)
  (package-mark-delete-line :background red-darker)

  ;; proced
  (proced-user :weight 'bold)
  (proced-emacs-pid :foreground violet-primary)
  (proced-pid :foreground blue-primary)
  (proced-session-leader-pid :foreground blue-primary :underline t)
  (proced-ppid :foreground blue-mid)
  (proced-pgrp :foreground blue-mid)
  (proced-sess :foreground blue-mid)
  (proced-cpu :foreground cyan-primary :weight 'bold)
  (proced-mem :foreground cyan-mid)
  (proced-memory-low-usage :foreground green-primary)
  (proced-memory-medium-usage :foreground yellow-primary)
  (proced-memory-high-usage :foreground orange-primary)
  (proced-run-status-code :foreground green-light)
  (proced-interruptible-sleep-status-code :foreground bg-lighter)
  (proced-uninterruptible-sleep-status-code :foreground red-primary)
  (proced-time-colon :foreground fg-darker)
  (proced-executable :foreground blue-light)
  (proced-mark :foreground yellow-primary :weight 'bold)
  (proced-marked :inherit 'proced-mark)
  (proced-sort-header :foreground fg-dark :inverse-video t :weight 'bold)

  ;; rst-mode
  (rst-level-1 :inherit '(outline-1))
  (rst-level-2 :inherit '(outline-2))
  (rst-level-3 :inherit '(outline-3))
  (rst-level-4 :inherit '(outline-4))
  (rst-level-5 :inherit '(outline-5))
  (rst-level-6 :inherit '(outline-6))
  (rst-adornment :foreground fg-darker)
  (rst-block :foreground fg-darker)
  (rst-comment :foreground fg-darker)
  (rst-transition :foreground fg-darker)
  (rst-definition :foreground yellow-light)
  (rst-directive :foreground blue-light)
  (rst-external :foreground green-light)
  (rst-literal :foreground cyan-primary)
  (rst-reference :foreground blue-primary)

  ;; ruler-mode
  (ruler-mode-default
   :background bg-light :foreground fg-darker
   :box `( :line-width ,(if (>= emacs-major-version 28) '(-1 . 1) 1)
           :color ,bg-light :style released-button))
  (ruler-mode-column-number :inherit 'ruler-mode-default :foreground fg-primary)
  (ruler-mode-current-column :inherit 'ruler-mode-default
                             :foreground yellow-primary :weight 'bold)
  (ruler-mode-comment-column :inherit 'ruler-mode-default
                             :foreground cyan-light)
  (ruler-mode-fill-column :inherit 'ruler-mode-default :foreground cyan-light)
  (ruler-mode-goal-column :inherit 'ruler-mode-default :foreground cyan-light)
  (ruler-mode-tab-stop :inherit 'ruler-mode-default :foreground blue-primary)
  (ruler-mode-fringes :inherit 'ruler-mode-default)
  (ruler-mode-pad :inherit 'ruler-mode-default :foreground fg-primary)

  ;; sh-mode
  (sh-escaped-newline :foreground fg-darker)
  (sh-heredoc :foreground green-light)
  (sh-quoted-exec :foreground orange-light)

  ;; shr
  (shr-h1 :height 2.00 :weight 'bold)
  (shr-h2 :height 1.50 :weight 'bold)
  (shr-h3 :height 1.17 :weight 'bold)
  (shr-h4 :height 1.00 :weight 'bold)
  (shr-h5 :height 0.83 :weight 'bold)
  (shr-h6 :height 0.67 :weight 'bold)
  (shr-abbreviation :underline '(:style dots))
  (shr-mark :background yellow-primary :foreground bg-primary)
  (shr-selected-link :background cyan-primary :foreground fg-light
                     :box (when (>= emacs-major-version 28)
                            `(:line-width (-1 . -1) :color ,cyan-light)))

  ;; smerge-mode
  (smerge-markers :background blue-darker :foreground fg-light)
  (smerge-upper :background red-darker)
  (smerge-lower :background green-darker)
  (smerge-base :background yellow-darker)
  (smerge-refined-removed :background red-dark)
  (smerge-refined-added :background green-dark)

  ;; tab-bar
  (tab-bar :inherit 'variable-pitch :background bg-lighter)
  (tab-bar-tab-group-current :weight 'bold)
  (tab-bar-tab-group-inactive :inherit 'tab-bar-tab-inactive :weight 'bold)
  (tab-bar-tab
    :background bg-primary :foreground fg-primary :overline yellow-primary
    :box `(:line-width ,sendai-tab-padding :color ,bg-primary))
  (tab-bar-tab-inactive
    :background bg-light :foreground fg-dark
    :box `(:line-width ,sendai-tab-padding :color ,bg-light))
  (tab-bar-tab-ungrouped :inherit 'tab-bar-tab-inactive)

  ;; tab-line
  (tab-line :inherit 'variable-pitch :background bg-lighter)
  (tab-line-tab-group :inherit 'tab-line :weight 'bold)
  (tab-line-tab
    :background bg-primary :foreground fg-darker :overline yellow-dark
    :box `(:line-width ,sendai-tab-padding :color ,bg-primary))
  (tab-line-tab-current
    :background bg-primary :foreground fg-primary :overline yellow-primary
    :box `(:line-width ,sendai-tab-padding :color ,bg-primary))
  (tab-line-tab-inactive
    :background bg-light :foreground fg-dark :overline bg-light
    :box `(:line-width ,sendai-tab-padding :color ,bg-light))
  (tab-line-tab-inactive-alternate :overline blue-mid)
  (tab-line-highlight :foreground fg-light)
  (tab-line-tab-special :slant 'italic)

  ;; transient
  (transient-heading :foreground yellow-primary :weight 'bold)
  (transient-key :inherit 'help-key-binding)
  (transient-separator :background fg-darker)
  (transient-higher-level :underline t)
  (transient-active-infix :background yellow-light)
  (transient-enabled-suffix :background green-light)
  (transient-disabled-suffix :background red-primary)
  (transient-inapt-suffix :inherit 'shadow :slant 'italic)
  (transient-argument :foreground orange-light)
  (transient-value :foreground green-light)
  (transient-inactive-argument :inherit 'shadow)
  (transient-inactive-value :inherit 'shadow)
  (transient-mismatched-key :underline t)
  (transient-nonstandard-key :underline t)
  (transient-unreachable :inherit 'shadow)
  (transient-unreachable-key :inherit 'shadow)
  (transient-amaranth :foreground magenta-primary)
  (transient-blue :foreground blue-primary)
  (transient-pink :foreground magenta-light)
  (transient-purple :foreground violet-primary)
  (transient-red :foreground red-primary)
  (transient-teal :foreground cyan-primary)

  ;; vc-dir
  (vc-dir-header :foreground blue-light)
  (vc-dir-header-value :foreground fg-primary)
  (vc-dir-directory :foreground blue-primary :weight 'bold)
  (vc-dir-file :foreground fg-primary)
  (vc-dir-status-up-to-date :foreground green-light)
  (vc-dir-status-edited :foreground yellow-light)
  (vc-dir-status-ignored :foreground fg-darker)
  (vc-dir-status-warning :foreground red-primary :weight 'bold)
  (vc-dir-mark-indicator :foreground yellow-primary :weight 'bold)

  ;; whitespace-mode
  (whitespace-space :foreground violet-primary)
  (whitespace-hspace :inherit 'whitespace-space)
  (whitespace-tab :inherit 'whitespace-space)
  (whitespace-newline :foreground bg-lighter)
  (whitespace-indentation :background violet-dark :foreground fg-primary)
  (whitespace-empty :inherit 'whitespace-indentation)
  (whitespace-big-indent :background violet-primary :foreground fg-light)
  (whitespace-line :background red-dark)
  (whitespace-trailing :background red-primary :foreground fg-light)
  (whitespace-missing-newline-at-eof :inherit 'whitespace-trailing)
  (whitespace-space-before-tab :inherit 'whitespace-trailing)
  (whitespace-space-after-tab :inherit 'whitespace-indentation)

  ;; --------------------
  ;; Third-party packages
  ;; --------------------

  ;; company-mode
  (company-preview :foreground fg-primary :background blue-dark)
  (company-preview-common :foreground yellow-primary)
  (company-preview-search :inherit 'isearch)
  (company-template-field :foreground fg-primary :background bg-lighter)
  (company-tooltip :foreground fg-primary :background bg-lighter)
  (company-tooltip-common :foreground yellow-primary)
  (company-tooltip-annotation :foreground cyan-light)
  (company-tooltip-selection :background blue-mid)
  (company-tooltip-search :inherit 'isearch)
  (company-tooltip-search-selection :inherit 'isearch)
  (company-scrollbar-fg :background fg-darker)
  (company-scrollbar-bg :background bg-light)
  (company-echo-common :foreground yellow-primary)

  ;; corfu-mode
  (corfu-default :foreground fg-primary :background bg-lighter)
  (corfu-border :background bg-darker)
  (corfu-bar :background fg-darker)
  (corfu-current :background blue-mid)

  ;; elfeed
  (elfeed-search-title-face :foreground fg-dark)
  (elfeed-search-unread-title-face :foreground fg-primary :weight 'bold)
  (elfeed-search-date-face :foreground fg-darker)
  (elfeed-search-feed-face :foreground blue-primary)
  (elfeed-search-tag-face :foreground yellow-light)
  (elfeed-search-unread-count-face :foreground blue-light)
  (elfeed-log-date-face :foreground cyan-primary)
  (elfeed-log-error-level-face :foreground red-primary)
  (elfeed-log-warn-level-face :foreground yellow-primary)
  (elfeed-log-info-level-face :foreground blue-primary)
  (elfeed-log-debug-level-face :foreground magenta-light)

  ;; form-feed
  (form-feed-line `(((supports :strike-through t))
                    :foreground ,fg-darker :strike-through t)
                  `(nil :foreground ,fg-darker :underline t))

  ;; hl-todo
  (hl-todo :foreground yellow-light :weight 'bold)
  (sendai-hl-todo-error :inherit 'hl-todo :foreground red-primary)
  (sendai-hl-todo-warning :inherit 'hl-todo :foreground orange-primary)
  (sendai-hl-todo-success :inherit 'hl-todo :foreground green-primary)
  (sendai-hl-todo-info :inherit 'hl-todo :foreground blue-light)

  ;; js2-mode
  (js2-function-param :foreground yellow-light)
  (js2-function-call '(nil))
  (js2-external-variable :foreground orange-primary)
  (js2-object-property '(nil))
  (js2-jsdoc-tag :inherit 'c-annotation-face)
  (js2-jsdoc-type :inherit 'font-lock-type-face)
  (js2-jsdoc-value :inherit 'font-lock-variable-name-face)
  (js2-jsdoc-html-tag-delimiter :foreground fg-primary)
  (js2-jsdoc-html-tag-name :foreground yellow-primary)
  (js2-error :foreground red-primary)
  (js2-warning :underline `(:color ,orange-primary :style wave))

  ;; markdown-mode
  (markdown-header-face :underline t)
  (markdown-header-face-1 :inherit '(outline-1 markdown-header-face))
  (markdown-header-face-2 :inherit '(outline-2 markdown-header-face))
  (markdown-header-face-3 :inherit '(outline-3 markdown-header-face))
  (markdown-header-face-4 :inherit '(outline-4 markdown-header-face))
  (markdown-header-face-5 :inherit '(outline-5 markdown-header-face))
  (markdown-header-face-6 :inherit '(outline-6 markdown-header-face))
  (markdown-html-tag-delimiter-face :inherit 'default)
  (markdown-html-tag-name-face :inherit 'font-lock-function-name-face)
  (markdown-html-attr-name-face :inherit 'font-lock-variable-name-face)
  (markdown-html-attr-value-face :inherit 'font-lock-string-face)
  (markdown-html-entity-face :inherit 'font-lock-variable-name-face)
  (markdown-gfm-checkbox-face :foreground fg-darker :weight 'bold)
  (markdown-table-face :foreground blue-light :background blue-darker)
  (markdown-inline-code-face :foreground cyan-primary)
  (markdown-pre-face :foreground fg-darker)
  (markdown-code-face :background blue-darker :extend t)
  (markdown-language-keyword-face :foreground cyan-light)

  ;; rainbow-delimiters
  (rainbow-delimiters-depth-1-face :foreground fg-primary)
  (rainbow-delimiters-depth-2-face :foreground fg-darker)
  (rainbow-delimiters-depth-3-face :foreground fg-light)
  (rainbow-delimiters-depth-4-face :foreground fg-dark)
  (rainbow-delimiters-base-error-face :foreground red-primary)

  ;; solaire-mode
  (solaire-default-face :background bg-dark :foreground fg-primary)
  (solaire-fringe-face :background bg-dark)

  ;; telephone-line
  (telephone-line-accent-active :background blue-mid)
  (telephone-line-accent-inactive :background blue-dark)

  ;; tempel
  (tempel-field :background violet-dark :foreground fg-primary
                :box (when (>= emacs-major-version 28)
                       `(:line-width (-1 . -1) :color ,violet-mid)))
  (tempel-form :foreground magenta-light)
  (tempel-default :foreground violet-light :slant 'italic)

  ;; wgrep
  (wgrep-face :background violet-dark)
  (wgrep-done-face :foreground green-light :distant-foreground green-dark)
  (wgrep-delete-face :background red-darker :foreground fg-dark)
  (wgrep-reject-face :weight 'bold :foreground red-primary)
  (wgrep-file-face :background bg-light)

  ;; which-key
  (which-key-key-face :inherit 'help-key-binding)
  (which-key-command-description-face :foreground cyan-primary)
  (which-key-group-description-face :foreground cyan-light :weight 'bold)

  ;; yaml-mode
  (yaml-tab-face :background red-primary))

(when (< emacs-major-version 28)
  (sendai-set-faces
    ;; Backwards compatibility for term
    (term-color-black :background bg-lighter :foreground bg-lighter)
    (term-color-red :background red-light :foreground red-light)
    (term-color-green :background green-light :foreground green-light)
    (term-color-yellow :background yellow-light :foreground yellow-light)
    (term-color-blue :background blue-primary :foreground blue-primary)
    (term-color-magenta :background magenta-primary :foreground magenta-primary)
    (term-color-cyan :background cyan-primary :foreground cyan-primary)
    (term-color-white :background fg-primary :foreground fg-primary)))

(custom-theme-set-variables
 'sendai
 `(hl-todo-keyword-faces
   '(("FIXME" . sendai-hl-todo-error)
     ("TODO"  . sendai-hl-todo-warning)
     ("HACK"  . sendai-hl-todo-warning)
     ("XXX"   . sendai-hl-todo-warning)
     ("DONE"  . sendai-hl-todo-success)
     ("NOTE"  . sendai-hl-todo-info)))
 `(rainbow-delimiters-max-face-count 4))

(sendai-with-concrete-palette (sendai-active-class)
  (when (< emacs-major-version 28)
    (custom-theme-set-variables
     'sendai
     `(ansi-color-names-vector
       ,(vector bg-lighter red-light green-light yellow-light
                blue-primary magenta-primary cyan-primary fg-primary))))

  (custom-theme-set-variables
   'sendai
   `(xterm-color-names
     ,(vector bg-lighter red-light green-light yellow-light
              blue-primary magenta-primary cyan-primary fg-primary))
   `(xterm-color-names-bright
     ,(vector fg-darker red-primary green-primary yellow-primary
              blue-light magenta-light cyan-light fg-light)))

  (when (eq (sendai-active-class) 'true-color)
    (custom-theme-set-variables
     'sendai
     `(vc-annotate-color-map
       ',(let ((colors (if (bound-and-true-p vc-annotate-background-mode)
                           (list red-darker orange-darker yellow-darker
                                 green-darker cyan-darker blue-darker)
                         (list red-light orange-light yellow-light
                                 green-light cyan-light blue-light)))
              (age 0))
          (mapcar (lambda (c) (cons (setq age (+ age 20)) c))
                  (sendai--interpolate-colors colors 3)))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'sendai)
;;; sendai-theme.el ends here
