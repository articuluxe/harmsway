;;; autothemer.el --- Conveniently define themes -*- lexical-binding: t -*-
;;
;; Authors: Sebastian Sturm, Jason Milkins
;;
;; Copyright 2015-2022 Sebastian Sturm, Jason Milkins
;;
;; Maintainer: Jason Milkins <jasonm23@gmail.com>
;;
;; URL: https://github.com/jasonm23/autothemer
;; Version: 0.2.10
;; Package-Requires: ((dash "2.10.0") (emacs "26.1"))
;;
;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Reduces the amount of boilerplate code needed to define custom themes. Also
;; provides the user with an interactive command that automatically generates
;; face customization code using the theme's color palette.

;;; Code:
(require 'cl-lib)
(require 'dash)
(require 'lisp-mnt)
(require 'subr-x)

(cl-defstruct
    autothemer--color
  name
  value)

(cl-defstruct
    autothemer--theme
  colors
  defined-faces
  name
  description)

(defvar autothemer--current-theme nil
  "Internal variable of type `autothemer--theme' used by autothemer.
Contains the color palette and the list of faces most recently
customized using `autothemer-deftheme'.")

(defun autothemer--reduced-spec-to-facespec (display reduced-specs)
  "Create a face spec for DISPLAY, with specs REDUCED-SPECS.
E.g., (autothemer--reduced-spec-to-facespec '(min-colors 60)
'(button (:underline t :foreground red)))
-> `(button (((min-colors 60) (:underline ,t :foreground
,red))))."
  (let* ((face (elt reduced-specs 0))
         (properties (elt reduced-specs 1))
         (spec (autothemer--demote-heads `(list (,display ,properties)))))
    `(list ',face ,spec)))

(defun autothemer--demote-heads (expr)
  "Demote every list head within EXPR by one element.
E.g., (a (b c d) e (f g)) -> (list a (list b c d) e (list f g))."
  (if (listp expr)
      `(list ,@(mapcar
                (lambda (it) (if (and (listp it) (not (eq (car it) 'quote)))
                                 (autothemer--demote-heads it) it))
                expr))
    expr))

;;;###autoload
(defmacro autothemer-deftheme (name description palette reduced-specs &rest body)
  "Define a theme NAME with description DESCRIPTION.
A color PALETTE can be used to define let*-like
bindings within both the REDUCED-SPECS and the BODY."
  (let* ((face-names (-map #'car reduced-specs))
         (color-names (-map #'car (-drop 1 palette)))
         (n-displays (length (car palette)))
         (n-faces (length reduced-specs))
         (face-customizer)
         (full-palette (autothemer--fill-empty-palette-slots palette))
         (face-specs (make-symbol "face-specs"))
         (temp-n (make-symbol "n"))
         (temp-defined-colors (make-symbol "defined-colors"))
         (temp-color-structs (make-symbol "defined-colors-as-structs"))
         (temp-color (make-symbol "color"))
         (temp-colorname (make-symbol "colorname")))
    (setq face-customizer
          `(let ((,face-specs)
                 (,temp-color-structs)
                 (,temp-defined-colors))
             (deftheme ,name ,description)
             ,@(cl-loop for n from 0 to (1- n-displays)
                        collect
                        `(let* ,(autothemer--extract-let-block full-palette n)
                           ,@(when (and body (eq n 0))
                               body)
                           ,(when (> n 0)
                              `(ignore ,@color-names))
                           ,(when (and (eq n 0) (not (bound-and-true-p byte-compile-current-file)))
                              `(progn
                                 (setq ,temp-defined-colors
                                       (list ,@(--map (list 'list `',it it) color-names)))
                                 (setq ,temp-color-structs
                                       (cl-loop for (,temp-colorname ,temp-color)
                                                in ,temp-defined-colors
                                                collect (make-autothemer--color :name ,temp-colorname
                                                                                :value ,temp-color)))
                                 (setq autothemer--current-theme
                                       (make-autothemer--theme
                                        :name ,(symbol-name name)
                                        :description ,description
                                        :colors ,temp-color-structs
                                        :defined-faces ',face-names))))
                           (setq ,face-specs
                                 (autothemer--append-column
                                  ,face-specs
                                  (list ,@(--map `(list
                                                   (list
                                                    ',(autothemer--extract-display palette n)
                                                    ,(autothemer--demote-heads (elt it 1))))
                                                 reduced-specs))))))
             (apply #'custom-theme-set-faces ',name
                    (cl-loop for ,temp-n from 0 to ,(1- n-faces)
                             collect (list (elt ',face-names ,temp-n)
                                           (elt ,face-specs ,temp-n))))))
    face-customizer))

(defun autothemer--color-distance (color autothemer-color)
  "Return the distance in rgb space between COLOR and AUTOTHEMER-COLOR.
Here, COLOR is an Emacs color specification and AUTOTHEMER-COLOR is of
type `autothemer--color'."
  (let ((rgb-1 (autothemer-hex-to-rgb color))
        (rgb-2 (autothemer-hex-to-rgb (autothemer--color-value autothemer-color))))
    (-sum (--zip-with (abs (- it other)) rgb-1 rgb-2))))

(defun autothemer--find-closest-color (colors color)
  "Return the element of COLORS that is closest in rgb space to COLOR.
Here, COLOR is an Emacs color specification and COLORS is a list
of `autothemer--color' structs."
  (let ((min-distance 0)
        (closest-color nil))
    (mapc (lambda (candidate)
            (when (color-defined-p (autothemer--color-value candidate))
              (let ((distance (autothemer--color-distance color candidate)))
                (if (or (not closest-color) (< distance min-distance))
                    (setq closest-color candidate
                          min-distance distance)))))
          colors)
    closest-color))

(defun autothemer--unthemed-faces ()
  "Find uncustomized faces.
Iterate through all currently defined faces and return those that
were left uncustomized by the most recent call to
`autothemer-deftheme'."
  (let ((all-faces (face-list))
        (themed-faces (autothemer--theme-defined-faces autothemer--current-theme)))
    (--filter (not (-contains? themed-faces it)) all-faces)))

(defun autothemer--face-to-alist (face)
  "Return the attribute alist for FACE in frame (selected-frame)."
  (face-all-attributes face (selected-frame)))

(defun autothemer--cons-to-tree (the-cons)
  "Turn THE-CONS into a list, unless its cdr is `unspecified'."
  (let ((property-name (car the-cons))
        (property-value (cdr the-cons))
        (result))
    (unless (eq property-value 'unspecified)
      (setq result (list property-name property-value)))
    result))

(defun autothemer--alist-to-reduced-spec (facename alist)
  "Generate a reduced-spec for FACENAME, based on the face attribute ALIST."
  (list facename
        (--reduce-from (append acc it) nil
                       (mapcar 'autothemer--cons-to-tree
                               alist))))

(defun autothemer--approximate-spec (reduced-spec theme)
  "Replace colors in REDUCED-SPEC by their closest approximations in THEME.
Replace every expression in REDUCED-SPEC that passes
`color-defined-p' by the closest approximation found in
`autothemer--current-theme'.  Also quote all face names and
unbound symbols, such as `normal' or `demibold'."
  (let ((colors (autothemer--theme-colors theme))
        (face (car reduced-spec))
        (spec (cdr reduced-spec)))
    `(,face ,@(--tree-map (cond ((and (stringp it) (color-defined-p it))
                                 (autothemer--color-name
                                  (autothemer--find-closest-color colors it)))
                                ((stringp it) it)
                                ((numberp it) it)
                                ((facep it) `(quote ,it))
                                ((consp it) it)
                                ((not (boundp it)) `(quote ,it))
                                (t it))
                          spec))))

(defun autothemer--pad-with-nil (row min-number-of-elements)
  "Make sure that ROW has at least MIN-NUMBER-OF-ELEMENTS.
Pad with nil if necessary."
  (append
   row
   (-repeat
    (max 0
         (- min-number-of-elements
            (length row)))
    nil)))

(defun autothemer--replace-nil-by-precursor(palette-row)
  "Replace nil colors in PALETTE-ROW with their precursor.

PALETTE-ROW is of the form `(name color [color ...])'

Where  the first `color' must be non nil.

Any subsequent nil color will be replaced by the previous value.

For example:

     (\"red-foo\" \"#FF0000\" nil)

Will become:

     (\"red-foo\" \"#FF0000\" \"#FF0000\")"
  (cl-assert (car palette-row))
  (let* ((color-name (car palette-row))
         (color-definitions (cdr palette-row))
         (last-definition))
    (cons color-name
          (cl-loop for definition in color-definitions
                   do (when definition (setq last-definition definition))
                   collect last-definition))))

(defun autothemer--fill-empty-palette-slots (palette)
  "Fill empty PALETTE slots so each display has all color-definitions."
  (let ((n-displays (length (car palette))))
    (cons (car palette)
          (cl-loop for row in (cdr palette)
                   collect (autothemer--replace-nil-by-precursor
                            (autothemer--pad-with-nil row (1+ n-displays)))))))

(defun autothemer--extract-display (palette n)
  "Extract from PALETTE display specification #N."
  (elt (car palette) n))

(defun autothemer--extract-let-block (palette n)
  "Extract a variable definition block from PALETTE for display type N."
  (cl-loop for row in (cdr palette)
           collect (list (car row) (elt row (1+ n)))))

;;;###autoload
(defun autothemer-generate-templates-filtered (regexp)
  "Autogenerate customizations for unthemed faces matching REGEXP.

Calls `autothemer-generate-templates' after user provides REGEXP interactively."
  (interactive "sGenerate face templates matching regexp: ")
  (autothemer-generate-templates regexp))

;;;###autoload
(defun autothemer-generate-templates (&optional regexp)
  "Autogenerate customizations for unthemed faces (optionally by REGEXP).

Generate customizations that approximate current face definitions using the
nearest colors in the color palette of `autothemer--current-theme'.

An error is shown when no current theme is available."
  (interactive)
  (unless autothemer--current-theme
    (user-error "No autothemer--current-theme available. Please evaluate an autothemer-deftheme"))
  (let* ((missing-faces
          (if (null regexp)
              (autothemer--unthemed-faces)
            (--filter
              (string-match-p
               regexp
               (symbol-name it))
             (autothemer--unthemed-faces))))
         (templates
           (--map (autothemer--approximate-spec
                   (autothemer--alist-to-reduced-spec
                    it (autothemer--face-to-alist it))
                   autothemer--current-theme)
                  missing-faces))
         (buffer
          (get-buffer-create
           (generate-new-buffer-name "*Autothemer: unthemed faces*"))))
    (with-current-buffer buffer (emacs-lisp-mode) (insert (pp templates)))
    (switch-to-buffer buffer)))

(cl-defsubst autothemer--append-column (lists new-column)
  "If LISTS is nil, return NEW-COLUMN.
Otherwise, append NEW-COLUMN to every element of LISTS."
  (cl-assert (or (not lists) (eq (length lists) (length new-column))))
  (if lists (inline (-zip-with #'append lists new-column))
    new-column))

(defun autothemer--current-theme-guard ()
  "Guard functions from executing when there's no current theme."
  (unless autothemer--current-theme
    (user-error "No current theme available. Evaluate an autotheme definition")))

;;; Get colors from theme palette

(defun autothemer--get-color (color-name)
  "Return color palette object for (string) COLOR-NAME.

Search the `autothemer--current-theme' color palette for COLOR-NAME
and returns a color in the form of `autothemer--color' struct.

See also `autothemer--color-p',
         `autothemer--color-name',
         `autothemer--color-value'."
  (autothemer--current-theme-guard)
  (--find
   (eql (intern color-name)
        (autothemer--color-name it))
   (autothemer--theme-colors autothemer--current-theme)))

(defun autothemer--select-color (&optional prompt)
  "Select a color from the current palette, optionally use PROMPT.
Current palette is read from `autothemer--current-theme'.

The selected color will be in the form of a `autothemer--color'

See also `autothemer--color-p',
         `autothemer--color-name',
         `autothemer--color-value'."
  (autothemer--current-theme-guard)
  (let*
      ((selected
        (completing-read (if (null prompt)
                             "Select a color: "
                           prompt)
         (mapcar #'(lambda (it)
                     (let ((color (autothemer--color-value it))
                           (name (autothemer--color-name it)))
                      (format
                       "%s %s  %-45s"
                       (propertize "                                        "
                        'face (list ':background color
                                    ':foreground (readable-foreground-color color)))
                       (propertize color
                        'face (list ':background color
                                    ':foreground (readable-foreground-color color)))
                       name)))
          (autothemer--theme-colors autothemer--current-theme))))
       (color-name (cadr (split-string selected " " t " "))))
    (autothemer--get-color color-name)))

(defun autothemer-insert-color ()
  "Select and insert a color from the current autotheme palette."
  (interactive)
  (autothemer--current-theme-guard)
  (let ((color (autothemer--color-value
                 (autothemer--select-color "Insert a color: "))))
    (insert color)))

(defun autothemer-insert-color-name ()
  "Select and insert a color name from the current autotheme palette."
  (interactive)
  (autothemer--current-theme-guard)
  (let ((color-name (autothemer--color-name
                      (autothemer--select-color "Insert a color name: "))))
    (insert (format"%s" color-name))))

;;; Helper Functions

(defmacro autothemer--plist-bind (args plist &rest body)
  "Evaluate BODY with using ARGS to access PLIST values.

For example:

    (autothemer--plist-bind (a b c) '(:a 1 :b 2 :c 3) (list a b))
    => '(1 2)

If PLIST is nil, ARGS are bound to BODY nil values."
  `(if (listp ,plist)
       (cl-destructuring-bind (&key ,@args &allow-other-keys) ,plist ,@body)
     (let (,@args) ,@body)))

(defun autothemer--unindent (s)
  "Unindent string S marked with | chars."
  (replace-regexp-in-string "^ *|" "" s))

;;; let palette...
(defmacro autothemer-let-palette (&rest body)
  "Provide a let block for BODY from `autothemer--current-theme'.

Load/eval the required autothemer theme source (not
byte-compiled) to set `autothemer--current-theme'."
  (autothemer--current-theme-guard)
  `(let ,(--map (list (autothemer--color-name it) (autothemer--color-value it))
                (autothemer--theme-colors autothemer--current-theme))
     ,@body))

;;; Colorize alist for rainbow-mode
(defun autothemer-colorize-alist ()
  "Generate an alist for use with rainbow-mode.

To colorize use:

    (rainbow-colorize-by-assoc (autothemer-colorize-alist))

Colors are from `autothemer--current-theme'."
  (autothemer--current-theme-guard)
  (--map (cons (format "%s" (autothemer--color-name it))
               (autothemer--color-value it))
    (autothemer--theme-colors autothemer--current-theme)))

(defvar autothemer--colors-font-lock-keywords nil)

(defun autothemer-colorize ()
  "Colorize using rainbow-mode."
  (interactive)
  (setq autothemer--colors-font-lock-keywords
      `((,(regexp-opt (mapcar 'car (autothemer-colorize-alist)) 'words)
         (0 (rainbow-colorize-by-assoc (autothemer-colorize-alist))))))
  (font-lock-add-keywords nil autothemer--colors-font-lock-keywords t))

(defun autothemer--color-to-hsv (rgb)
  "Convert RGB, a list of `(r g b)' to list `(h s v)'.
The `r' `g' `b' values can range between `0..65535'.

In `(h s v)' `h', `s' and `v' are `0.0..1.0'."
  (cl-destructuring-bind
      (r g b) rgb
    (let*
        ((bri (max r g b))
         (delta (- bri (min r g b)))
         (sat (if (cl-plusp bri)
                  (/ delta bri)
                0.0))
         (normalize #'(lambda
                        (constant right left)
                        (let ((hue (+ constant (/ (* 60.0 (- right left)) delta))))
                          (if (cl-minusp hue)
                              (+ hue 360.0)
                            hue)))))
      (list (/ (cond
                ((zerop sat) 0.0)
                ((= r bri) (funcall normalize 0.0 g b)) ; dominant r
                ((= g bri) (funcall normalize 120.0 b r)) ; dominant g
                (t (funcall normalize 240.0 r g))) ; dominant b
               360.0)
            sat
            bri))))

(defun autothemer-hex-to-rgb (hex)
  "Fast convert HEX to `(r g b)'.
(Perf equal to wx color values C function.)
`r', `g', `b' will be values `0..65535'"
  (let ((rgb (string-to-number (substring hex 1) 16)))
    (list
     (* #x101 (ash (logand #xFF0000 rgb) -16))
     (* #x101 (ash (logand #xFF00 rgb) -8))
     (* #x101 (logand #xFF rgb)))))

(defun autothemer-color-hue (hex-color)
  "Return the HSV hue of HEX-COLOR."
  (car (autothemer--color-to-hsv (autothemer-hex-to-rgb hex-color))))

(defun autothemer-color-sat (hex-color)
  "Return the HSV sat of HEX-COLOR."
  (cadr (autothemer--color-to-hsv (autothemer-hex-to-rgb hex-color))))

(defun autothemer-color-brightness (hex-color)
  "Return the HSV brightness of HEX-COLOR."
  (caddr (autothemer--color-to-hsv (autothemer-hex-to-rgb hex-color))))

(defun autothemer-darkest-order (a b)
  "Return t if the darkness of A > B."
  (let ((a (autothemer-color-brightness (autothemer--color-value a)))
        (b (autothemer-color-brightness (autothemer--color-value b))))
    (> b a)))

(defun autothemer-lightest-order (a b)
  "Return t if the lightness of A > B."
  (let ((a (autothemer-color-brightness (autothemer--color-value a)))
        (b (autothemer-color-brightness (autothemer--color-value b))))
      (> a b)))

(defun autothemer-saturated-order (a b)
  "Return t if the saturation of A > B."
  (let ((a (autothemer-color-sat (autothemer--color-value a)))
        (b (autothemer-color-sat (autothemer--color-value b))))
      (> a b)))

(defun autothemer-hue-order (a b)
  "Return t if the hue of A > B."
  (let ((a (autothemer-color-hue (autothemer--color-value a)))
        (b (autothemer-color-hue (autothemer--color-value b))))
      (> a b)))

(defun autothemer-hue-sat-order (a b)
  "Return t if the hue and sat of A > B."
  (let ((a-hue (autothemer-color-hue (autothemer--color-value a)))
        (b-hue (autothemer-color-hue (autothemer--color-value b)))
        (a-sat (autothemer-color-sat (autothemer--color-value a)))
        (b-sat (autothemer-color-sat (autothemer--color-value b)))
        (sort-hash-fmt "%016s-%016s"))
    (string> (format sort-hash-fmt a-hue a-sat)
             (format sort-hash-fmt b-hue b-sat))))

(defun autothemer-sort-palette (theme-colors &optional fn)
  "Produce a list of sorted THEME-COLORS using FN.

If FN is nil, sort by default FN `autothemer-darkest-order'.

`autothemer-lightest-order' is available to balance the force.

There are also `autothemer-hue-order' and `autothemer-saturated-order'"
  (let ((fn (or fn 'autothemer-darkest-order)))
     (-sort fn theme-colors)))

;;; SVG Palette generator...

(defun autothemer-generate-palette-svg (&optional options)
  "Create an SVG palette image for a theme.

Optionally supply a plist of OPTIONS (all keys are optional, 
required values will default or prompt interactively.):

    :theme-file - theme filename
    :theme-name - override the title found in :theme-file
    :theme-description - override the description found in :theme-file
    :theme-url - override the url found in :theme-file
    :swatch-width - px spacing width of a color swatch (default: 100)
    :swatch-height - px spacing height of a color swatch (default: 150)
    :swatch-rotate - degrees of rotation for swatch (default: 45)
    :columns - number of columns for each palette row (default: 6)
    :page-template - see page-template below
    :page-top-margin - (default 120)
    :page-right-margin - (default 30)
    :page-bottom-margin - (default 60)
    :page-left-margin - (default 30)
    :h-space - (default 10)
    :v-space - (default 10)
    :swatch-template - see swatch-template below
    :font-family - font name to use in the generated SVG
    :bg-color
    :text-color
    :text-accent-color
    :swatch-border-color
    :sort-palette
    :svg-out-file

For advanced customization the :page-template and :swatch-template can be
used to provide customize the SVG templates.

Note: Template parameters are filled by `format' so we mark them as follows:

Page Template parameters:

    %1$s  - width
    %2$s  - height
    %3$s  - font-family
    %4$s  - text-color
    %5$s  - text-accent-color
    %6$s  - bg-color
    %7$s  - theme-name
    %8$s  - theme-description
    %9$s  - theme-url
    %10$s - color swatches

Swatch Template parameters:

    %1$s - x
    %2$s - y
    %3$s - swatch-border-color
    %4$s - swatch-color
    %5$s - text-accent-color
    %6$s - swatch-color-name"
  (interactive)
  (autothemer--plist-bind
    (theme-file
     theme-name
     theme-description
     theme-url

     sort-palette
     swatch-width
     swatch-height
     swatch-rotate
     columns

     page-top-margin
     page-right-margin
     page-bottom-margin
     page-left-margin

     page-template
     swatch-template

     font-family

     bg-color
     text-color
     text-accent-color
     swatch-border-color
     h-space
     v-space
     svg-out-file)
    options
   (let ((theme-file (or theme-file (read-file-name "Select autothemer theme .el file: "))))
     (load-file theme-file) ;; make it the current-theme
     (let* ((page-template
             (or page-template
              (autothemer--unindent "<?xml version=\"1.0\" standalone=\"no\"?>
                         |<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
                         |\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">
                         |<svg width=\"%1$spx\" height=\"%2$spx\"
                         |     version=\"1.1\"
                         |     xmlns=\"http://www.w3.org/2000/svg\"
                         |     xmlns:xlink=\"http://www.w3.org/1999/xlink\">
                         |  <style>
                         |    text {
                         |    font-family: \"%3$s\";
                         |    fill: %4$s;
                         |    }
                         |  </style>
                         |  <rect x=\"0\" y=\"0\" rx=\"10\" width=\"%1$spx\" height=\"%2$spx\" id=\"background-panel\" fill=\"%6$s\"/>
                         |  <g transform=\"translate(14,10)\">
                         |    <a xlink:href=\"%9$s\">
                         |    <text style=\"font-size:42pt;\" font-weight=\"bold\" x=\"3%%\" y=\"50\" id=\"theme-name\">%7$s</text>
                         |    <text style=\"font-size:12pt;\" x=\"4%%\" y=\"75\" id=\"theme-description\">%8$s</text>
                         |    <text style=\"font-size:8pt;fill: %5$s\" text-anchor=\"end\" x=\"95%%\" y=\"20\" id=\"theme-url\">%9$s</text>
                         |    </a>
                         |  </g>
                         |  <g transform=\"translate(70,-40)\">
                         |    %10$s
                         |  </g>
                         |</svg>
                         |")))

            (swatch-template
             (or swatch-template
              (autothemer--unindent "<g transform=\"translate(%1$s,%2$s),rotate(%9$s)\">
                         | <ellipse cx=\"70\" cy=\"70\" rx=\"45\" ry=\"45\" id=\"background-color\" fill=\"%3$s\"/>
                         | <ellipse cx=\"70\" cy=\"70\" rx=\"42\" ry=\"42\" id=\"color\" fill=\"%4$s\"/>
                         | <text style=\"font-size:7pt\" font-weight=\"bold\" x=\"52\" y=\"125\" id=\"color-name\">%6$s</text>
                         | <text style=\"font-size:7pt; fill:%5$s;\" font-weight=\"bold\" x=\"52\" y=\"134\" id=\"color\">%4$s</text>
                         | <!-- Rect below is for debug set stroke width to be visible -->
                         |   <rect x=\"0\" y=\"0\" width=\"%7$spx\" height=\"%8$spx\" class=\"debug-rect\" fill-opacity=\"0.0\" stroke-width=\"0.0mm\" stroke=\"#FF8000\"/>
                         |</g>
                         |")))

            (autotheme-name (autothemer--theme-name autothemer--current-theme))
            (colors (autothemer--theme-colors autothemer--current-theme))
            (theme-name        (or theme-name (autothemer--theme-name autothemer--current-theme)))
            (theme-description (or theme-description (autothemer--theme-description autothemer--current-theme)))
            (theme-url         (or theme-url (lm-homepage theme-file) (read-string "Enter theme URL: " "https://github.com/")))

            (font-family        (or font-family        (read-string "Font family name: " "Helvetica Neue")))
            (swatch-width       (or swatch-width       (read-number "Swatch width: " 100)))
            (swatch-height      (or swatch-height      (read-number "Swatch height: " 150)))
            (swatch-rotate      (or swatch-rotate      (read-number "Swatch rotate: " 45)))
            (columns            (or columns            (read-number "Number or columns: " 6)))
            (page-top-margin    (or page-top-margin    (read-number "Page Top margin: " 120)))
            (page-bottom-margin (or page-bottom-margin (read-number "Page Bottom margin: " 60)))
            (page-left-margin   (or page-left-margin   (read-number "Page Left margin: " 30)))
            (page-right-margin  (or page-right-margin  (read-number "Page Right margin: " 30)))
            (h-space            (or h-space            (read-number "Swatch horiztonal spacing: " 10)))
            (v-space            (or v-space            (read-number "Swatch vertical spacing: " 10)))

            (rows (/ (length colors) columns))
            (width (+ page-right-margin page-left-margin
                      (* h-space columns)
                      (* swatch-width columns)))
            (height (+ page-top-margin page-bottom-margin
                       (* v-space rows)
                       (* swatch-height (+ 1 rows))))

            (background-color    (or bg-color            (autothemer--color-value (autothemer--select-color "Select Background color: "))))
            (text-color          (or text-color          (autothemer--color-value (autothemer--select-color "Select Text color: "))))
            (text-accent-color   (or text-accent-color   (autothemer--color-value (autothemer--select-color "Select Text accent color: "))))
            (swatch-border-color (or swatch-border-color (autothemer--color-value (autothemer--select-color "Select swatch border color: "))))

            (svg-out-file (or svg-out-file (read-file-name (format "Enter a Filename to save SVG palette for %s." theme-name))))
            (svg-swatches (string-join
                            (-map-indexed
                               (lambda (index it)
                                   (let ((color (autothemer--color-value it))
                                         (name  (upcase
                                                 (replace-regexp-in-string
                                                  (concat autotheme-name "-") ""
                                                  (format "%s" (autothemer--color-name it)))))
                                         (x (+ page-left-margin (* (+ h-space swatch-width) (% index columns))))
                                         (y (+ page-top-margin (* (+ v-space swatch-height) (/ index columns)))))
                                     (format swatch-template
                                             x
                                             y
                                             swatch-border-color
                                             color
                                             text-accent-color
                                             name swatch-width swatch-height swatch-rotate)))
                             (if sort-palette
                                 (if (eql t sort-palette)
                                     (autothemer-sort-palette colors)
                                   (autothemer-sort-palette colors (intern sort-palette)))
                                 colors))
                            "\n")))
       (with-temp-file svg-out-file
         (insert
          (format page-template
                  width
                  height
                  font-family
                  text-color
                  text-accent-color
                  background-color
                  theme-name
                  theme-description
                  theme-url
                  svg-swatches)))
      (message "%s generated." svg-out-file)))))

(provide 'autothemer)
;;; autothemer.el ends here
