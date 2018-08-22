;;; solarized-emacsy-theme.el

;; Copyright (c) 2013 Antonio Nikishaev <a@lelf.me>
;; Licensed under MIT license.

;; Description: solarized theme that uses colours similar to standard
;; Emacs ones, therefore suck less.  Created after frustration with
;; other solarized themes.

;; This is uncomplete and only for light background.

;; Solarized colour definitions.
;; (Stolen from https://github.com/altercation/solarized)
(defvar solarized-emacsy-colors
  ;; name     sRGB      Gen RGB   256       16              8
  '((base03   "#002b36"  "#042028"  "#1c1c1c" "brightblack"   "black")
    (base02   "#073642"  "#0a2832"  "#262626" "black"         "black")
    (base01   "#586e75"  "#465a61"  "#585858" "brightgreen"   "green")
    (base00   "#657b83"  "#52676f"  "#626262" "brightyellow"  "yellow")
    (base0    "#839496"  "#708183"  "#808080" "brightblue"    "blue")
    (base1    "#93a1a1"  "#81908f"  "#8a8a8a" "brightcyan"    "cyan")
    (base2    "#eee8d5"  "#e9e2cb"  "#e4e4e4" "white"         "white")
    (base3    "#fdf6e3"  "#fcf4dc"  "#ffffd7" "brightwhite"   "white")
    (yellow   "#b58900"  "#a57705"  "#af8700" "yellow"        "yellow")
    (orange   "#cb4b16"  "#bd3612"  "#d75f00" "brightred"     "red")
    (red      "#dc322f"  "#c60007"  "#d70000" "red"           "red")
    (magenta  "#d33682"  "#c61b6e"  "#af005f" "magenta"       "magenta")
    (violet   "#6c71c4"  "#5859b7"  "#5f5faf" "brightmagenta" "magenta")
    (blue     "#268bd2"  "#2075c7"  "#0087ff" "blue"          "blue")
    (cyan     "#2aa198"  "#259185"  "#00afaf" "cyan"          "cyan")
    (green    "#859900"  "#728a05"  "#5f8700" "green"         "green")))


(require 'cl-macs)


(defun solarized-srgb-of (colour)
  (car (alist-get colour solarized-emacsy-colors)))





(defun solarized-emacsy/make-defs (arg)
  (let ((defs arg))
    (mapcar (lambda (def)
	      (let ((face (car def))
		    (value (mapcar (lambda (s) (or (solarized-srgb-of s) s))
				   (cdr def))))
		`(,face ((t ,@value)))))
	    defs)))


(defun solarized- (a)
  (intern (concat
	   "solarized-"
	   (symbol-name (nth 0 def)))))

(defconst solarized-emacsy-std-defs
  (mapcar
   (lambda (def)
     (list (solarized- (nth 0 def))
	     :foreground
	     (nth 1 def)))
   solarized-emacsy-colors)
  )


(defun solarized-emacsy/make-faces ()
  (mapc
   (lambda (def)
     (let ((name (car def))
	   (plist (cdr def)))
       (eval
	`(defface ,name
	   '((default . ,plist))
	   (symbol-name ',name)
	   )
	)
       )
     )
   solarized-emacsy-std-defs)
  )



(defconst solarized-emacsy-faces-defs
  '(
    (default :foreground base01)

    (font-lock-comment-face		:inherit solarized-red)
    (font-lock-string-face		:inherit solarized-orange)
    (font-lock-doc-face			:inherit font-lock-comment-face
					:underline "#CBA492")
    (font-lock-function-name-face	:inherit solarized-blue)
    (font-lock-variable-name-face	:inherit solarized-yellow)
    (font-lock-type-face		:foreground cyan)
    (font-lock-builtin-face		:foreground yellow)
    (font-lock-keyword-face		:foreground magenta)
    (font-lock-constant-face		:foreground green)
    ;;(font-lock-preprocessor-face	:foreground yellow)

    (warning	:foreground yellow)
    (error	:foreground magenta)
    (success    :inherit solarized-cyan)
    
    (gnus-cite-1		:foreground violet)
    (gnus-cite-2		:foreground red)
    (gnus-group-news-2		:foreground cyan :weight normal)
    (gnus-group-news-2-empty	:foreground cyan)

    (magit-branch-local		:inherit solarized-blue  :underline t)
    (magit-branch-remote	:inherit solarized-green :underline t)
    (magit-tag			:inherit solarized-cyan  :underline t)

    (agda2-highlight-keyword-face			:foreground yellow)
    (agda2-highlight-module-face			:foreground magenta)
    (agda2-highlight-primitive-type-face		:foreground violet)
    (agda2-highlight-postulate-face			:foreground violet)
    (agda2-highlight-datatype-face			:foreground violet)
    (agda2-highlight-record-face			:foreground violet)
    (agda2-highlight-function-face			:foreground blue)
    (agda2-highlight-symbol-face			:foreground base00)
    (agda2-highlight-inductive-constructor-face		:foreground cyan)
    (agda2-highlight-coinductive-constructor-face	:foreground yellow)
    (agda2-highlight-field-face				:foreground magenta)

    (scala-font-lock:var-face :inherit font-lock-variable-name-face)

    (custom-button :box (:line-width 1) :background base3)
    
    (tuareg-font-lock-operator-face	:foreground yellow)
    (tuareg-font-lock-governing-face	:foreground green)

    (proof-tactics-name-face	:foreground violet)
    (coq-solve-tactics-face	:foreground magenta)
    (coq-cheat-face		:background red :distant-foreground red :foreground base3)
    (proof-locked-face		:background base3)

    (mode-line			:background base00      :foreground base2)
    (mode-line-buffer-id	:inherit variable-pitch :foreground shadow) ;???
    (mode-line-inactive		:background base2       :foreground base0)
    (mode-line-highlight :inverse-video t)
    
    (region		:background base3)
    (fringe		:background base2 :foreground base00)
    (scroll-bar		:background base2 :foreground base00)
    (minibuffer		:foreground base03)
    (echo-area		:foreground base03)
    (shadow		:foreground base1)

    (tooltip :background "lightyellow" :inherit variable-pitch)
    
    (magit-item-highlight	:background base02)
    ))

(defvar solarized-emacsy/defs)
(setq solarized-emacsy/defs
      `(,@(solarized-emacsy/make-defs solarized-emacsy-faces-defs)
	,@(solarized-emacsy/make-defs solarized-emacsy-std-defs)))

(solarized-emacsy/make-faces)
(deftheme solarized-emacsy)

(apply 'custom-theme-set-faces 'solarized-emacsy solarized-emacsy/defs)

(provide-theme 'solarized-emacsy)

;;;; ???
(defun lelf/reload-theme ()
  (interactive)
  (apply 'custom-theme-set-faces 'solarized-emacsy
	 (solarized-emacsy/make-defs solarized-emacsy-faces-defs))
)
