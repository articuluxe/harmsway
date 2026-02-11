;;; jblow-nostalgia-theme.el --- The jblow-nostalgia color theme

;; Author: Ivan Prikaznov <prikaznov555@gmail.com>
;; Filename: jblow-nostalgia-theme.el
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/Prikaz98/jblow-nostalgia
;; License: UNLICENCE

;;; Commentary:

;; Dark theme that was stole from John Blow old live streams.

;;; Code:

(unless (>= emacs-major-version 24)
  (error "The jblow-nostalgia theme requires Emacs 24 or later!"))

(deftheme jblow-nostalgia "The jblow-nostalgia color theme")

;; Monokai colors
(defcustom jblow-nostalgia-theme-yellow "#EFFF18" "Primary colors - yellow" :type 'string :group 'monokai)
(defcustom jblow-nostalgia-theme-orange "#FD971F" "Primary colors - orange" :type 'string :group 'monokai)
(defcustom jblow-nostalgia-theme-red "#F92672" "Primary colors - red" :type 'string :group 'monokai)
(defcustom jblow-nostalgia-theme-magenta "#FD5FF0" "Primary colors - magenta" :type 'string :group 'monokai)
(defcustom jblow-nostalgia-theme-blue "#7FEFD1" "Primary colors - blue" :type 'string :group 'monokai)
(defcustom jblow-nostalgia-theme-green "#A9ECB6" "Primary colors - green" :type 'string :group 'monokai)
(defcustom jblow-nostalgia-theme-cyan "#A1EFE4" "Primary colors - cyan" :type 'string :group 'monokai)
(defcustom jblow-nostalgia-theme-violet "#AE81FF" "Primary colors - violet" :type 'string :group 'monokai)

(let ((background      "#292929")
      (gutters         "#292929")
      (gutter-fg       "#292929")
      (gutters-active  "#292929")
      (builtin         "#FFFFFF")
      (selection       "#0000FF")
      (text            "#D3B58D")
      (comments        "#FFFF00")
      (punctuation     "#D3B58D")
      (type            "#98FB98")
      (keywords        "#FFFFFF")
      (variables       "#BFC9DB")
      (functions       "#ffffff")
      (methods         "#BFC9DB")
      (strings         "#BEBEBE")
      (constants       "#BFC9DB")
      (macros          "#E67D74")
      (numbers         "#D699B5")
      (white           "#FFFFFF")
      (error           "#ff0000")
      (warning         "#E4D97D")
      (highlight-line  "#D89B75")
      (line-fg         "#126367"))

  (custom-theme-set-faces
   'jblow-nostalgia

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background, :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,white                        ))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords))))
   `(font-lock-type-face              ((t (:foreground ,type))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-property-use-face      ((t (:foreground ,text))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-variable-use-face      ((t (:foreground ,text))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-doc-face               ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   `(font-lock-function-call-face     ((t (:foreground ,text))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   `(font-lock-warning-face           ((t (:foreground ,warning))))

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,warning))))
   `(whitespace-trailing ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum ((t (:foreground ,line-fg :background ,background))))
   `(linum-relative-current-face ((t (:foreground ,white :background ,background))))
   `(line-number ((t (:foreground ,line-fg :background ,background))))
   `(line-number-current-line ((t (:foreground ,white :background ,background))))

   ;; compilation
   `(compilation-info ((t ,(list :foreground jblow-nostalgia-theme-green
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground jblow-nostalgia-theme-yellow
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground, error))))
   `(compilation-mode-line-fail ((t ,(list :foreground error
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground jblow-nostalgia-theme-green
                                           :weight 'bold
                                           :inherit 'unspecified))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,jblow-nostalgia-theme-violet))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,jblow-nostalgia-theme-blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,jblow-nostalgia-theme-green))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,jblow-nostalgia-theme-yellow))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,jblow-nostalgia-theme-orange))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,jblow-nostalgia-theme-red))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,jblow-nostalgia-theme-violet))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,jblow-nostalgia-theme-blue))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,jblow-nostalgia-theme-green))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,jblow-nostalgia-theme-yellow))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,jblow-nostalgia-theme-orange))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,jblow-nostalgia-theme-red))))

   ;; which-func
   `(which-func ((t (:inverse-video unspecified
                                    :underline unspecified
                                    :foreground ,background
                                    :weight bold
                                    :box nil))))

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,background :distant-foreground ,text :text ,text :weight bold))))
   `(mode-line ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,background
                                   :background ,text
                                   :box nil))))
   `(powerline-active1 ((t (:background ,text :foreground ,background))))
   `(powerline-active2 ((t (:background ,text :foreground ,background))))

   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,text
                                            :background ,background
                                            :box nil))))
   `(powerline-inactive1 ((t (:background ,background :foreground ,text))))
   `(powerline-inactive2 ((t (:background ,background :foreground ,text))))

    ;; better compatibility with default DOOM mode-line
   `(error ((t (:foreground nil :weight normal))))
   `(doom-modeline-project-dir ((t (:foreground nil :weight bold))))
   
   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,text))))
   `(js2-jsdoc-tag ((t (:foreground ,keywords))))
   `(js2-jsdoc-type ((t (:foreground ,constants))))
   `(js2-jsdoc-value((t (:foreground ,text))))
   `(js2-object-property ((t (:foreground ,text))))
   `(js2-external-variable ((t (:foreground ,constants))))
   `(js2-error ((t (:foreground ,error))))
   `(js2-warning ((t (:foreground ,warning))))

   ;; highlight numbers
   `(highlight-numbers-number ((t (:foreground ,numbers))))

   ;; tab-bar-mode
   `(tab-bar ((t (:inherit modeline))))
   `(tab-bar-tab ((t (:foreground ,background :background ,text))))
   `(tab-bar-tab-inactive ((t (:foreground ,text :background ,background))))
  )

  (custom-theme-set-variables
    'jblow-nostalgia
    '(linum-format " %5i ")
  )
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************

(provide-theme 'jblow-nostalgia)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'jblow-nostalgia-theme)

;;; jblow-nostalgia-theme.el ends here
