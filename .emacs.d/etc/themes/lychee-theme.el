;; lychee-theme

;; [[file:../../notebook/config/emacs/themes/lychee-theme.org::*lychee-theme][lychee-theme:1]]
;;; lychee-theme.el --- chee's light emacs 24_ theme.
;;; Commentary:
;; version 1
;; keywords: deftheme theme
;; author: chee <yay@chee.party>
;; url: chee.party/lychee-theme.el
;;; Code:

(deftheme lychee "chee -- i love you and colours")
(defvar lychee:colors
       '(("background" . "#ffffff")
	      ("foreground" . "#000000")
	      ("hot"        . "#ec6ca7")
	      ("subtle"     . "#8899aa")
	      ("subtle2"    . "#334455")
	      ("mint"       . "#205B46")
	      ("lymint"     . "#d5f6ea")
	      ("uhoh"       . "#F12D00")
	      ("wishy"      . "#333333")
	      ("function"   . "#DB4E80")
	      ("type"       . "#0CADD6")
	      ("variable"   . "#E85B3A")
	      ("myeyes"     . "#DB4E80")
	      ("brush"      . "#6F3A1D")
	      ("lone"       . "#ccddff")
	      ("sky"        . "#3999FF"))
       "Colours for lychee-theme!")
;; thanks to bbatsov for this macro
(defmacro lychee:with-colors (&rest body)
       "Let's bind our BODY colors!"
       (declare (indent 0))
       `(let ((class '((class color) (min-colors 89)))
	      ,@(mapcar (lambda (cons)
			       (list (intern (car cons)) (cdr cons)))
			lychee:colors))
	       ,@body))
(lychee:with-colors
       (custom-theme-set-faces
	'lychee
	`(default
		((t (:foreground ,foreground
				      :background ,background))))

	`(cursor
	       ((t (:foreground "white" :background ,hot))))

	`(font-lock-builtin-face
	       ((t (:foreground ,hot))))

	`(font-lock-comment-delimiter-face
	       ((t (:foreground ,subtle2))))

	`(font-lock-comment-face
	       ((t (:foreground ,subtle
				:slant       italic))))

	`(font-lock-constant-face
	       ((t (:foreground ,wishy))))

	`(font-lock-doc-face
	       ((t (:foreground ,wishy
				:slant       italic))))

	`(font-lock-function-name-face
	       ((t (:foreground ,function))))

	`(font-lock-keyword-face
	       ((t (:foreground ,myeyes))))

	`(font-lock-negation-char-face
	       ((t (:foreground ,uhoh))))

	`(font-lock-preprocessor-face
	       ((t (:foreground ,sky))))

	`(font-lock-string-face
	       ((t (:foreground ,mint
				:background ,lymint))))

	`(font-lock-type-face
	       ((t (:foreground ,type))))

	`(font-lock-variable-name-face
	       ((t (:foreground ,variable))))

	`(font-lock-warning-face
	       ((t (:foreground ,uhoh))))

	`(fringe
	       ((t (:background ,background))))

	`(mode-line
	       ((t (:inherit    'fringe
				:foreground ,subtle2
				:background ,lymint))))

	'(mode-line-inactive
	       ((t (:inherit    'mode-line
				:slant       italic
				:box         nil
				:background  "#dff6ee"))))

	`(mode-line-highlight
	       ((t (:foreground "#fff"
				:background "#00f"
				:reverse t))))

	`(mode-line-buffer-id
	       ((t (:foreground ,hot))))

	`(show-paren-match
	       ((t (:background ,foreground
				:foreground ,background))))

	'(show-paren-mismatch
	       ((t (:background "#FA2573"))))

	`(minibuffer-prompt
	       ((t (:foreground ,hot))))

	`(highlight
	       ((t (:background "#FFFFEE"))))

	`(hl-line
	       ((t (:background ,subtle))))

	`(linum
	       ((t (:inherit    (shadow default background)
				:foreground ,subtle2
				:slant      oblique
				:height     0.94))))

	`(region
	       ((t (:background "#EEEEFF"))))

	'(trailing-whitespace
	       ((t (:background "#571C0E"
				:foreground "#331C10" ))))


	;;
	;; lsp
	;;

	`(lsp-ui-doc-background
	       ((t (:background: "#ffffee"))))


	;;
	;; ISearch
	;;
	`(isearch
	       ((t (:background ,foreground
				:box         "white"))))

	`(isearch-fail
	       ((t (:background "#382323"
				:foreground ,uhoh))))

	;;
	;; Twittering mode
	;;
	'(twittering-uri-face
	       ((t (:foreground "#729FCF"))))

	'(twittering-username-face
	       ((t (:foreground "#FC951E"))))

	;;
	;; Usual UI things
	;;
	`(widget-field
	       ((t (:background ,foreground
				:foreground "white"))))

	'(custom-group-tag-face
	       ((t (:foreground "#67D9F0"
				:height      1.2))))

	'(custom-variable-tag-face
	       ((t (:foreground "#729FCF"))))

	'(custom-state-face
	       ((t (:foreground "#A6E32D"))))

	'(link
	       ((t (:foreground "#729FCF"
				:underline  nil))))

	;;
	;; Diff
	;;
	'(diff-added
	       ((t (:foreground "#00cc66"))))

	'(diff-removed
	       ((t (:foreground "#ff0099"))))

	'(ediff-odd-diff-C
	       ((t (:background "#A6E32D"))))

	'(diff-changed
	       ((t (:foreground "#67D9F0"))))

	'(ediff-even-diff-C
	       ((t (:background "#FA2573"))))

	'(diff-header
	       ((t (:background "white" :foreground "#333"))))

	'(diff-file-header
	       ((t (:background "white" :foreground "#333"))))

	'(diff-context
	       ((t (:foreground "#555"))))


	;;
	;; Whitespace mode
	;;
	`(whitespace-indentation
	       ((t (:background ,background
				:foreground "#82996A"))))

	`(whitespace-line
	       ((t (:background ,background
				:foreground "#7A6D89"))))

	`(whitespace-newline
	       ((t (:foreground ,background
				:weight      normal))))

	`(whitespace-space
	       ((t (:background ,background
				:foreground ,background))))

	`(whitespace-tab
	       ((t (:background ,background
				:foreground ,background))))

	'(whitespace-space-after-tab
	       ((t (:background "#303636"
				:foreground "#82996A"))))

	'(whitespace-space-before-tab
	       ((t (:background "#382323"
				:foreground "#82996A"))))

	'(whitespace-trailing
	       ((t (:inherit 'trailing-whitespace))))

	'(whitespace-empty
	       ((t (:background "#382323"
				:foreground "#624935"))))

	'(whitespace-hspace
	       ((t (:background "#382323"
				:foreground "#82996A"))))

	`(tab-line
	       ((t (:inherit variable-pitch
			:height 1.0
			:background "#ffffee"
			:foreground "#000000"))))
	`(tab-line-tab
	       ((t (
	       :background "#ffffff"
	       :foreground "#662244"))))

	`(tab-line-tab-current
	       ((t (
	       :box (:line-width 10 :color "white")
	       :background "white"
	       :foreground "black"))))

	`(tab-line-tab-inactive
	       ((t (
	       :box (:line-width 10 :color "#ffffee")
	       :slant italic
	       :background "#ffffee"
	       :foreground "#662244"))))

	;;
	;; Flyspell stuff
	;;
	'(flyspell-duplicate
	       ((t (:background "#382323"
				:underline  "#FC951E"))))

	'(flyspell-incorrect
	       ((t (:background "#382323"
				:underline "#E52222"))))

	;;
	;; ERC
	;;
	'(erc-notice-face
	       ((t (:foreground "#75766A"))))

	'(erc-current-nick-face
	       ((t (:foreground "#FA2573"))))

	'(erc-input-face
	       ((t (:foreground "#ABB4A1"))))

	'(erc-nick-default-face
	       ((t (:foreground "#729FCF"))))

	'(erc-prompt-face
	       ((t (:foreground "#FC951E"
				:background nil))))

	'(erc-timestamp-face
	       ((t (:foreground "#75766A"))))

	;;
	;; ReStructuredText
	;;
	'(rst-level-1-face
	       ((t (:foreground "#729FCF"
				:background nil))))

	'(rst-level-2-face
	       ((t (:inherit 'rst-level-1-face))))

	'(rst-level-3-face
	       ((t (:inherit 'rst-level-2-face))))

	'(rst-level-4-face
	       ((t (:inherit 'rst-level-3-face))))

	'(rst-level-5-face
	       ((t (:inherit 'rst-level-4-face))))

	'(rst-level-6-face
	       ((t (:inherit 'rst-level-5-face))))

	;; org/outline
	'(outline-1
	       ((t (:foreground "#033B65"))))
	'(outline-2
	       ((t (:foreground "#225b86"))))
	'(outline-3
	       ((t (:foreground "#427ca8"))))
	'(outline-4
	       ((t (:foreground "#6f9dc7"))))
	'(outline-5
	       ((t (:foreground "#8fbfef"))))
	'(outline-6
	       ((t (:foreground "#6090c0"))))
	'(outline-7
	       ((t (:foreground "#427ba6"))))
	'(outline-8
	       ((t (:foreground "#235b85"))))
	`(org-tag
	       ((t (:inherit fixed-width :weight bold :background ,lymint :foreground "#000000"))))
	'(org-block ((t (:background "#f3fcff"))))
	'(org-code ((t (:background "#caeaf7" :inherit fixed-width))))
	'(org-hide
	       ((t (:foreground "#303636"))))
	`(org-headline-done
	       ((t (:foreground ,subtle :strike-through "#000000"))))
	`(org-done
	       ((t (:weight bold :foreground ,lone))))
	`(org-link
	       ((t (:foreground "#33ccff"))))
	`(org-todo
	       ((t (:weight bold :foreground ,sky))))
	`(org-verbatim
	       ((t (:inherit fixed-width :background "#f7f7f7" :foreground ,function))))
	'(org-special-keyword
	       ((t (:inherit 'font-lock-type-face))))

	;;
	;; Perspective mode
	;;
	'(persp-selected-face
	       ((t (:foreground "#eeeeee"
				:background "#382323"
				:box        "#382323"))))

	`(company-tooltip
	       ((t (
	       :foreground "#000000"))))

	`(company-tooltip-selection
	       ((t (
	       :foreground "#000000"
	       :background ,myeyes))))
	;;
	;; Yasnippet
	;;
	'(yas/field-highlight-face
	       ((t (:background "#586045"
				:box "#ACAE95"))))))
;; lychee-theme:1 ends here
