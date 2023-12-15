;;; wildcharm-light-theme.el --- Port of vim-wildcharm (light) colorscheme -*- lexical-binding: t; -*-

;; Author: Maxim Kim <habamax@gmail.com>
;; URL: https://github.com/habamax/wildcharm-theme
;; Package-Requires: ((emacs "24.1"))
;; Package-Version: 0.7

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; High-contrast light Emacs theme.
;; - Port of the light background vim-wildcharm colorscheme.
;; - Should look mostly the same in GUI and TUI with 256 colors support.

;;; Code:

(deftheme wildcharm-light
  "High-contrast light Emacs theme."
  :family 'wildcharm)

(let* ((classTC '((class color) (min-colors 257)))
       (class256 '((class color) (min-colors 256)))
       (classTTY '((type tty)))
       (fg "#000000")(bg "#ffffff")
       (black "#000000")(darkgrey "#808080")(darkergrey "#5f5f5f")
       (red "#af0000")(bright-red "#d70000")
       (green "#008700")(bright-green "#5faf5f")
       (yellow "#af5f00")(bright-yellow "#d78700")
       (blue "#005faf")(bright-blue "#0087d7")
       (magenta "#870087")(bright-magenta "#af00af")
       (cyan "#005f5f")(bright-cyan "#008787")
       (grey "#d0d0d0")(white "#ffffff")
       (purple "#5f00d7")(comment "#87875f")
       (yellow1 "#875f00")(yellow2 "#af5f00")
       (grey1 "#dadada")(grey2 "#e4e4e4")(grey3 "#eeeeee")
       (non-text "#b2b2b2")
       (match-paren "#ff00af")(match "#ffd7ff")
       (mode-line-active "#d0d0d0")(mode-line-inactive "#e4e4e4")
       (menu "#eeeeee")
       (header-line "#d7d7d7")
       (hl-line "#eeeeee")
       (block "#eeeeee")
       (diff-added-bg "#afd7af")(diff-refine-added-bg "#d7ffd7")
       (diff-added-fg "#005f00")
       (diff-removed-bg "#d7afaf")(diff-refine-removed-bg "#ffd7d7")
       (diff-removed-fg "#5f0000")
       (diff-changed-bg "#d7d7af")(diff-refine-changed-bg "#ffffd7")
       (diff-changed-fg "#5f5f00")
       (diff-ancestor-bg "#afafd7")(diff-refine-ancestor-bg "#d7d7ff")
       (diff-ancestor-fg "#00005f")
       (outline-1 black)
       (outline-2 "#5f005f")
       (outline-3 "#000087")
       (outline-4 "#875f5f")
       (outline-5 "#005f5f")
       (outline-6 "#af875f")
       (outline-7 "#005f87")
       (outline-8 darkgrey)
       (hi-yellow "#ffffaf")
       (hi-pink "#ffafd7")
       (hi-blue "#afd7ff")
       (hi-green "#d7ffd7")
       (hi-salmon "#ffd7af")
       (hi-aquamarine "#d7ffff")
       (code-block "#f7f7f7"))

  (custom-theme-set-faces
   'wildcharm-light

   ;; standard faces
   `(default
     ((,class256 (:background ,bg :foreground ,fg))))
   `(shadow
     ((,class256 (:foreground ,non-text))))
   `(link
     ((,class256 (:foreground ,blue :underline t))))
   `(link-visited
     ((,class256 (:foreground ,magenta :underline t))))
   `(highlight
     ((,class256 (:background ,white :foreground ,blue :inverse-video t))))
   `(region
     ((,class256 (:background ,bg :foreground ,bright-blue :inverse-video t))))
   `(secondary-selection
     ((,class256 (:background ,bg :foreground ,bright-cyan :inverse-video t))))
   `(trailing-whitespace
     ((,class256 (:foreground ,bright-red :inverse-video t))))
   `(line-number
     ((,class256 (:inherit default :foreground ,non-text))))
   `(line-number-current-line
     ((,class256 (:inherit default :foreground ,yellow :weight bold))))
   `(line-number-major-tick
     ((,class256 (:inherit default :foreground ,darkgrey :weight bold))))
   `(line-number-minor-tick
     ((,class256 (:inherit default :foreground ,darkgrey))))
   `(escape-glyph
     ((,class256 (:foreground ,red))))
   `(homoglyph
     ((,class256 (:inherit 'escape-glyph))))
   `(nobreak-space
     ((,class256 (:inherit 'escape-glyph :underline t))))
   `(nobreak-hyphen
     ((,class256 (:inherit 'escape-glyph))))
   `(mode-line
     ((,class256 (:background ,mode-line-active :foreground ,fg
                              :box (:line-width 1 :color ,non-text)))))
   `(mode-line-inactive
     ((,class256 (:background ,mode-line-inactive :foreground ,darkgrey
                              :box (:line-width 1 :color ,mode-line-active)))))
   `(mode-line-highlight
     ((,class256 (:background ,bg
                              :box (:line-width 1 :color ,non-text)))))
   `(mode-line-emphasis
     ((,class256 (:weight bold))))
   `(mode-line-buffer-id
     ((,class256 (:weight bold))))
   `(header-line
     ((,class256 (:background ,header-line :foreground ,fg :extend t
                              :box (:line-width 1 :color ,darkgrey)))))
   `(vertical-border
     ((,classTTY (:background ,mode-line-inactive :foreground ,mode-line-inactive))
      (,class256 (:background ,non-text :foreground ,non-text))))
   `(window-divider
     ((t (:foreground ,mode-line-inactive))))
   `(window-divider-first-pixel
     ((t (:foreground ,mode-line-active))))
   `(window-divider-last-pixel
     ((t (:foreground ,mode-line-active))))
   `(minibuffer-prompt
     ((,class256 (:foreground ,yellow :weight bold))))
   `(fringe
     ((t (:foreground ,non-text :background unspecified))))
   `(separator-line
     ((t (:foreground ,non-text :underline t))))
   ;; -scroll-bar
   `(cursor
     ((t (:background ,fg))))
   ;; -tool-bar
   `(tab-bar
     ((,class256 (:background ,header-line :foreground ,fg))))
   `(tab-bar-tab
     ((,class256 (:background ,mode-line-inactive :foreground ,fg :weight bold
                              :box (:style released-button)))))
   `(tab-bar-tab-inactive
     ((,class256 (:background ,header-line :foreground ,darkergrey
                              :box (:style released-button)))))
   `(tab-line
     ((,class256 (:background ,mode-line-inactive :foreground ,darkgrey))))
   `(tab-line-tab
     ((,class256 (:background ,mode-line-active :foreground ,black :weight bold
                              :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-current
     ((,class256 (:background ,mode-line-active :foreground ,black :weight bold
                              :box (:line-width 1 :color ,non-text)))))
   `(tab-line-highlight
     ((,class256 (:background ,mode-line-active :foreground ,black :weight bold
                              :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-inactive
     ((t (:background unspecified :foreground ,fg
                      :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-modified
     ((t (:background unspecified :foreground ,green))))
   `(tab-line-close-highlight
     ((t (:background unspecified :foreground ,red))))
   `(help-key-binding
     ((,class256 (:background unspecified :foreground ,bright-cyan :weight bold))))
   `(error
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(warning
     ((,class256 (:foreground ,yellow :weight bold))))
   `(success
     ((,class256 (:foreground ,green :weight bold))))
   `(menu
     ((t (:background ,mode-line-inactive :foreground ,fg))))
   `(tty-menu-enabled-face
     ((t (:background ,menu :foreground ,fg))))
   `(tty-menu-disabled-face
     ((t (:background ,menu :foreground ,darkgrey))))
   `(tty-menu-selected-face
     ((t (:background ,mode-line-active :foreground ,fg :weight bold))))

   `(show-paren-match
     ((,class256 :foreground ,match-paren :weight bold)))
   `(show-paren-mismatch
     ((,class256 :background ,match-paren :foreground ,black :weight bold)))

   ;; ansi colors
   `(ansi-color-black
     ((t (:background ,black :foreground ,black))))
   `(ansi-color-bright-black
     ((t (:background ,darkgrey :foreground ,darkgrey))))
   `(ansi-color-red
     ((t (:background ,red :foreground ,red))))
   `(ansi-color-bright-red
     ((t (:background ,bright-red :foreground ,bright-red))))
   `(ansi-color-green
     ((t (:background ,green :foreground ,green))))
   `(ansi-color-bright-green
     ((t (:background ,bright-green :foreground ,bright-green))))
   `(ansi-color-yellow
     ((t (:background ,yellow :foreground ,yellow))))
   `(ansi-color-bright-yellow
     ((t (:background ,bright-yellow :foreground ,bright-yellow))))
   `(ansi-color-blue
     ((t (:background ,blue :foreground ,blue))))
   `(ansi-color-bright-blue
     ((t (:background ,bright-blue :foreground ,bright-blue))))
   `(ansi-color-magenta
     ((t (:background ,magenta :foreground ,magenta))))
   `(ansi-color-bright-magenta
     ((t (:background ,bright-magenta :foreground ,bright-magenta))))
   `(ansi-color-cyan
     ((t (:background ,cyan :foreground ,cyan))))
   `(ansi-color-bright-cyan
     ((t (:background ,bright-cyan :foreground ,bright-cyan))))
   `(ansi-color-white
     ((t (:background ,grey :foreground ,grey))))
   `(ansi-color-bright-white
     ((t (:background ,white :foreground ,white))))

   ;; font-lock
   `(font-lock-string-face
     ((,class256 (:foreground ,green :weight unspecified :slant unspecified))))
   `(font-lock-comment-face
     ((,class256 (:foreground ,comment :weight unspecified :slant unspecified))))
   `(font-lock-keyword-face
     ((,class256 (:foreground ,blue :weight unspecified :slant unspecified))))
   `(font-lock-preprocessor-face
     ((,class256 (:foreground ,purple :weight unspecified :slant unspecified))))
   `(font-lock-builtin-face
     ((,class256 (:foreground ,magenta :weight unspecified :slant unspecified))))
   `(font-lock-type-face
     ((,class256 (:foreground ,yellow :weight unspecified :slant unspecified))))
   `(font-lock-function-name-face
     ((,class256 (:foreground ,bright-magenta :weight unspecified :slant unspecified))))
   `(font-lock-variable-name-face
     ((,class256 (:foreground ,bright-cyan :weight unspecified :slant unspecified))))
   `(font-lock-constant-face
     ((,class256 (:foreground ,red :weight unspecified :slant unspecified))))
   `(font-lock-warning-face
     ((,class256 (:foreground ,bright-yellow :weight bold :slant unspecified))))

   `(font-lock-number-face
     ((,class256 (:foreground ,red :weight unspecified :slant unspecified))))
   `(font-lock-escape-face
     ((,class256 (:foreground ,yellow :weight unspecified :slant unspecified))))
   `(font-lock-function-call-face
     ((,class256 (:foreground ,magenta :weight unspecified :slant unspecified))))
   `(font-lock-regexp-face
     ((,class256 (:foreground ,bright-green :weight unspecified :slant unspecified))))
   `(font-lock-delimiter-face
     ((,class256 (:foreground ,magenta :weight unspecified :slant unspecified))))
   `(font-lock-property-use-face
     ((,class256 (:foreground ,cyan :weight unspecified :slant unspecified))))
   `(font-lock-bracket-face
     ((,class256 (:foreground ,purple :weight unspecified :slant unspecified))))

   `(elisp-shorthand-font-lock-face
     ((,class256 (:foreground ,bright-cyan :weight bold :slant unspecified))))

   `(perl-non-scalar-variable
     ((,class256 (:inherit font-lock-variable-name-face))))

   `(cperl-nonoverridable-face
     ((,class256 (:background unspecified :foreground unspecified))))
   `(cperl-hash-face
     ((,class256 (:inherit font-lock-variable-name-face))))
   `(cperl-array-face
     ((,class256 (:inherit font-lock-variable-name-face))))

   ;; isearch & search
   `(isearch
     ((,class256 (:background ,white :foreground ,bright-yellow :inverse-video t))))
   `(isearch-group-1
     ((,class256 (:background ,white :foreground ,yellow2 :inverse-video t))))
   `(isearch-group-2
     ((,class256 (:background ,white :foreground ,yellow1 :inverse-video t))))
   `(lazy-highlight
     ((,class256 (:background ,white :foreground ,green :inverse-video t))))
   `(isearch-fail
     ((,class256 (:background ,diff-removed-bg :foreground ,diff-removed-fg))))
   `(hi-yellow
     ((,class256 (:background ,hi-yellow :foreground unspecified :inverse-video nil))))
   `(hi-pink
     ((,class256 (:background ,hi-pink :foreground unspecified :inverse-video nil))))
   `(hi-blue
     ((,class256 (:background ,hi-blue :foreground unspecified :inverse-video nil))))
   `(hi-green
     ((,class256 (:background ,hi-green :foreground unspecified :inverse-video nil))))
   `(hi-salmon
     ((,class256 (:background ,hi-salmon :foreground unspecified :inverse-video nil))))
   `(hi-aquamarine
     ((,class256 (:background ,hi-aquamarine :foreground unspecified :inverse-video nil))))
   `(hi-red-b
     ((,class256 (:background unspecified :foreground ,red :weight bold))))
   `(hi-green-b
     ((,class256 (:background unspecified :foreground ,green :weight bold))))
   `(hi-blue-b
     ((,class256 (:background unspecified :foreground ,blue :weight bold))))

   ;; replace.el
   `(match
     ((,class256 (:background ,match))))

   ;; global-hl-line-mode
   `(hl-line
     ((,class256 (:background ,hl-line :foreground unspecified))))

   ;; customize & widget
   `(custom-button
     ((,class256 (:background ,grey1 :foreground ,fg :extend t
                              :box (:line-width (2 . 2) :style released-button)))))
   `(custom-button-pressed
     ((,class256 (:background ,grey1 :foreground ,fg :extend t
                              :box (:line-width (2 . 2) :style pressed-button)))))
   `(custom-button-mouse
     ((,class256 (:background ,hl-line :foreground ,fg :extend t
                              :box (:line-width (2 . 2) :style released-button)))))
   `(custom-state
     ((,class256 (:foreground ,green))))
   `(custom-group-tag
     ((,class256 (:foreground ,bright-magenta :weight bold))))
   `(custom-variable-tag
     ((,class256 (:background unspecified :foreground unspecified :weight bold))))
   `(custom-comment
     ((,class256 (:background unspecified :foreground ,comment :weight unspecified))))
   `(custom-comment-tag
     ((,class256 (:background unspecified :foreground unspecified :weight unspecified))))
   `(widget-field
     ((,class256 (:background ,grey1 :foreground ,fg :extend t))))
   `(widget-inactive
     ((,class256 (:foreground ,darkgrey))))
   `(widget-button-pressed
     ((,class256 (:foreground ,bright-red))))
   `(widget-documentation
     ((,class256 (:foreground ,green))))

   ;; shortdoc
   `(shortdoc-heading
     ((,class256 (:inherit default :weight bold :height 1.3))))
   `(shortdoc-section
     ((,class256 (:inherit default))))

   ;; package
   `(package-help-section-name
     ((,class256 (:foreground unspecified :weight bold))))
   `(package-status-installed
     ((,class256 (:foreground ,darkgrey))))

   ;; dired
   `(dired-header
     ((,class256 (:foreground ,fg :weight bold))))
   `(dired-directory
     ((,class256 (:inherit font-lock-keyword-face :weight bold))))
   `(dired-symlink
     ((,class256 (:foreground ,cyan :weight bold :inherit nil))))
   `(dired-broken-symlink
     ((,class256 (:background ,bright-red :foreground ,white))))
   `(dired-special
     ((,class256 (:foreground ,magenta :inherit nil))))
   `(dired-perm-write
     ((,class256 (:foreground ,yellow :inherit nil))))

   ;; completion
   `(icomplete-first-match
     ((,class256 (:foreground ,green :weight bold))))
   `(icomplete-selected-match
     ((,class256 (:background ,hl-line))))
   `(completions-common-part
     ((,class256 (:foreground ,red :weight bold))))
   `(completions-first-difference
     ((,class256 (:foreground ,blue :weight bold))))
   `(completions-annotations
     ((,class256 (:foreground ,darkgrey))))

   ;; ido
   `(ido-first-match
     ((,class256 (:foreground ,green :weight bold))))
   `(ido-only-match
     ((,class256 (:inherit 'ido-first-match))))
   `(ido-virtual
     ((,class256 (:foreground ,darkgrey))))
   `(ido-subdir
     ((,class256 (:foreground ,fg :weight bold))))
   ;; check how good it is
   `(ido-indicator
     ((,class256 (:background ,bright-red :foreground ,yellow))))

   ;; compilation
   `(compilation-mode-line-fail
     ((,class256 (:foreground ,red :weight bold))))
   `(compilation-mode-line-exit
     ((,class256 (:foreground ,green :weight bold))))
   `(compilation-line-number
     ((,class256 (:foreground ,darkgrey))))

   ;; whitespace
   `(whitespace-space
     ((,class256 (:background unspecified :foreground ,non-text))))
   `(whitespace-line
     ((,class256 nil)))
   `(whitespace-trailing
     ((,class256 (:inherit 'trailing-whitespace))))
   `(whitespace-indentation
     ((,class256 (:inherit 'whitespace-space))))
   `(whitespace-tab
     ((,class256 (:inherit 'whitespace-space))))
   `(whitespace-empty
     ((,class256 (:background ,yellow))))

   ;; message
   `(message-header-name
     ((,class256 (:foreground ,magenta))))
   `(message-header-newsgroups
     ((,class256 (:foreground ,black :weight bold))))
   `(message-header-subject
     ((,class256 (:foreground ,black :weight bold))))
   `(message-header-to
     ((,class256 (:foreground ,black))))
   `(message-header-other
     ((,class256 (:foreground ,fg))))
   `(message-header-cc
     ((,class256 (:foreground ,darkgrey))))
   `(message-separator
     ((,class256 (:background ,grey1 :foreground ,fg :extend t))))
   `(message-mml
     ((,class256 (:foreground ,comment))))

   ;; bookmark
   `(bookmark-face
     ((,class256 (:background unspecified :foreground ,yellow))))

   ;; info
   `(info-title-1
     ((,class256 (:foreground ,black :weight bold))))
   `(info-title-2
     ((,class256 (:foreground ,black :weight bold))))
   `(info-title-3
     ((,class256 (:foreground ,black :weight bold))))
   `(info-title-4
     ((,class256 (:foreground ,black :weight bold))))
   `(info-menu-header
     ((,class256 (:inherit info-title-3))))
   `(info-node
     ((,class256 (:foreground ,yellow :weight bold))))

   ;; edmacro
   `(edmacro-label
     ((,class256 (:foreground ,blue :weight bold))))

   ;; outline
   `(outline-1
     ((,class256 (:foreground ,outline-1 :weight bold))))
   `(outline-2
     ((,class256 (:foreground ,outline-2 :weight bold))))
   `(outline-3
     ((,class256 (:foreground ,outline-3 :weight bold))))
   `(outline-4
     ((,class256 (:foreground ,outline-4 :weight bold))))
   `(outline-5
     ((,class256 (:foreground ,outline-5 :weight bold))))
   `(outline-6
     ((,class256 (:foreground ,outline-6 :weight bold))))
   `(outline-7
     ((,class256 (:foreground ,outline-7 :weight bold))))
   `(outline-8
     ((,class256 (:foreground ,outline-8 :weight bold))))

   ;; org
   `(org-meta-line
     ((,class256 (:foreground ,darkgrey))))
   `(org-document-info-keyword
     ((t (:inherit org-meta-line))))
   `(org-special-keyword
     ((t (:inherit org-meta-line))))
   `(org-block
     ((,classTC (:background ,code-block :foreground ,fg :extend t))
      (t (:foreground ,fg))))
   `(org-block-begin-line
     ((t (:background unspecified :foreground ,darkgrey))))
   `(org-block-end-line
     ((t (:background unspecified :foreground ,darkgrey))))
   `(org-document-title
     ((,class256 (:foreground ,black :weight bold))))
   `(org-document-info
     ((,class256 (:foreground ,fg))))
   `(org-drawer
     ((,class256 (:foreground ,purple))))
   `(org-code
     ((,classTC (:background ,code-block :foreground ,cyan :extend t))
      (t (:foreground ,cyan))))
   `(org-verbatim
     ((,classTC (:background ,code-block :foreground ,magenta :extend t))
      (t (:foreground ,magenta))))
   `(org-footnote
     ((,class256 (:foreground ,darkgrey))))
   `(org-ellipsis
     ((,class256 (:foreground ,yellow))))
   `(org-formula
     ((,class256 (:foreground ,red))))
   `(org-latex-and-related
     ((,class256 (:foreground ,yellow))))
   `(org-tag
     ((,class256 (:foreground ,darkgrey :weight normal))))
   `(org-level-1
     ((,class256 (:inherit outline-1))))
   `(org-level-2
     ((,class256 (:inherit outline-2))))
   `(org-level-3
     ((,class256 (:inherit outline-3))))
   `(org-level-4
     ((,class256 (:inherit outline-4))))
   `(org-level-5
     ((,class256 (:inherit outline-5))))
   `(org-level-6
     ((,class256 (:inherit outline-6))))
   `(org-level-7
     ((,class256 (:inherit outline-7))))
   `(org-level-8
     ((,class256 (:inherit outline-8))))
   `(org-todo
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(org-done
     ((,class256 (:foreground ,bright-green :weight bold))))
   `(org-date
     ((,class256 (:foreground ,darkgrey))))
   `(org-sexp-date
     ((,class256 (:foreground ,bright-cyan))))
   `(org-headline-done
     ((,class256 (:foreground unspecified))))
   `(org-checkbox
     ((,class256 (:foreground ,darkgrey :weight normal))))
   `(org-dispatcher-highlight
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(org-agenda-structure
     ((,class256 (:foreground ,black :weight bold))))
   `(org-agenda-structure-filter
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(org-date-selected
     ((,class256 (:background ,bright-magenta :foreground ,bg))))
   `(org-agenda-date
     ((,class256 (:foreground ,blue :weight normal))))
   `(org-agenda-date-today
     ((,class256 (:foreground ,bright-magenta :weight bold))))
   `(org-agenda-current-time
     ((,class256 (:foreground ,bright-magenta))))
   `(org-agenda-done
     ((,class256 (:foreground ,green))))
   `(org-scheduled-today
     ((,class256 (:foreground ,bright-cyan))))
   `(org-scheduled
     ((,class256 (:foreground ,cyan))))
   `(org-scheduled-previously
     ((,class256 (:foreground ,red))))
   `(org-upcoming-deadline
     ((,class256 (:foreground ,bright-yellow))))
   `(org-imminent-deadline
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(org-time-grid
     ((,class256 (:foreground ,darkgrey :weight normal))))
   `(org-table
     ((,class256 (:foreground ,fg))))
   `(org-mode-line-clock-overrun
     ((,class256 (:background ,yellow :foreground ,white))))

   ;; gnus
   `(gnus-button
     ((,class256 (:inherit link))))
   `(gnus-group-mail-1
     ((,class256 (:foreground ,green :weight bold))))
   `(gnus-group-mail-1-empty
     ((,class256 (:foreground ,green))))
   `(gnus-group-mail-2
     ((,class256 (:foreground ,yellow :weight bold))))
   `(gnus-group-mail-2-empty
     ((,class256 (:foreground ,yellow))))
   `(gnus-group-mail-3
     ((,class256 (:foreground ,fg :weight bold))))
   `(gnus-group-mail-3-empty
     ((,class256 (:foreground ,fg))))
   `(gnus-group-mail-4
     ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-4-empty
     ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-mail-5
     ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-5-empty
     ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-mail-6
     ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-6-empty
     ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-mail-low
     ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-mail-low-empty
     ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-news-1
     ((,class256 (:foreground ,green :weight bold))))
   `(gnus-group-news-1-empty
     ((,class256 (:foreground ,green))))
   `(gnus-group-news-2
     ((,class256 (:foreground ,yellow :weight bold))))
   `(gnus-group-news-2-empty
     ((,class256 (:foreground ,yellow))))
   `(gnus-group-news-3
     ((,class256 (:foreground ,fg :weight bold))))
   `(gnus-group-news-3-empty
     ((,class256 (:foreground ,fg))))
   `(gnus-group-news-4
     ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-4-empty
     ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-news-5
     ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-5-empty
     ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-news-6
     ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-6-empty
     ((,class256 (:foreground ,darkgrey))))
   `(gnus-group-news-low
     ((,class256 (:foreground ,darkgrey :weight bold))))
   `(gnus-group-news-low-empty
     ((,class256 (:foreground ,darkgrey))))
   `(gnus-summary-selected
     ((,class256 (:background ,cyan :foreground ,bg :underline nil))))
   `(gnus-summary-normal-unread
     ((,class256 (:foreground ,black :weight bold))))
   `(gnus-summary-normal-read
     ((,class256 (:foreground ,fg))))
   `(gnus-summary-normal-ticked
     ((,class256 (:foreground ,yellow))))
   `(gnus-summary-normal-ancient
     ((,class256 (:foreground ,fg))))
   `(gnus-summary-cancelled
     ((,class256 (:background unspecified :foreground ,red))))
   `(gnus-header
     ((,class256 (:inherit default))))
   `(gnus-header-name
     ((,class256 (:foreground ,magenta))))
   `(gnus-header-from
     ((,class256 (:foreground ,fg :weight normal))))
   `(gnus-header-content
     ((,class256 (:foreground ,fg :weight normal :slant normal))))
   `(gnus-header-subject
     ((,class256 (:foreground ,black :weight bold))))
   `(gnus-header-newsgroups
     ((,class256 (:foreground ,black :weight bold))))
   `(gnus-cite-attribution
     ((,class256 (:foreground ,green :weight bold :slant normal :underline nil))))
   `(gnus-cite-1
     ((,class256 (:foreground ,green))))
   `(gnus-cite-2
     ((,class256 (:foreground ,yellow))))
   `(gnus-cite-3
     ((,class256 (:foreground ,blue))))
   `(gnus-cite-4
     ((,class256 (:foreground ,magenta))))
   `(gnus-cite-5
     ((,class256 (:foreground ,cyan))))
   `(gnus-cite-6
     ((,class256 (:foreground ,bright-green))))
   `(gnus-cite-7
     ((,class256 (:foreground ,bright-yellow))))
   `(gnus-cite-8
     ((,class256 (:foreground ,bright-blue))))
   `(gnus-cite-9
     ((,class256 (:foreground ,bright-magenta))))
   `(gnus-cite-10
     ((,class256 (:foreground ,bright-cyan))))
   `(gnus-cite-11
     ((,class256 (:foreground ,darkgrey))))
   `(mm-uu-extract
     ((,class256 (:background ,block :foreground ,yellow))))

   ;; highlight-changes
   `(highlight-changes
     ((,class256 (:foreground ,yellow1))))
   `(highlight-changes-delete
     ((,class256 (:foreground ,red :underline t))))

   ;; shr
   `(shr-h1
     ((,class256 (:foreground ,black :weight bold :height 1.6))))
   `(shr-h2
     ((,class256 (:foreground ,black :weight bold :height 1.4))))
   `(shr-h3
     ((,class256 (:foreground ,black :weight bold :height 1.2))))
   `(shr-h4
     ((,class256 (:foreground ,black :weight bold :height 1.1))))
   `(shr-h5
     ((,class256 (:foreground ,black :weight bold :height 1.0))))
   `(shr-h6
     ((,class256 (:foreground ,black :weight bold :height 1.0))))

   ;; dictionary
   `(dictionary-word-definition-face
     ((,class256 (:family nil))))
   `(dictionary-reference-face
     ((,class256 (:foreground ,yellow))))

   ;; markdown
   `(markdown-metadata-key-face
     ((,class256 (:foreground ,darkgrey))))
   `(markdown-header-face
     ((,class256 (:foreground ,black :weight bold))))
   `(markdown-header-delimiter-face
     ((,class256 (:foreground ,blue :weight bold))))
   `(markdown-header-rule-face
     ((,class256 (:foreground ,blue :weight bold))))
   `(markdown-code-face
     ((,classTC (:background ,code-block :foreground ,fg :extend t))
      (t (:foreground ,fg))))
   `(markdown-list-face
     ((,class256 (:foreground ,yellow))))
   `(markdown-markup-face
     ((,class256 (:foreground ,darkgrey))))
   `(markdown-inline-code-face
     ((,classTC (:background ,code-block :foreground ,cyan))
      (t (:foreground ,cyan))))
   `(markdown-line-break-face
     ((,class256 (:foreground ,red :weight unspecified :slant unspecified))))
   `(markdown-language-keyword-face
     ((,class256 (:foreground ,darkgrey))))
   `(markdown-gfm-checkbox-face
     ((,class256 (:foreground ,darkgrey))))

   ;; diff
   `(diff-header
     ((,class256 (:background ,grey3 :foreground ,blue :weight bold))))
   `(diff-file-header
     ((,class256 (:background ,grey3 :foreground ,black))))
   `(diff-hunk-header
     ((,class256 (:foreground ,yellow :weight bold))))
   `(diff-function
     ((,class256 (:foreground ,red :weight bold))))
   `(diff-added
     ((,class256 (:background ,diff-added-bg :foreground ,diff-added-fg))))
   `(diff-indicator-added
     ((t (:inherit 'diff-added :foreground ,green))))
   `(diff-refine-added
     ((,class256 (:background ,diff-refine-added-bg :foreground ,diff-added-fg))))
   `(diff-removed
     ((,class256 (:background ,diff-removed-bg :foreground ,diff-removed-fg))))
   `(diff-refine-removed
     ((,class256 (:background ,diff-refine-removed-bg :foreground ,diff-removed-fg))))
   `(diff-indicator-removed
     ((t (:inherit 'diff-removed :foreground ,red))))

   ;; vc
   `(vc-dir-header
     ((,class256 (:foreground ,magenta))))
   `(vc-dir-header-value
     ((,class256 (:foreground ,fg))))
   `(vc-dir-directory
     ((,class256 (:foreground ,blue :weight bold))))
   `(vc-dir-file
     ((,class256 (:foreground ,fg))))
   `(vc-dir-status-up-to-date
     ((,class256 (:foreground ,green))))
   `(vc-dir-status-edited
     ((,class256 (:foreground ,yellow))))
   `(vc-dir-mark-indicator
     ((,class256 (:foreground ,red))))
   `(vc-edited-state
     ((,class256 (:foreground ,yellow))))
   `(vc-conflict-state
     ((,class256 (:foreground ,red))))
   `(vc-locally-added-state
     ((,class256 (:foreground ,cyan))))
   `(vc-locked-state
     ((,class256 (:foreground ,blue))))
   `(vc-missing-state
     ((,class256 (:foreground ,magenta))))
   `(vc-needs-update-state
     ((,class256 (:foreground ,green))))
   `(vc-removed-state
     ((,class256 (:foreground ,red))))
   `(log-edit-header
     ((,class256 (:foreground ,bright-magenta :weight bold))))
   `(log-edit-summary
     ((,class256 (:foreground ,black :weight bold))))
   `(log-edit-headers-separator
     ((,classTTY (:background unspecified))
      (,classTC (:background ,grey :height 0.1 :extend t))))
   `(log-view-message
     ((,class256 (:foreground ,darkgrey))))
   `(log-view-commit-body
     ((,class256 (:foreground ,fg))))

   ;; git-commit
   `(git-commit-summary
     ((,class256 (:foreground ,black :weight bold))))
   `(git-commit-nonempty-second-line
     ((,class256 (:foreground ,red :weight bold))))

   ;; magit
   `(magit-section-heading
     ((,class256 (:foreground ,yellow :weight bold))))
   `(magit-section-heading-selection
     ((,class256 (:foreground ,bright-yellow))))
   `(magit-section-highlight
     ((,class256 (:background ,hl-line))))
   `(magit-branch-local
     ((,class256 (:foreground ,blue))))
   `(magit-branch-remote
     ((,class256 (:foreground ,green))))
   `(magit-tag
     ((,class256 (:foreground ,bright-yellow))))
   `(magit-dimmed
     ((t (:foreground ,darkgrey))))
   `(magit-hash
     ((t (:foreground ,darkgrey))))
   `(magit-cherry-equivalent
     ((,class256 (:foreground ,magenta))))
   `(magit-cherry-unmatched
     ((,class256 (:foreground ,cyan))))
   `(magit-bisect-bad
     ((,class256 (:foreground ,red))))
   `(magit-bisect-good
     ((,class256 (:foreground ,green))))
   `(magit-bisect-skip
     ((,class256 (:foreground ,yellow))))
   `(magit-diff-hunk-heading
     ((t (:background ,grey1))))
   `(magit-diff-hunk-heading-highlight
     ((t (:background ,grey1 :weight bold))))
   `(magit-diff-context
     ((t (:foreground ,fg))))
   `(magit-diff-context-highlight
     ((t (:background ,grey3))))
   `(magit-diff-added
     ((t (:inherit 'diff-added))))
   `(magit-diff-added-highlight
     ((t (:inherit 'diff-added))))
   `(magit-diff-removed
     ((t (:inherit 'diff-removed))))
   `(magit-diff-removed-highlight
     ((t (:inherit 'diff-removed))))
   `(magit-diff-lines-heading
     ((,class256 (:background ,green :foreground ,black))))
   `(magit-diffstat-added
     ((,class256 (:foreground ,green))))
   `(magit-diffstat-removed
     ((,class256 (:foreground ,red))))
   `(magit-log-author
     ((,class256 (:foreground ,red))))
   `(magit-log-graph
     ((,class256 (:foreground ,darkgrey))))
   `(magit-log-date
     ((,class256 (:foreground ,darkgrey))))
   `(magit-blame-name
     ((,class256 (:foreground ,red))))
   `(magit-blame-date
     ((,class256 (:foreground ,cyan))))
   `(magit-blame-heading
     ((,class256 (:background ,grey1 :foreground ,black))))
   `(magit-blame-margin
     ((,class256 (:background ,grey1 :foreground ,black))))
   `(magit-blame-highlight
     ((,class256 (:background ,grey1 :foreground ,black))))
   `(magit-reflog-amend
     ((,class256 (:foreground ,bright-magenta))))
   `(magit-reflog-merge
     ((,class256 (:foreground ,bright-green))))
   `(magit-reflog-other
     ((,class256 (:foreground ,bright-cyan))))
   `(magit-reflog-reset
     ((,class256 (:foreground ,bright-red))))
   `(magit-reflog-commit
     ((,class256 (:foreground ,bright-green))))
   `(magit-reflog-rebase
     ((,class256 (:foreground ,bright-magenta))))
   `(magit-reflog-remote
     ((,class256 (:foreground ,bright-cyan))))
   `(magit-reflog-checkout
     ((,class256 (:foreground ,bright-blue))))
   `(magit-reflog-cherry-pick
     ((,class256 (:foreground ,bright-green))))

   `(transient-key
     ((,class256 (:inherit help-key-binding))))
   `(transient-key-stay
     ((,class256 (:foreground ,green :weight bold))))
   `(transient-key-exit
     ((,class256 (:foreground ,magenta :weight bold))))
   `(transient-key-return
     ((,class256 (:foreground ,blue :weight bold))))
   `(transient-heading
     ((,class256 (:inherit magit-section-heading))))

   ;; ediff
   `(ediff-current-diff-A
     ((,class256 (:background ,diff-removed-bg))))
   `(ediff-current-diff-B
     ((,class256 (:background ,diff-added-bg))))
   `(ediff-current-diff-C
     ((,class256 (:background ,diff-changed-bg))))
   `(ediff-current-diff-Ancestor
     ((,class256 (:background ,diff-ancestor-bg))))
   `(ediff-fine-diff-A
     ((,class256 (:background ,diff-refine-removed-bg :foreground ,diff-removed-fg))))
   `(ediff-fine-diff-B
     ((,class256 (:background ,diff-refine-added-bg :foreground ,diff-added-fg))))
   `(ediff-fine-diff-C
     ((,class256 (:background ,diff-refine-changed-bg :foreground ,diff-changed-fg))))
   `(ediff-fine-diff-Ancestor
     ((,class256 (:background ,diff-refine-ancestor-bg :foreground ,diff-ancestor-fg))))
   `(ediff-even-diff-A
     ((,class256 (:background ,grey1))))
   `(ediff-even-diff-B
     ((,class256 (:background ,grey1))))
   `(ediff-even-diff-C
     ((,class256 (:background ,grey1))))
   `(ediff-even-diff-Ancestor
     ((,class256 (:background ,grey1))))
   `(ediff-odd-diff-A
     ((,class256 (:background ,grey1))))
   `(ediff-odd-diff-B
     ((,class256 (:background ,grey1))))
   `(ediff-odd-diff-C
     ((,class256 (:background ,grey1))))
   `(ediff-odd-diff-Ancestor
     ((,class256 (:background ,grey1))))

   ;; smerge
   `(smerge-lower
     ((,class256 (:background ,diff-added-bg))))
   `(smerge-upper
     ((,class256 (:background ,diff-removed-bg))))
   `(smerge-refined-added
     ((t (:background ,diff-refine-added-bg :foreground ,diff-added-fg))))
   `(smerge-refined-removed
     ((t (:background ,diff-refine-removed-bg :foreground ,diff-removed-fg))))

   ;; epa
   `(epa-mark
     ((,class256 (:foreground ,red :weight bold))))
   `(epa-string
     ((,class256 (:foreground ,green))))
   `(epa-validity-high
     ((,class256 (:foreground ,green :weight bold))))
   `(epa-validity-medium
     ((,class256 (:foreground ,cyan :weight bold))))
   `(epa-validity-low
     ((,class256 (:foreground ,yellow))))
   `(epa-validity-disabled
     ((,class256 (:foreground ,darkgrey))))

   ;; flyspell
   `(flyspell-incorrect
     ((,classTTY (:inherit error :underline t))
      (t (:underline (:style wave :color ,bright-red)))))
   `(flyspell-duplicate
     ((,classTTY (:inherit warning :underline t))
      (t (:underline (:style wave :color ,bright-yellow)))))

   ;; flymake
   `(flymake-error
     ((,classTTY (:inherit error :underline t))
      (t (:underline (:style wave :color ,bright-red)))))
   `(flymake-warning
     ((,classTTY (:inherit warning :underline t))
      (t (:underline (:style wave :color ,bright-yellow)))))

   ;; eglot
   `(eglot-highlight-symbol-face
     ((,class256 (:background ,match))))

   ;; lsp-mode
   `(lsp-face-highlight-textual
     ((,class256 (:background ,match))))

   ;; wgrep
   `(wgrep-face
     ((,class256 (:background ,diff-added-bg))))
   `(wgrep-done-face
     ((,class256 (:foreground ,yellow))))
   `(wgrep-file-face
     ((t (:inherit wgrep-face))))
   `(wgrep-delete-face
     ((,class256 (:background ,diff-removed-bg))))
   `(wgrep-reject-face
     ((t (:inherit error))))

   ;; erc
   `(erc-timestamp-face
     ((,class256 (:foreground ,bright-cyan))))
   `(erc-notice-face
     ((,class256 (:foreground ,darkgrey))))
   `(erc-my-nick-face
     ((,class256 (:foreground ,red :weight bold))))
   `(erc-current-nick-face
     ((,class256 (:foreground ,red :weight bold))))
   `(erc-nick-msg-face
     ((,class256 (:foreground ,yellow))))
   `(erc-input-face
     ((,class256 (:foreground ,green))))
   `(erc-error-face
     ((,class256 (:foreground ,bright-red))))
   `(erc-dangerous-host-face
     ((,class256 (:foreground ,bright-red))))
   `(erc-direct-msg-face
     ((,class256 (:foreground ,yellow))))
   `(erc-button
     ((,class256 (:background unspecified :foreground ,blue :underline t))))
   `(erc-prompt-face
     ((,class256 (:background unspecified :foreground ,bright-magenta :weight bold :inverse-video t))))
   `(erc-action-face
     ((,class256 (:background unspecified :foreground ,comment))))
   `(fg:erc-face0
     ((,class256 (:foreground ,black))))
   `(fg:erc-face1
     ((,class256 (:foreground ,red))))
   `(fg:erc-face2
     ((,class256 (:foreground ,green))))
   `(fg:erc-face3
     ((,class256 (:foreground ,yellow))))
   `(fg:erc-face4
     ((,class256 (:foreground ,blue))))
   `(fg:erc-face5
     ((,class256 (:foreground ,magenta))))
   `(fg:erc-face6
     ((,class256 (:foreground ,cyan))))
   `(fg:erc-face7
     ((,class256 (:foreground ,grey))))
   `(fg:erc-face8
     ((,class256 (:foreground ,darkgrey))))
   `(fg:erc-face9
     ((,class256 (:foreground ,bright-red))))
   `(fg:erc-face10
     ((,class256 (:foreground ,bright-green))))
   `(fg:erc-face11
     ((,class256 (:foreground ,bright-yellow))))
   `(fg:erc-face12
     ((,class256 (:foreground ,bright-blue))))
   `(fg:erc-face13
     ((,class256 (:foreground ,bright-magenta))))
   `(fg:erc-face14
     ((,class256 (:foreground ,bright-cyan))))
   `(fg:erc-face15
     ((,class256 (:foreground ,black))))

   ;; rcirc
   `(rcirc-server
     ((,class256 (:foreground ,darkgrey))))
   `(rcirc-timestamp
     ((,class256 (:foreground ,bright-cyan))))
   `(rcirc-prompt
     ((,class256 (:foreground ,bright-magenta :weight bold))))
   `(rcirc-url
     ((,class256 (:background unspecified :foreground ,blue :underline t))))
   `(rcirc-my-nick
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(rcirc-nick-in-message
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(rcirc-other-nick
     ((,class256 (:foreground ,blue))))

   ;; calendar
   `(calendar-month-header
     ((,class256 (:foreground ,black :weight bold))))
   `(calendar-weekday-header
     ((,class256 (:foreground ,blue))))
   `(calendar-weekend-header
     ((,class256 (:foreground ,yellow))))
   `(calendar-today
     ((,class256 (:foreground ,green))))
   `(holiday
     ((,class256 (:background ,bright-magenta :foreground ,bg))))

   ;; elfeed
   `(elfeed-log-date-face
     ((,class256 (:foreground ,yellow))))
   `(elfeed-log-error-level-face
     ((,class256 (:foreground ,red))))
   `(elfeed-log-warn-level-face
     ((,class256 (:foreground ,yellow))))
   `(elfeed-log-info-level-face
     ((,class256 (:foreground ,blue))))
   `(elfeed-log-debug-level-face
     ((,class256 (:foreground ,magenta))))
   `(elfeed-search-date-face
     ((,class256 (:foreground ,darkgrey))))
   `(elfeed-search-title-face
     ((,class256 (:foreground ,black))))
   `(elfeed-search-unread-title-face
     ((,class256 (:foreground ,black :weight bold))))
   `(elfeed-search-feed-face
     ((,class256 (:foreground ,green))))
   `(elfeed-search-tag-face
     ((,class256 (:foreground ,yellow))))
   `(elfeed-search-unread-count-face
     ((,class256 (:foreground ,blue))))
   `(info-menu-star
     ((,class256 (:foreground ,bright-yellow))))

   ;; comint
   `(comint-highlight-prompt
     ((,class256 (:foreground ,bright-magenta :weight bold))))

   ;; eshell
   `(eshell-prompt
     ((,class256 (:foreground ,bright-magenta :weight bold))))
   `(eshell-ls-directory
     ((,class256 (:foreground ,blue :weight bold))))
   `(eshell-ls-symlink
     ((,class256 (:foreground ,cyan :weight bold))))
   `(eshell-ls-executable
     ((,class256 (:foreground ,green :weight bold))))
   `(eshell-ls-cluttern
     ((,class256 (:foreground ,red))))
   `(eshell-ls-archive
     ((,class256 (:foreground ,yellow))))
   `(eshell-ls-backup
     ((,class256 (:foreground ,darkgrey))))
   `(eshell-ls-unreadable
     ((,class256 (:foreground ,non-text))))
   `(eshell-ls-missing
     ((,class256 (:background ,red :foreground ,black))))
   `(eshell-ls-product
     ((,class256 (:foreground ,black))))
   `(eshell-ls-readonly
     ((,class256 (:foreground ,darkgrey))))
   `(eshell-ls-special
     ((,class256 (:foreground ,magenta))))

   ;; eww
   `(eww-form-text
     ((,class256 (:inherit widget-field :box (:foreground ,grey)))))
   `(eww-form-textarea
     ((,class256 (:inherit widget-field))))
   `(eww-form-submit
     ((,class256 (:inherit custom-button))))
   `(eww-form-file
     ((,class256 (:inherit custom-button))))
   `(eww-valid-certificate
     ((,class256 (:foreground ,green))))
   `(eww-invalid-certificate
     ((,class256 (:foreground ,red))))

   ;; emms
   `(emms-playlist-selected-face
     ((,class256 (:foreground ,blue :weight bold))))
   `(emms-playlist-track-face
     ((,class256 (:foreground ,fg))))
   `(emms-browser-track-face
     ((,class256 (:inherit emms-playlist-track-face))))
   `(emms-browser-artist-face
     ((,class256 (:foreground ,blue))))
   `(emms-browser-album-face
     ((,class256 (:foreground ,yellow))))
   `(emms-browser-composer-face
     ((,class256 (:foreground ,cyan))))
   `(emms-browser-performer-face
     ((,class256 (:foreground ,magenta))))
   `(emms-browser-year/genre-face
     ((,class256 (:foreground ,red))))

   ;; vertico
   `(vertico-current
     ((,class256 (:background ,hl-line))))

   ;; orderless
   `(orderless-match-face-0
     ((,class256 (:foreground ,red :weight bold))))
   `(orderless-match-face-1
     ((,class256 (:foreground ,blue :weight bold))))
   `(orderless-match-face-2
     ((,class256 (:foreground ,green :weight bold))))
   `(orderless-match-face-3
     ((,class256 (:foreground ,yellow :weight bold))))

   ;; marginalia
   `(marginalia-key
     ((,class256 (:foreground ,cyan))))
   `(marginalia-date
     ((,class256 (:foreground ,darkgrey))))
   `(marginalia-file-priv-dir
     ((,class256 (:foreground ,darkgrey))))
   `(marginalia-file-priv-link
     ((,class256 (:foreground ,cyan))))
   `(marginalia-file-priv-read
     ((,class256 (:foreground ,magenta))))
   `(marginalia-file-priv-write
     ((,class256 (:foreground ,blue))))
   `(marginalia-file-priv-exec
     ((,class256 (:foreground ,yellow))))

   ;; consult
   `(consult-file
     ((,class256 (:foreground ,darkgrey))))
   `(consult-bookmark
     ((,class256 (:foreground ,comment))))
   `(consult-highlight-match
     ((,class256 (:background ,match))))

   ;; embark
   `(embark-keybinding
     ((,class256 (:inherit help-key-binding))))

   ;; notmuch
   `(notmuch-message-summary-face
     ((,class256 (:background ,grey2))))
   `(notmuch-tag-face
     ((,class256 (:foreground ,yellow))))
   `(notmuch-tag-unread
     ((,class256 (:foreground ,green))))
   `(notmuch-tag-flagged
     ((,class256 (:foreground ,blue))))
   `(notmuch-search-flagged-face
     ((,class256 (:foreground ,blue))))
   `(notmuch-tag-added
     ((,class256 (:underline ,cyan))))
   `(notmuch-tag-deleted
     ((,class256 (:foreground ,red :strike-through ,red))))

   ;; verb (org based restclient)
   `(verb-http-keyword
     ((,class256 (:foreground ,blue))))
   `(verb-header
     ((,class256 (:foreground ,yellow))))
   `(verb-code-tag
     ((,class256 (:foreground ,cyan))))

   ;; sly
   `(sly-mrepl-output-face
     ((,class256 (:foreground ,fg))))
   `(sly-mrepl-note-face
     ((,class256 (:foreground ,yellow))))
   `(sly-action-face
     ((,class256 (:foreground ,bright-blue :weight bold))))

   ;; corfu
   `(corfu-default
     ((,class256 (:background ,grey3))))
   `(corfu-current
     ((,class256 (:background ,grey1))))
   `(corfu-bar
     ((,class256 (:background ,non-text))))
   `(corfu-border
     ((,class256 (:background ,grey))))

   ;; company
   `(company-tooltip
     ((,class256 (:background ,grey3))))
   `(company-tooltip-common
     ((,class256 (:inherit completions-common-part))))
   `(company-tooltip-annotation
     ((,class256 (:inherit completions-annotations))))
   `(company-tooltip-selection
     ((,class256 (:background ,grey1))))
   `(company-tooltip-scrollbar-track
     ((,class256 (:background ,grey))))
   `(company-tooltip-scrollbar-thumb
     ((,class256 (:background ,non-text))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-base-error-face
     ((,class256 (:background ,grey3 :foreground ,red :weight bold))))
   `(rainbow-delimiters-depth-1-face
     ((,class256 (:foreground ,fg))))
   `(rainbow-delimiters-depth-2-face
     ((,class256 (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face
     ((,class256 (:foreground ,yellow))))
   `(rainbow-delimiters-depth-4-face
     ((,class256 (:foreground ,cyan))))
   `(rainbow-delimiters-depth-5-face
     ((,class256 (:foreground ,magenta))))
   `(rainbow-delimiters-depth-6-face
     ((,class256 (:foreground ,bright-blue))))
   `(rainbow-delimiters-depth-7-face
     ((,class256 (:foreground ,red))))
   `(rainbow-delimiters-depth-8-face
     ((,class256 (:foreground ,bright-cyan))))
   `(rainbow-delimiters-depth-9-face
     ((,class256 (:foreground ,bright-yellow))))

   ;; rst
   `(rst-level-1
     ((,class256 (:background unspecified :foreground ,black :weight bold))))
   `(rst-level-2
     ((,class256 (:background unspecified :foreground ,black :weight bold))))
   `(rst-level-3
     ((,class256 (:background unspecified :foreground ,black :weight bold))))
   `(rst-level-4
     ((,class256 (:background unspecified :foreground ,black :weight bold))))
   `(rst-level-5
     ((,class256 (:background unspecified :foreground ,black :weight bold))))
   `(rst-level-6
     ((,class256 (:background unspecified :foreground ,black :weight bold))))
   `(rst-literal
     ((,class256 (:foreground ,magenta))))
   `(rst-directive
     ((,class256 (:foreground ,purple))))
   `(rst-block
     ((,class256 (:foreground ,red))))
   `(rst-definition
     ((,class256 (:foreground ,green))))

   ;; sh
   `(sh-quoted-exec
     ((,class256 (:foreground ,bright-magenta))))
   `(sh-heredoc
     ((,class256 (:foreground ,bright-green))))

   ;; tuareg
   `(tuareg-font-lock-error-face
     ((,class256 (:foreground ,bright-red :background ,white :inverse-video t))))
   `(tuareg-font-lock-interactive-error-face
     ((,class256 (:inherit error))))
   `(tuareg-font-double-semicolon-face
     ((,class256 (:foreground ,bright-red))))
   `(tuareg-font-lock-interactive-output-face
     ((,class256 (:foreground ,darkgrey))))
   `(tuareg-font-lock-line-number-face
     ((,class256 (:foreground ,darkgrey))))
   `(tuareg-font-lock-extension-node-face
     ((,class256 (:inherit font-lock-preprocessor-face))))
   `(tuareg-font-lock-interactive-directive-face
     ((,class256 (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-governing-face
     ((,class256 (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-operator-face
     ((,class256 (:inherit font-lock-keyword-face))))

   `(which-key-key-face
     ((,class256 (:inherit help-key-binding))))
   `(which-key-separator-face
     ((,class256 (:foreground ,darkgrey))))
   `(which-key-command-description-face
     ((,class256 (:inherit default))))
   `(which-key-group-description-face
     ((,class256 (:foreground ,bright-magenta))))

   ;; tempel
   `(tempel-default
     ((,class256 (:background ,diff-added-bg))))
   `(tempel-field
     ((,class256 (:background ,diff-added-bg))))
   `(tempel-form
     ((,class256 (:background unspecified))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'wildcharm-light)
;;; wildcharm-light-theme.el ends here
