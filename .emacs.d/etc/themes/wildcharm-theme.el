;;; wildcharm-theme.el --- Port of vim-wildcharm colorscheme -*- lexical-binding: t; -*-

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

;; High-contrast dark Emacs theme.
;; - Port of the dark background vim-wildcharm colorscheme.
;; - TUI has no background defined, i.e. depends on terminal.
;; - Should look mostly the same in GUI and TUI with 256 colors support.

;;; Code:

(deftheme wildcharm
  "High-contrast dark Emacs theme."
  :family 'wildcharm)

(let* ((classTC '((class color) (min-colors 257)))
       (class256 '((class color) (min-colors 256)))
       (classTTY '((type tty)))
       (fg "#d0d0d0")(bg "#121212")
       (black "#000000")(darkgrey "#808080")
       (red "#d75f5f")(bright-red "#ff5f87")
       (green "#00af5f")(bright-green "#00d75f")
       (yellow "#d78700")(bright-yellow "#ffaf00")
       (blue "#0087d7")(bright-blue "#00afff")
       (magenta "#d787d7")(bright-magenta "#ff87ff")
       (cyan "#00afaf")(bright-cyan "#00d7d7")
       (grey "#d0d0d0")(white "#ffffff")
       (purple "#875fff")(comment "#87875f")
       (yellow1 "#d7af5f")(yellow2 "#ffd75f")
       (grey1 "#3a3a3a")(grey2 "#262626")
       (non-text "#585858")
       (match-paren "#ff00af")(match "#5f005f")
       (mode-line-active "#444444")(mode-line-inactive "#303030")
       (menu "#444444")
       (header-line "#262626")
       (hl-line "#3a3a3a")
       (block "#1c1c1c")
       (diff-added-bg "#3f4f3f")(diff-added-bg-tty "#005f00")
       (diff-refine-added-bg "#4a5f4a")(diff-refine-added-bg-tty "#005f5f")
       (diff-added-fg "#afffaf")
       (diff-removed-bg "#4f3f3f")(diff-removed-bg-tty "#5f0000")
       (diff-refine-removed-bg "#6f4f3f")(diff-refine-removed-bg-tty "#870000")
       (diff-removed-fg "#ffd7d7")
       (diff-changed-bg "#4f4f3f")(diff-changed-bg-tty "#5f5f00")
       (diff-refine-changed-bg "#7f7f5f")(diff-refine-changed-bg-tty "#87875f")
       (diff-changed-fg "#ffffd7")
       (diff-ancestor-bg "#3f3f4f")(diff-ancestor-bg-tty "#00005f")
       (diff-refine-ancestor-bg "#5f5f7f")(diff-refine-ancestor-bg-tty "#5f5f87")
       (diff-ancestor-fg "#d7d7ff")
       (outline-1 white)
       (outline-2 "#d7afd7")
       (outline-3 "#afd7ff")
       (outline-4 "#d7af87")
       (outline-5 "#87afaf")
       (outline-6 "#af875f")
       (outline-7 "#5fafd7")
       (outline-8 darkgrey)
       (hi-yellow "#5f5f00")
       (hi-pink "#005f87")
       (hi-blue "#000087")
       (hi-green "#005f00")
       (hi-salmon "#875f00")
       (hi-aquamarine "#005f5f")
       (code-block "#1F1F1F"))

  (custom-theme-set-faces
   'wildcharm

   ;; standard faces
   `(default
     ((,classTTY (:background unspecified, :foreground unspecified))
      (t (:background ,bg :foreground ,fg))))
   `(shadow
     ((,class256 (:foreground ,non-text))))
   `(link
     ((,class256 (:foreground ,bright-blue :underline t))))
   `(link-visited
     ((,class256 (:foreground ,magenta :underline t))))
   `(highlight
     ((,class256 (:background ,black :foreground ,bright-blue :inverse-video t))))
   `(region
     ((,class256 (:background ,bg :foreground ,blue :inverse-video t))))
   `(secondary-selection
     ((,class256 (:background ,bg :foreground ,cyan :inverse-video t))))
   `(trailing-whitespace
     ((,class256 (:foreground ,red :inverse-video t))))
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
     ((,class256 (:background ,mode-line-active :foreground ,grey
                              :box (:line-width 1 :color ,non-text)))))
   `(mode-line-inactive
     ((,class256 (:background ,mode-line-inactive :foreground ,darkgrey
                              :box (:line-width 1 :color ,mode-line-active)))))
   `(mode-line-highlight
     ((,class256 (:background ,black
                              :box (:line-width 1 :color ,non-text)))))
   `(mode-line-emphasis
     ((,class256 (:weight bold))))
   `(mode-line-buffer-id
     ((,class256 (:weight bold))))
   `(header-line
     ((,class256 (:background ,header-line :foreground ,fg :extend t
                              :box (:line-width 1 :color ,mode-line-active)))))
   `(vertical-border
     ((,classTTY (:background ,mode-line-inactive :foreground ,mode-line-inactive))
      (,class256 (:background ,non-text :foreground ,non-text))))
   `(window-divider
     ((t (:foreground ,mode-line-inactive))))
   `(window-divider-first-pixel
     ((t (:foreground ,mode-line-active))))
   `(window-divider-last-pixel
     ((t (:foreground ,mode-line-active))))
   ;; internal-border, not sure what exactly it affects...
   ;; child-frame-border, not sure neither...
   `(minibuffer-prompt
     ((,class256 (:foreground ,bright-yellow :weight bold))))
   `(fringe
     ((t (:foreground ,non-text :background unspecified))))
   `(separator-line
     ((t (:foreground ,non-text :underline t))))
   ;; -scroll-bar
   `(cursor
     ((t (:background ,white))))
   ;; -tool-bar
   `(tab-bar
     ((,class256 (:background ,mode-line-inactive :foreground ,darkgrey))))
   `(tab-bar-tab
     ((,class256 (:background ,mode-line-active :foreground ,white :weight bold
                              :box (:line-width 1 :color ,non-text)))))
   `(tab-bar-tab-inactive
     ((t (:background unspecified :foreground ,grey
                      :box (:line-width 1 :color ,non-text)))))
   `(tab-line
     ((,class256 (:background ,mode-line-inactive :foreground ,darkgrey))))
   `(tab-line-tab
     ((,class256 (:background ,mode-line-active :foreground ,white :weight bold
                              :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-current
     ((,class256 (:background ,mode-line-active :foreground ,white :weight bold
                              :box (:line-width 1 :color ,non-text)))))
   `(tab-line-highlight
     ((,class256 (:background ,mode-line-active :foreground ,white :weight bold
                              :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-inactive
     ((t (:background unspecified :foreground ,grey
                      :box (:line-width 1 :color ,non-text)))))
   `(tab-line-tab-modified
     ((t (:background unspecified :foreground ,green))))
   `(tab-line-close-highlight
     ((t (:background unspecified :foreground ,red))))
   `(help-key-binding
     ((,class256 (:background unspecified :foreground ,cyan :weight bold))))
   `(error
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(warning
     ((,class256 (:foreground ,yellow :weight bold))))
   `(success
     ((,class256 (:foreground ,bright-green :weight bold))))
   `(menu
     ((t (:background ,mode-line-inactive :foreground ,grey))))
   `(tty-menu-enabled-face
     ((t (:background ,menu :foreground ,grey))))
   `(tty-menu-disabled-face
     ((t (:background ,menu :foreground ,darkgrey))))
   `(tty-menu-selected-face
     ((t (:background ,grey2 :foreground ,grey :weight bold))))

   `(show-paren-match
     ((,class256 :foreground ,match-paren :weight bold)))
   `(show-paren-mismatch
     ((,class256 :background ,match-paren :foreground ,white :weight bold)))

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
     ((,class256 (:foreground ,bright-green :weight unspecified :slant unspecified))))
   `(font-lock-comment-face
     ((,class256 (:foreground ,comment :weight unspecified :slant unspecified))))
   `(font-lock-keyword-face
     ((,class256 (:foreground ,bright-blue :weight unspecified :slant unspecified))))
   `(font-lock-preprocessor-face
     ((,class256 (:foreground ,purple :weight unspecified :slant unspecified))))
   `(font-lock-builtin-face
     ((,class256 (:foreground ,magenta :weight unspecified :slant unspecified))))
   `(font-lock-type-face
     ((,class256 (:foreground ,bright-yellow :weight unspecified :slant unspecified))))
   `(font-lock-function-name-face
     ((,class256 (:foreground ,bright-magenta :weight unspecified :slant unspecified))))
   `(font-lock-variable-name-face
     ((,class256 (:foreground ,bright-cyan :weight unspecified :slant unspecified))))
   `(font-lock-constant-face
     ((,class256 (:foreground ,bright-red :weight unspecified :slant unspecified))))
   `(font-lock-warning-face
     ((,class256 (:foreground ,yellow :weight bold :slant unspecified))))

   `(font-lock-number-face
     ((,class256 (:foreground ,bright-red :weight unspecified :slant unspecified))))
   `(font-lock-escape-face
     ((,class256 (:foreground ,bright-yellow :weight unspecified :slant unspecified))))
   `(font-lock-function-call-face
     ((,class256 (:foreground ,magenta :weight unspecified :slant unspecified))))
   `(font-lock-regexp-face
     ((,class256 (:foreground ,green :weight unspecified :slant unspecified))))
   `(font-lock-delimiter-face
     ((,class256 (:foreground ,bright-magenta :weight unspecified :slant unspecified))))
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
     ((,class256 (:background ,black :foreground ,bright-yellow :inverse-video t))))
   `(isearch-group-1
     ((,class256 (:background ,black :foreground ,yellow2 :inverse-video t))))
   `(isearch-group-2
     ((,class256 (:background ,black :foreground ,yellow1 :inverse-video t))))
   `(lazy-highlight
     ((,class256 (:background ,black :foreground ,bright-green :inverse-video t))))
   `(isearch-fail
     ((,classTC (:background ,diff-removed-bg :foreground ,diff-removed-fg))
      (t (:background ,diff-removed-bg-tty :foreground ,diff-removed-fg))))
   `(hi-yellow
     ((,classTC (:background ,hi-yellow :foreground unspecified :inverse-video nil))))
   `(hi-pink
     ((,classTC (:background ,hi-pink :foreground unspecified :inverse-video nil))))
   `(hi-blue
     ((,classTC (:background ,hi-blue :foreground unspecified :inverse-video nil))))
   `(hi-green
     ((,classTC (:background ,hi-green :foreground unspecified :inverse-video nil))))
   `(hi-salmon
     ((,classTC (:background ,hi-salmon :foreground unspecified :inverse-video nil))))
   `(hi-aquamarine
     ((,classTC (:background ,hi-aquamarine :foreground unspecified :inverse-video nil))))
   `(hi-red-b
     ((,classTC (:background unspecified :foreground ,red :weight bold))))
   `(hi-green-b
     ((,classTC (:background unspecified :foreground ,green :weight bold))))
   `(hi-blue-b
     ((,classTC (:background unspecified :foreground ,blue :weight bold))))

   ;; replace.el
   `(match
     ((,class256 (:background ,match))))

   ;; global-hl-line-mode
   `(hl-line
     ((,class256 (:background ,hl-line :foreground unspecified))))

   ;; customize & widget
   `(custom-button
     ((,class256 (:background ,grey2 :foreground ,fg :extend t
                              :box (:line-width (2 . 2) :style released-button)))))
   `(custom-button-pressed
     ((,class256 (:background ,grey2 :foreground ,fg :extend t
                              :box (:line-width (2 . 2) :style pressed-button)))))
   `(custom-button-mouse
     ((,class256 (:background ,non-text :foreground ,fg :extend t
                              :box (:line-width (2 . 2) :style released-button)))))
   `(custom-state
     ((,class256 (:foreground ,green))))
   `(custom-group-tag
     ((,class256 (:foreground ,bright-magenta :weight bold))))
   `(custom-variable-tag
     ((,class256 (:background unspecified :foreground ,white :weight bold))))
   `(custom-comment
     ((,class256 (:background unspecified :foreground ,comment :weight unspecified))))
   `(custom-comment-tag
     ((,class256 (:background unspecified :foreground unspecified :weight unspecified))))
   `(widget-field
     ((,class256 (:background ,grey2 :foreground ,fg :extend t))))
   `(widget-inactive
     ((,class256 (:foreground ,darkgrey))))
   `(widget-button-pressed
     ((,class256 (:foreground ,red))))
   `(widget-documentation
     ((,class256 (:foreground ,green))))

   ;; shortdoc
   `(shortdoc-heading
     ((,class256 (:inherit default :weight bold :height 1.3))))
   `(shortdoc-section
     ((,class256 (:inherit default))))

   ;; package
   `(package-help-section-name
     ((,class256 (:foreground ,white :weight bold))))
   `(package-status-installed
     ((,class256 (:foreground ,darkgrey))))

   ;; dired
   `(dired-header
     ((,class256 (:foreground ,white :weight bold))))
   `(dired-directory
     ((,class256 (:inherit font-lock-keyword-face :weight bold))))
   `(dired-symlink
     ((,class256 (:foreground ,bright-cyan :weight bold :inherit nil))))
   `(dired-broken-symlink
     ((,class256 (:background ,red :foreground ,white))))
   `(dired-special
     ((,class256 (:foreground ,magenta :inherit nil))))
   `(dired-perm-write
     ((,class256 (:foreground ,yellow :inherit nil))))

   ;; completion
   `(icomplete-first-match
     ((,class256 (:foreground ,bright-green :weight bold))))
   `(icomplete-selected-match
     ((,class256 (:background ,hl-line))))
   `(completions-common-part
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(completions-first-difference
     ((,class256 (:foreground ,bright-blue :weight bold))))
   `(completions-annotations
     ((,class256 (:foreground ,darkgrey))))

   ;; ido
   `(ido-first-match
     ((,class256 (:foreground ,bright-green :weight bold))))
   `(ido-only-match
     ((,class256 (:inherit 'ido-first-match))))
   `(ido-virtual
     ((,class256 (:foreground ,darkgrey))))
   `(ido-subdir
     ((,class256 (:foreground ,white :weight bold))))
   ;; check how good it is
   `(ido-indicator
     ((,class256 (:background ,red :foreground ,bright-yellow))))

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
     ((,class256 (:foreground ,white :weight bold))))
   `(message-header-subject
     ((,class256 (:foreground ,white :weight bold))))
   `(message-header-to
     ((,class256 (:foreground ,white))))
   `(message-header-other
     ((,class256 (:foreground ,fg))))
   `(message-header-cc
     ((,class256 (:foreground ,darkgrey))))
   `(message-separator
     ((,class256 (:background ,grey2 :foreground ,fg :extend t))))
   `(message-mml
     ((,class256 (:foreground ,comment))))

   ;; bookmark
   `(bookmark-face
     ((,class256 (:background unspecified :foreground ,yellow))))

   ;; info
   `(info-title-1
     ((,class256 (:foreground ,white :weight bold))))
   `(info-title-2
     ((,class256 (:foreground ,white :weight bold))))
   `(info-title-3
     ((,class256 (:foreground ,white :weight bold))))
   `(info-title-4
     ((,class256 (:foreground ,white :weight bold))))
   `(info-menu-header
     ((,class256 (:inherit info-title-3))))
   `(info-node
     ((,class256 (:foreground ,bright-yellow :weight bold))))
   `(info-menu-star
     ((,class256 (:foreground ,bright-yellow))))

   ;; edmacro
   `(edmacro-label
     ((,class256 (:foreground ,bright-blue :weight bold))))

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
     ((,class256 (:foreground ,white :weight bold))))
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
   `(org-formula
     ((,class256 (:foreground ,bright-red))))
   `(org-footnote
     ((,class256 (:foreground ,darkgrey))))
   `(org-ellipsis
     ((,class256 (:foreground ,bright-yellow))))
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
     ((,class256 (:foreground ,red :weight bold))))
   `(org-done
     ((,class256 (:foreground ,green :weight bold))))
   `(org-date
     ((,class256 (:foreground ,darkgrey))))
   `(org-sexp-date
     ((,class256 (:foreground ,cyan))))
   `(org-headline-done
     ((,class256 (:foreground unspecified))))
   `(org-checkbox
     ((,class256 (:foreground ,darkgrey :weight normal))))
   `(org-dispatcher-highlight
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(org-agenda-structure
     ((,class256 (:foreground ,white :weight bold))))
   `(org-agenda-structure-filter
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(org-date-selected
     ((,class256 (:background ,bright-magenta :foreground ,bg))))
   `(org-agenda-date
     ((,class256 (:foreground ,bright-blue :weight normal))))
   `(org-agenda-date-today
     ((,class256 (:foreground ,bright-magenta :weight bold))))
   `(org-agenda-current-time
     ((,class256 (:foreground ,bright-magenta))))
   `(org-agenda-done
     ((,class256 (:foreground ,bright-green))))
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
     ((,class256 (:background ,bright-yellow :foreground ,black))))

   ;; gnus
   `(gnus-button
     ((,class256 (:inherit link))))
   `(gnus-group-mail-1
     ((,class256 (:foreground ,bright-green :weight bold))))
   `(gnus-group-mail-1-empty
     ((,class256 (:foreground ,bright-green))))
   `(gnus-group-mail-2
     ((,class256 (:foreground ,bright-yellow :weight bold))))
   `(gnus-group-mail-2-empty
     ((,class256 (:foreground ,bright-yellow))))
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
     ((,class256 (:foreground ,bright-green :weight bold))))
   `(gnus-group-news-1-empty
     ((,class256 (:foreground ,bright-green))))
   `(gnus-group-news-2
     ((,class256 (:foreground ,bright-yellow :weight bold))))
   `(gnus-group-news-2-empty
     ((,class256 (:foreground ,bright-yellow))))
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
     ((,class256 (:foreground ,white :weight bold))))
   `(gnus-summary-normal-read
     ((,class256 (:foreground ,fg))))
   `(gnus-summary-normal-ticked
     ((,class256 (:foreground ,bright-yellow))))
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
     ((,class256 (:foreground ,white :weight bold))))
   `(gnus-header-newsgroups
     ((,class256 (:foreground ,white :weight bold))))
   `(gnus-cite-attribution
     ((,class256 (:foreground ,bright-green :weight bold :slant normal :underline nil))))
   `(gnus-cite-1
     ((,class256 (:foreground ,bright-green))))
   `(gnus-cite-2
     ((,class256 (:foreground ,bright-yellow))))
   `(gnus-cite-3
     ((,class256 (:foreground ,bright-blue))))
   `(gnus-cite-4
     ((,class256 (:foreground ,bright-magenta))))
   `(gnus-cite-5
     ((,class256 (:foreground ,bright-cyan))))
   `(gnus-cite-6
     ((,class256 (:foreground ,green))))
   `(gnus-cite-7
     ((,class256 (:foreground ,yellow))))
   `(gnus-cite-8
     ((,class256 (:foreground ,blue))))
   `(gnus-cite-9
     ((,class256 (:foreground ,magenta))))
   `(gnus-cite-10
     ((,class256 (:foreground ,cyan))))
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
     ((,class256 (:foreground ,white :weight bold :height 1.6))))
   `(shr-h2
     ((,class256 (:foreground ,white :weight bold :height 1.4))))
   `(shr-h3
     ((,class256 (:foreground ,white :weight bold :height 1.2))))
   `(shr-h4
     ((,class256 (:foreground ,white :weight bold :height 1.1))))
   `(shr-h5
     ((,class256 (:foreground ,white :weight bold :height 1.0))))
   `(shr-h6
     ((,class256 (:foreground ,white :weight bold :height 1.0))))

   ;; dictionary
   `(dictionary-word-definition-face
     ((,class256 (:family nil))))
   `(dictionary-reference-face
     ((,class256 (:foreground ,yellow))))

   ;; markdown
   `(markdown-metadata-key-face
     ((,class256 (:foreground ,darkgrey))))
   `(markdown-header-face
     ((,class256 (:foreground ,white :weight bold))))
   `(markdown-header-delimiter-face
     ((,class256 (:foreground ,bright-blue :weight bold))))
   `(markdown-header-rule-face
     ((,class256 (:foreground ,bright-blue :weight bold))))
   `(markdown-code-face
     ((,classTC (:background ,code-block :foreground ,fg :extend t))
      (t (:foreground ,fg))))
   `(markdown-list-face
     ((,class256 (:foreground ,bright-yellow))))
   `(markdown-markup-face
     ((,class256 (:foreground ,darkgrey))))
   `(markdown-inline-code-face
     ((,classTC (:background ,code-block :foreground ,cyan))
      (t (:foreground ,green))))
   `(markdown-language-keyword-face
     ((,class256 (:foreground ,darkgrey))))
   `(markdown-gfm-checkbox-face
     ((,class256 (:foreground ,darkgrey))))

   ;; diff
   `(diff-header
     ((,class256 (:background ,grey2 :foreground ,bright-blue :weight bold))))
   `(diff-file-header
     ((,class256 (:background ,grey2 :foreground ,white))))
   `(diff-hunk-header
     ((,class256 (:foreground ,bright-yellow :weight bold))))
   `(diff-function
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(diff-added
     ((,classTC (:background ,diff-added-bg :foreground ,diff-added-fg))
      (t (:background ,diff-added-bg-tty :foreground ,diff-added-fg))))
   `(diff-indicator-added
     ((t (:inherit 'diff-added :foreground ,bright-green))))
   `(diff-refine-added
     ((,classTC (:background ,diff-refine-added-bg :foreground ,diff-added-fg))
      (t (:background ,diff-refine-added-bg-tty :foreground ,diff-added-fg))))
   `(diff-removed
     ((,classTC (:background ,diff-removed-bg :foreground ,diff-removed-fg))
      (t (:background ,diff-removed-bg-tty :foreground ,diff-removed-fg))))
   `(diff-refine-removed
     ((,classTC (:background ,diff-refine-removed-bg :foreground ,diff-removed-fg))
      (t (:background ,diff-refine-removed-bg-tty :foreground ,diff-removed-fg))))
   `(diff-indicator-removed
     ((t (:inherit 'diff-removed :foreground ,bright-red))))

   ;; vc
   `(vc-dir-header
     ((,class256 (:foreground ,bright-magenta))))
   `(vc-dir-header-value
     ((,class256 (:foreground ,fg))))
   `(vc-dir-directory
     ((,class256 (:foreground ,bright-blue :weight bold))))
   `(vc-dir-file
     ((,class256 (:foreground ,fg))))
   `(vc-dir-status-up-to-date
     ((,class256 (:foreground ,bright-green))))
   `(vc-dir-status-edited
     ((,class256 (:foreground ,bright-yellow))))
   `(vc-dir-mark-indicator
     ((,class256 (:foreground ,bright-red))))
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
     ((,class256 (:foreground ,bright-red))))
   `(log-edit-header
     ((,class256 (:foreground ,bright-magenta :weight bold))))
   `(log-edit-summary
     ((,class256 (:foreground ,white :weight bold))))
   `(log-edit-headers-separator
     ((,classTTY (:background unspecified))
      (t (:background ,darkgrey :height 0.1 :extend t))))
   `(log-view-message
     ((,class256 (:foreground ,darkgrey))))
   `(log-view-commit-body
     ((,class256 (:foreground ,fg))))

   ;; git-commit
   `(git-commit-summary
     ((,class256 (:foreground ,white :weight bold))))
   `(git-commit-nonempty-second-line
     ((,class256 (:foreground ,bright-red :weight bold))))

   ;; magit
   `(magit-section-heading
     ((,class256 (:foreground ,bright-yellow :weight bold))))
   `(magit-section-heading-selection
     ((,class256 (:foreground ,yellow))))
   `(magit-section-highlight
     ((,class256 (:background ,hl-line))))
   `(magit-branch-local
     ((,class256 (:foreground ,bright-blue))))
   `(magit-branch-remote
     ((,class256 (:foreground ,bright-green))))
   `(magit-tag
     ((,class256 (:foreground ,yellow))))
   `(magit-dimmed
     ((t (:foreground ,darkgrey))))
   `(magit-hash
     ((t (:foreground ,darkgrey))))
   `(magit-cherry-equivalent
     ((,class256 (:foreground ,bright-magenta))))
   `(magit-cherry-unmatched
     ((,class256 (:foreground ,bright-cyan))))
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
     ((t (:background ,grey2))))
   `(magit-diff-added
     ((t (:inherit 'diff-added))))
   `(magit-diff-added-highlight
     ((t (:inherit 'diff-added))))
   `(magit-diff-removed
     ((t (:inherit 'diff-removed))))
   `(magit-diff-removed-highlight
     ((t (:inherit 'diff-removed))))
   `(magit-diff-lines-heading
     ((,class256 (:background ,green :foreground ,white))))
   `(magit-diffstat-added
     ((,class256 (:foreground ,bright-green))))
   `(magit-diffstat-removed
     ((,class256 (:foreground ,bright-red))))
   `(magit-log-author
     ((,class256 (:foreground ,bright-red))))
   `(magit-log-graph
     ((,class256 (:foreground ,darkgrey))))
   `(magit-log-date
     ((,class256 (:foreground ,darkgrey))))
   `(magit-blame-name
     ((,class256 (:foreground ,bright-red))))
   `(magit-blame-date
     ((,class256 (:foreground ,cyan))))
   `(magit-blame-heading
     ((,class256 (:background ,grey1 :foreground ,white))))
   `(magit-blame-margin
     ((,class256 (:background ,grey1 :foreground ,white))))
   `(magit-blame-highlight
     ((,class256 (:background ,grey1 :foreground ,white))))
   `(magit-reflog-amend
     ((,class256 (:foreground ,magenta))))
   `(magit-reflog-merge
     ((,class256 (:foreground ,green))))
   `(magit-reflog-other
     ((,class256 (:foreground ,cyan))))
   `(magit-reflog-reset
     ((,class256 (:foreground ,red))))
   `(magit-reflog-commit
     ((,class256 (:foreground ,green))))
   `(magit-reflog-rebase
     ((,class256 (:foreground ,magenta))))
   `(magit-reflog-remote
     ((,class256 (:foreground ,cyan))))
   `(magit-reflog-checkout
     ((,class256 (:foreground ,blue))))
   `(magit-reflog-cherry-pick
     ((,class256 (:foreground ,green))))

   `(transient-key
     ((,class256 (:inherit help-key-binding))))
   `(transient-heading
     ((,class256 (:inherit magit-section-heading))))

   ;; ediff
   `(ediff-current-diff-A
     ((,classTC (:background ,diff-removed-bg))
      (t (:background ,diff-removed-bg-tty))))
   `(ediff-current-diff-B
     ((,classTC (:background ,diff-added-bg))
      (t (:background ,diff-added-bg-tty))))
   `(ediff-current-diff-C
     ((,classTC (:background ,diff-changed-bg))
      (t (:background ,diff-changed-bg-tty))))
   `(ediff-current-diff-Ancestor
     ((,classTC (:background ,diff-ancestor-bg))
      (t (:background ,diff-ancestor-bg-tty))))
   `(ediff-fine-diff-A
     ((,classTC (:background ,diff-refine-removed-bg :foreground ,diff-removed-fg))
      (t (:background ,diff-refine-removed-bg-tty :foreground ,diff-removed-fg))))
   `(ediff-fine-diff-B
     ((,classTC (:background ,diff-refine-added-bg :foreground ,diff-added-fg))
      (t (:background ,diff-refine-added-bg-tty :foreground ,diff-added-fg))))
   `(ediff-fine-diff-C
     ((,classTC (:background ,diff-refine-changed-bg :foreground ,diff-changed-fg))
      (t (:background ,diff-refine-changed-bg-tty :foreground ,diff-changed-fg))))
   `(ediff-fine-diff-Ancestor
     ((,classTC (:background ,diff-refine-ancestor-bg :foreground ,diff-ancestor-fg))
      (t (:background ,diff-refine-ancestor-bg-tty :foreground ,diff-ancestor-fg))))
   `(ediff-even-diff-A
     ((,class256 (:background ,grey2))))
   `(ediff-even-diff-B
     ((,class256 (:background ,grey2))))
   `(ediff-even-diff-C
     ((,class256 (:background ,grey2))))
   `(ediff-even-diff-Ancestor
     ((,class256 (:background ,grey2))))
   `(ediff-odd-diff-A
     ((,class256 (:background ,grey2))))
   `(ediff-odd-diff-B
     ((,class256 (:background ,grey2))))
   `(ediff-odd-diff-C
     ((,class256 (:background ,grey2))))
   `(ediff-odd-diff-Ancestor
     ((,class256 (:background ,grey2))))

   ;; smerge
   `(smerge-lower
     ((,classTC (:background ,diff-added-bg))
      (t (:background ,diff-added-bg-tty))))
   `(smerge-upper
     ((,classTC (:background ,diff-removed-bg))
      (t (:background ,diff-removed-bg-tty))))
   `(smerge-refined-added
     ((t (:background ,green :foreground ,black))))
   `(smerge-refined-removed
     ((t (:background ,red :foreground ,black))))

   ;; epa
   `(epa-mark
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(epa-string
     ((,class256 (:foreground ,bright-green))))
   `(epa-validity-high
     ((,class256 (:foreground ,bright-green :weight bold))))
   `(epa-validity-medium
     ((,class256 (:foreground ,bright-cyan :weight bold))))
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
     ((,classTC (:background ,diff-added-bg))
      (t (:background ,diff-added-bg-tty :foreground ,diff-added-fg))))
   `(wgrep-done-face
     ((,class256 (:foreground ,bright-yellow))))
   `(wgrep-file-face
     ((t (:inherit wgrep-face))))
   `(wgrep-delete-face
     ((,classTC (:background ,diff-removed-bg))
      (t (:background ,diff-removed-bg-tty :foreground ,diff-removed-fg))))
   `(wgrep-reject-face
     ((t (:inherit error))))

   ;; erc
   `(erc-timestamp-face
     ((,class256 (:foreground ,cyan))))
   `(erc-notice-face
     ((,class256 (:foreground ,darkgrey))))
   `(erc-my-nick-face
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(erc-current-nick-face
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(erc-nick-msg-face
     ((,class256 (:foreground ,bright-yellow))))
   `(erc-input-face
     ((,class256 (:foreground ,purple))))
   `(erc-error-face
     ((,class256 (:foreground ,red))))
   `(erc-dangerous-host-face
     ((,class256 (:foreground ,red))))
   `(erc-direct-msg-face
     ((,class256 (:foreground ,yellow))))
   `(erc-button
     ((,class256 (:background unspecified :foreground ,bright-blue :underline t))))
   `(erc-prompt-face
     ((,class256 (:background unspecified :foreground ,magenta :weight bold))))
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
     ((,class256 (:foreground ,white))))

   ;; rcirc
   `(rcirc-server
     ((,class256 (:foreground ,darkgrey))))
   `(rcirc-timestamp
     ((,class256 (:foreground ,cyan))))
   `(rcirc-prompt
     ((,class256 (:foreground ,magenta :weight bold))))
   `(rcirc-url
     ((,class256 (:background unspecified :foreground ,bright-blue :underline t))))
   `(rcirc-my-nick
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(rcirc-nick-in-message
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(rcirc-other-nick
     ((,class256 (:foreground ,bright-blue))))

   ;; calendar
   `(calendar-month-header
     ((,class256 (:foreground ,white :weight bold))))
   `(calendar-weekday-header
     ((,class256 (:foreground ,bright-blue))))
   `(calendar-weekend-header
     ((,class256 (:foreground ,bright-yellow))))
   `(calendar-today
     ((,class256 (:foreground ,bright-green))))
   `(holiday
     ((,class256 (:background ,magenta :foreground ,bg))))

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
     ((,class256 (:foreground ,grey))))
   `(elfeed-search-unread-title-face
     ((,class256 (:foreground ,white :weight bold))))
   `(elfeed-search-feed-face
     ((,class256 (:foreground ,green))))
   `(elfeed-search-tag-face
     ((,class256 (:foreground ,bright-yellow))))
   `(elfeed-search-unread-count-face
     ((,class256 (:foreground ,bright-blue))))

   ;; comint
   `(comint-highlight-prompt
     ((,class256 (:foreground ,magenta :weight bold))))

   ;; eshell
   `(eshell-prompt
     ((,class256 (:foreground ,magenta :weight bold))))
   `(eshell-ls-directory
     ((,class256 (:foreground ,bright-blue :weight bold))))
   `(eshell-ls-symlink
     ((,class256 (:foreground ,bright-cyan :weight bold))))
   `(eshell-ls-executable
     ((,class256 (:foreground ,bright-green :weight bold))))
   `(eshell-ls-clutter
     ((,class256 (:foreground ,red))))
   `(eshell-ls-archive
     ((,class256 (:foreground ,yellow))))
   `(eshell-ls-backup
     ((,class256 (:foreground ,darkgrey))))
   `(eshell-ls-unreadable
     ((,class256 (:foreground ,non-text))))
   `(eshell-ls-missing
     ((,class256 (:background ,red :foreground ,white))))
   `(eshell-ls-product
     ((,class256 (:foreground ,white))))
   `(eshell-ls-readonly
     ((,class256 (:foreground ,darkgrey))))
   `(eshell-ls-special
     ((,class256 (:foreground ,magenta))))

   ;; eww
   `(eww-form-text
     ((,class256 (:inherit widget-field :box (:foreground ,darkgrey)))))
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
     ((,class256 (:foreground ,bright-blue :weight bold))))
   `(emms-playlist-track-face
     ((,class256 (:foreground ,fg))))
   `(emms-browser-track-face
     ((,class256 (:inherit emms-playlist-track-face))))
   `(emms-browser-artist-face
     ((,class256 (:foreground ,blue))))
   `(emms-browser-album-face
     ((,class256 (:foreground ,bright-yellow))))
   `(emms-browser-composer-face
     ((,class256 (:foreground ,bright-cyan))))
   `(emms-browser-performer-face
     ((,class256 (:foreground ,bright-magenta))))
   `(emms-browser-year/genre-face
     ((,class256 (:foreground ,bright-red))))

   ;; vertico
   `(vertico-current
     ((,class256 (:background ,hl-line))))

   ;; orderless
   `(orderless-match-face-0
     ((,class256 (:foreground ,bright-red :weight bold))))
   `(orderless-match-face-1
     ((,class256 (:foreground ,bright-blue :weight bold))))
   `(orderless-match-face-2
     ((,class256 (:foreground ,bright-green :weight bold))))
   `(orderless-match-face-3
     ((,class256 (:foreground ,bright-yellow :weight bold))))

   ;; marginalia
   `(marginalia-key
     ((,class256 (:foreground ,cyan))))
   `(marginalia-date
     ((,class256 (:foreground ,darkgrey))))
   `(marginalia-file-priv-dir
     ((,class256 (:foreground ,darkgrey))))
   `(marginalia-file-priv-link
     ((,class256 (:foreground ,bright-cyan))))
   `(marginalia-file-priv-read
     ((,class256 (:foreground ,bright-magenta))))
   `(marginalia-file-priv-write
     ((,class256 (:foreground ,bright-blue))))
   `(marginalia-file-priv-exec
     ((,class256 (:foreground ,bright-yellow))))

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
     ((,class256 (:background ,grey1))))
   `(notmuch-tag-face
     ((,class256 (:foreground ,bright-yellow))))
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
     ((,class256 (:foreground ,bright-blue))))
   `(verb-header
     ((,class256 (:foreground ,bright-yellow))))
   `(verb-code-tag
     ((,class256 (:foreground ,cyan))))

   ;; sly
   `(sly-mrepl-output-face
     ((,class256 (:foreground ,fg))))
   `(sly-mrepl-note-face
     ((,class256 (:foreground ,yellow))))
   `(sly-action-face
     ((,class256 (:foreground ,blue :weight bold))))

   ;; corfu
   `(corfu-default
     ((,class256 (:background ,grey2))))
   `(corfu-current
     ((,class256 (:background ,grey1))))
   `(corfu-bar
     ((,class256 (:background ,non-text))))
   `(corfu-border
     ((,class256 (:background ,darkgrey))))

   ;; company
   `(company-tooltip
     ((,class256 (:background ,grey2))))
   `(company-tooltip-common
     ((,class256 (:inherit completions-common-part))))
   `(company-tooltip-annotation
     ((,class256 (:inherit completions-annotations))))
   `(company-tooltip-selection
     ((,class256 (:background ,grey1))))
   `(company-tooltip-scrollbar-track
     ((,class256 (:background ,non-text))))
   `(company-tooltip-scrollbar-thumb
     ((,class256 (:background ,darkgrey))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,class256 (:foreground ,fg))))
   `(rainbow-delimiters-depth-2-face
     ((,class256 (:foreground ,bright-blue))))
   `(rainbow-delimiters-depth-3-face
     ((,class256 (:foreground ,bright-yellow))))
   `(rainbow-delimiters-depth-4-face
     ((,class256 (:foreground ,bright-cyan))))
   `(rainbow-delimiters-depth-5-face
     ((,class256 (:foreground ,bright-magenta))))
   `(rainbow-delimiters-depth-6-face
     ((,class256 (:foreground ,blue))))
   `(rainbow-delimiters-depth-7-face
     ((,class256 (:foreground ,red))))
   `(rainbow-delimiters-depth-8-face
     ((,class256 (:foreground ,cyan))))
   `(rainbow-delimiters-depth-9-face
     ((,class256 (:foreground ,yellow))))

   ;; rst
   `(rst-level-1
     ((,class256 (:background unspecified :foreground ,white :weight bold))))
   `(rst-level-2
     ((,class256 (:background unspecified :foreground ,white :weight bold))))
   `(rst-level-3
     ((,class256 (:background unspecified :foreground ,white :weight bold))))
   `(rst-level-4
     ((,class256 (:background unspecified :foreground ,white :weight bold))))
   `(rst-level-5
     ((,class256 (:background unspecified :foreground ,white :weight bold))))
   `(rst-level-6
     ((,class256 (:background unspecified :foreground ,white :weight bold))))
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

   ;; tuareg
   `(tuareg-font-lock-error-face
     ((,class256 (:foreground ,red :background ,white :inverse-video t))))
   `(tuareg-font-lock-interactive-error-face
     ((,class256 (:inherit error))))
   `(tuareg-font-double-semicolon-face
     ((,class256 (:foreground ,red))))
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
     ((,class256 (:foreground ,magenta))))

   ;; tempel
   `(tempel-default
     ((,classTC (:background ,diff-added-bg))
      (t (:background ,diff-added-bg-tty))))
   `(tempel-field
     ((,classTC (:background ,diff-added-bg))
      (t (:background ,diff-added-bg-tty))))
   `(tempel-form
     ((,class256 (:background unspecified))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'wildcharm)
;;; wildcharm-theme.el ends here
