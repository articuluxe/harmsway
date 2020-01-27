;;; modus-operandi-theme.el --- Accessible light theme (WCAG AAA) -*- lexical-binding:t -*-

;; Copyright (c) 2019-2020 Protesilaos Stavrou <info@protesilaos.com>

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/modus-themes
;; Version: 0.5.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces, theme, accessibility

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This theme is designed for colour-contrast accessibility.
;;
;; 1. Provide a consistent minimum contrast ratio between background and
;; foreground values of 7:1 or higher.  This meets the highest such
;; accessibility criterion per the guidelines of the Worldwide Web
;; Consortium's Working Group on Accessibility (WCAG AAA standard).
;;
;; 2. Offer as close to full face coverage as possible.  The list is
;; already quite long (see further below), with more additions to follow
;; as part of the ongoing development process.
;;
;; The theme provides the following customisation options, all of which
;; are disabled by default:
;;
;;     modus-operandi-theme-slanted-constructs
;;     modus-operandi-theme-bold-constructs
;;     modus-operandi-theme-proportional-fonts
;;     modus-operandi-theme-scale-headings
;;
;; The default scale is as follows (it can be customised as well):
;;
;;     modus-operandi-theme-scale-1 1.05
;;     modus-operandi-theme-scale-2 1.1
;;     modus-operandi-theme-scale-3 1.15
;;     modus-operandi-theme-scale-4 1.2
;;
;; What follows is the list of explicitly supported packages or face
;; groups (there are implicitly supported packages as well, which
;; inherit from font-lock or some basic group).  You are encouraged to
;; notify me of any missing package or change you would like to see.
;;
;;     all-the-icons
;;     annotate
;;     anzu
;;     apt-sources-list
;;     artbollocks-mode
;;     auto-dim-other-buffers
;;     avy
;;     ace-window
;;     calendar
;;     calfw
;;     column-enforce-mode
;;     company-mode
;;     company-posframe
;;     compilation-mode
;;     completions
;;     counsel
;;     counsel-css
;;     counsel-notmuch
;;     counsel-org-capture-string
;;     cov
;;     custom (M-x customize)
;;     dashboard (emacs-dashboard)
;;     deadgrep
;;     define-word
;;     disk-usage
;;     diff-hl
;;     diff-mode
;;     dired
;;     dired-async
;;     dired-git-info
;;     dired-narrow
;;     dired-subtree
;;     diredfl
;;     doom-modeline
;;     easy-jekyll
;;     easy-kill
;;     ediff
;;     eldoc-box
;;     elfeed
;;     emms
;;     epa
;;     equake
;;     erc
;;     ert
;;     eshell
;;     evil (evil-mode)
;;     evil-goggles
;;     evil-visual-mark-mode
;;     eww
;;     eyebrowse
;;     fancy-dabbrev
;;     focus
;;     font-lock (generic syntax highlighting)
;;     flycheck
;;     flycheck-indicator
;;     flycheck-posframe
;;     flymake
;;     flyspell
;;     fountain (fountain-mode)
;;     geiser
;;     git
;;     git-gutter (and variants)
;;     git-lens
;;     git-timemachine
;;     gnus
;;     helm
;;     hydra
;;     highlight-blocks
;;     hl-line-mode
;;     hl-fill-column
;;     ido-mode
;;     iedit
;;     info
;;     info-colors
;;     isearch, occur, etc.
;;     ivy
;;     ivy-posframe
;;     keycast
;;     line numbers (`display-line-numbers-mode' and global variant)
;;     lsp-mode
;;     lsp-ui
;;     magit
;;     markdown-mode
;;     mentor
;;     messages
;;     modeline
;;     mood-line
;;     mu4e
;;     mu4e-conversation
;;     neotree
;;     org
;;     org-journal
;;     org-noter
;;     org-recur
;;     outline-mode
;;     package (M-x list-packages)
;;     paren-face
;;     pass
;;     persp-mode
;;     perspective
;;     powerline
;;     powerline-evil
;;     proced
;;     prodigy
;;     rainbow-blocks
;;     rainbow-delimiters
;;     regexp-builder (also known as `re-builder')
;;     ruler-mode
;;     shell-script-mode
;;     show-paren-mode
;;     smart-mode-line
;;     smartparens
;;     smerge
;;     speedbar
;;     suggest
;;     swiper
;;     sx
;;     telephone-line
;;     term
;;     transient (pop-up windows like Magit's)
;;     treemacs
;;     undo-tree
;;     vc (built-in mode line status for version control)
;;     visual-regexp
;;     wgrep
;;     which-function-mode
;;     which-key
;;     whitespace-mode
;;     writegood-mode
;;     xah-elisp-mode
;;     xterm-color (and ansi-colors)
;;     ztree

;;; Code:

(deftheme modus-operandi
  "Light theme that conforms with the highest accessibility
  standard for colour contrast between background and
  foreground elements (WCAG AAA).")

;; These faces will be inherited by actual constructs.  They are meant
;; for those cases where a face needs to distinguish its output from
;; the rest of the text, such as `isearch' and `occur'…  We define
;; these separately in order to combine each colour with its
;; appropriate foreground value.  This is to ensure a consistent
;; contrast ratio of >= 7:1.
(defgroup modus-theme ()
  "Theme that ensures WCAG AAA accessibility (contrast ratio
between foreground and background is >= 7:1)."
  :group 'faces
  :prefix "modus-theme-"
  :link '(url-link :tag "GitLab" "https://gitlab.com/protesilaos/modus-themes")
  :tag "Modus Operandi")

(defface modus-theme-subtle-red nil t)
(defface modus-theme-subtle-green nil t)
(defface modus-theme-subtle-yellow nil t)
(defface modus-theme-subtle-blue nil t)
(defface modus-theme-subtle-magenta nil t)
(defface modus-theme-subtle-cyan nil t)
(defface modus-theme-subtle-neutral nil t)
(defface modus-theme-intense-red nil t)
(defface modus-theme-intense-green nil t)
(defface modus-theme-intense-yellow nil t)
(defface modus-theme-intense-blue nil t)
(defface modus-theme-intense-magenta nil t)
(defface modus-theme-intense-cyan nil t)
(defface modus-theme-intense-neutral nil t)
(defface modus-theme-refine-red nil t)
(defface modus-theme-refine-green nil t)
(defface modus-theme-refine-yellow nil t)
(defface modus-theme-refine-blue nil t)
(defface modus-theme-refine-magenta nil t)
(defface modus-theme-refine-cyan nil t)
(defface modus-theme-active-red nil t)
(defface modus-theme-active-green nil t)
(defface modus-theme-active-yellow nil t)
(defface modus-theme-active-blue nil t)
(defface modus-theme-active-magenta nil t)
(defface modus-theme-active-cyan nil t)
(defface modus-theme-special-cold nil t)
(defface modus-theme-special-mild nil t)
(defface modus-theme-special-warm nil t)
(defface modus-theme-special-calm nil t)

;; User-facing customisation options.  They are all deactivated by
;; default (users must opt in).
(defcustom modus-operandi-theme-slanted-constructs nil
  "Use slanted text in more code constructs (italics or oblique)."
  :type 'boolean
  :group 'modus-theme)

(defcustom modus-operandi-theme-bold-constructs nil
  "Use bold text in more code constructs."
  :type 'boolean
  :group 'modus-theme)

(defcustom modus-operandi-theme-proportional-fonts nil
  "Use proportional fonts (variable-pitch) in headings."
  :type 'boolean
  :group 'modus-theme)

(defcustom modus-operandi-theme-scale-headings nil
  "Use font scaling for headings."
  :type 'boolean
  :group 'modus-theme)

(defcustom modus-operandi-theme-scale-1 1.05
  "Font size that is slightly larger than the base value."
  :type 'number
  :group 'modus-theme)

(defcustom modus-operandi-theme-scale-2 1.1
  "Font size that is slightly larger than `modus-theme-scale-1'."
  :type 'number
  :group 'modus-theme)

(defcustom modus-operandi-theme-scale-3 1.15
  "Font size that is slightly larger than `modus-theme-scale-2'."
  :type 'number
  :group 'modus-theme)

(defcustom modus-operandi-theme-scale-4 1.2
  "Font size that is slightly larger than `modus-theme-scale-3'."
  :type 'number
  :group 'modus-theme)

;; Define colour palette.  Each colour must have a >= 7:1 contrast
;; ratio relative to the foreground/background colour it is rendered
;; against.
(let ((class '((class color) (min-colors 89)))
      (fg-main "#000000") (bg-main "#ffffff")
      (fg-alt "#505050") (bg-alt "#f3f1f3")
      (fg-dim "#282828") (bg-dim "#f8f8f8")
      ;; specifically for on/off states (e.g. `mode-line')
      ;;
      ;; must be combined with themselves
      (fg-active "#191919") (bg-active "#e0e0e0")
      (fg-inactive "#424242") (bg-inactive "#efedef")
      ;; special base values, used only for cases where the above
      ;; fg-* or bg-* cannot or should not be used (to avoid confusion)
      ;; must be combined with: {fg,bg}-{main,alt,dim}
      (fg-special-cold "#093060") (bg-special-cold "#dde3f4")
      (fg-special-mild "#184034") (bg-special-mild "#c4ede0")
      (fg-special-warm "#5d3026") (bg-special-warm "#f0e0d4")
      (fg-special-calm "#61284f") (bg-special-calm "#f8ddea")
      ;; styles for the main constructs
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
      (red "#a80000") (green "#005200")
      (yellow "#8b3800") (blue "#0030a6")
      (magenta "#721045") (cyan "#005589")
      ;; styles for common, but still specialised constructs
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
      (red-alt "#880000") (green-alt "#4a5700")
      (yellow-alt "#714900") (blue-alt "#223fbf")
      (magenta-alt "#8f0075") (cyan-alt "#185870")
      ;; same purpose as above, just slight differences
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
      (red-alt-other "#9d2020") (green-alt-other "#145a00")
      (yellow-alt-other "#804000") (blue-alt-other "#0000bb")
      (magenta-alt-other "#5317ac") (cyan-alt-other "#005a68")
      ;; styles for elements that should draw attention to themselves
      ;;
      ;; must be combined with: `bg-main'
      (red-intense "#b60000") (green-intense "#006800")
      (yellow-intense "#904200") (blue-intense "#1111ee")
      (magenta-intense "#7000e0") (cyan-intense "#205b93")
      ;; styles for background elements that should be visible yet
      ;; subtle
      ;;
      ;; must be combined with: `fg-dim'
      (red-subtle-bg "#f2b0a2") (green-subtle-bg "#aecf90")
      (yellow-subtle-bg "#e4c340") (blue-subtle-bg "#b5d0ff")
      (magenta-subtle-bg "#f0d3ff") (cyan-subtle-bg "#c0efff")
      ;; styles for background elements that should be visible and
      ;; distinguishable
      ;;
      ;; must be combined with: `fg-main'
      (red-intense-bg "#ff8892") (green-intense-bg "#5ada88")
      (yellow-intense-bg "#f5df23") (blue-intense-bg "#6aaeff")
      (magenta-intense-bg "#d5baff") (cyan-intense-bg "#42cbd4")
      ;; styles for refined git diffs and other contexts where both the
      ;; foreground and the background need to have the same/similar hue
      ;;
      ;; must be combined with themselves OR the foregrounds can be
      ;; combined with any of the base backgrounds
      (red-refine-bg "#ffcccc") (green-refine-bg "#aceaac")
      (yellow-refine-bg "#fff29a") (blue-refine-bg "#8ac7ff")
      (magenta-refine-bg "#ffccff") (cyan-refine-bg "#8eecf4")
      (red-refine-fg "#780000") (green-refine-fg "#004c00")
      (yellow-refine-fg "#604000") (blue-refine-fg "#002288")
      (magenta-refine-fg "#770077") (cyan-refine-fg "#004850")
      ;; styles that are meant exclusively for the mode line
      ;;
      ;; must be combined with: `bg-active', `bg-inactive'
      (red-active "#930000") (green-active "#005300")
      (yellow-active "#703700") (blue-active "#0033c0")
      (magenta-active "#6320a0") (cyan-active "#004882")

      ;; styles reserved for specific faces
      ;;
      ;; `bg-hl-line' is between `bg-dim' and `bg-alt', so it should
      ;; work with all accents that cover those two, plus `bg-main'
      ;;
      ;; `bg-header' is between `bg-active' and `bg-inactive', so it
      ;; can be combined with any of the "active" values, plus the
      ;; "special" and base foreground colours
      ;;
      ;; `bg-region' must be combined with `fg-main'
      ;;
      ;; all other pairs are combinable with themselves
      (bg-hl-line "#f1f2f6")
      (bg-region "#bcbcbc")
      (fg-header "#2a2a2a") (bg-header "#e5e5e5")
      (fg-whitespace "#645060") (bg-whitespace "#fff8fc")
      (fg-paren-match "#222222") (bg-paren-match "#deb8af")

      ;; conditional styles that evaluate user-facing customisation
      ;; options
      (modus-theme-slant
       (if modus-operandi-theme-slanted-constructs
           'italic
         'normal))

      (modus-theme-bold
       (if modus-operandi-theme-bold-constructs
           'bold
         'normal))

      (modus-theme-variable-pitch
       (if modus-operandi-theme-proportional-fonts
           'variable-pitch
         'default)))
  (custom-theme-set-faces
   'modus-operandi
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; custom faces that are inherited by other constructs below ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;; subtle coloured backgrounds
   `(modus-theme-subtle-red ((,class (:background ,red-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-green ((,class (:background ,green-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-yellow ((,class (:background ,yellow-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-blue ((,class (:background ,blue-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-magenta ((,class (:background ,magenta-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-cyan ((,class (:background ,cyan-subtle-bg :foreground ,fg-dim))))
   `(modus-theme-subtle-neutral ((,class (:background ,bg-inactive :foreground ,fg-inactive))))
   ;;; intense coloured backgrounds
   `(modus-theme-intense-red ((,class (:background ,red-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-green ((,class (:background ,green-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-yellow ((,class (:background ,yellow-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-blue ((,class (:background ,blue-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-magenta ((,class (:background ,magenta-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-cyan ((,class (:background ,cyan-intense-bg :foreground ,fg-main))))
   `(modus-theme-intense-neutral ((,class (:background ,bg-active :foreground ,fg-main))))
   ;;; refined background and foreground combinations
   `(modus-theme-refine-red ((,class (:background ,red-refine-bg :foreground ,red-refine-fg))))
   `(modus-theme-refine-green ((,class (:background ,green-refine-bg :foreground ,green-refine-fg))))
   `(modus-theme-refine-yellow ((,class (:background ,yellow-refine-bg :foreground ,yellow-refine-fg))))
   `(modus-theme-refine-blue ((,class (:background ,blue-refine-bg :foreground ,blue-refine-fg))))
   `(modus-theme-refine-magenta ((,class (:background ,magenta-refine-bg :foreground ,magenta-refine-fg))))
   `(modus-theme-refine-cyan ((,class (:background ,cyan-refine-bg :foreground ,cyan-refine-fg))))
   ;;; invert the colours used on the "active" backgrounds
   ;;; mostly for use on the mode line
   `(modus-theme-active-red ((,class (:background ,red-active :foreground ,bg-active))))
   `(modus-theme-active-green ((,class (:background ,green-active :foreground ,bg-active))))
   `(modus-theme-active-yellow ((,class (:background ,yellow-active :foreground ,bg-active))))
   `(modus-theme-active-blue ((,class (:background ,blue-active :foreground ,bg-active))))
   `(modus-theme-active-magenta ((,class (:background ,magenta-active :foreground ,bg-active))))
   `(modus-theme-active-cyan ((,class (:background ,cyan-active :foreground ,bg-active))))
   ;;; special base values that are closer to the grayscale than
   ;;; the accents defined above
   `(modus-theme-special-cold ((,class (:background ,bg-special-cold :foreground ,fg-special-cold))))
   `(modus-theme-special-mild ((,class (:background ,bg-special-mild :foreground ,fg-special-mild))))
   `(modus-theme-special-warm ((,class (:background ,bg-special-warm :foreground ,fg-special-warm))))
   `(modus-theme-special-calm ((,class (:background ,bg-special-calm :foreground ,fg-special-calm))))
   ;;;;;;;;;;;;;;;;;;;
   ;; actual styles ;;
   ;;;;;;;;;;;;;;;;;;;
   ;;; default constructs
   ;;;; absolute essentials
   `(default ((,class (:background ,bg-main :foreground ,fg-main))))
   `(cursor ((,class (:background ,fg-main))))
   `(fringe ((,class (:background ,bg-main :foreground ,fg-main))))
   ;;;; basic and/or ungrouped styles
   `(diary ((,class (:foreground ,yellow))))
   `(error ((,class (:foreground ,red :weight bold))))
   `(escape-glyph ((,class (:inherit modus-theme-refine-blue :weight bold))))
   `(header-line ((,class (:background ,bg-header :foreground ,fg-header))))
   `(holiday ((,class (:inherit modus-theme-subtle-magenta))))
   `(homoglyph ((,class (:foreground ,yellow-alt-other))))
   `(ibuffer-locked-buffer ((,class (:foreground ,yellow-alt-other))))
   `(italic ((,class (:foreground ,fg-special-cold :slant italic))))
   `(no-emoji ((,class (:foreground ,fg-special-mild :weight bold))))
   `(nobreak-hyphen ((,class (:inherit modus-theme-refine-cyan))))
   `(nobreak-space ((,class (:inherit modus-theme-refine-cyan :underline t))))
   `(minibuffer-prompt ((,class (:foreground ,cyan-alt))))
   `(mm-command-output ((,class (:foreground ,red-alt-other))))
   `(mm-uu-extract ((,class (:inherit modus-theme-refine-yellow))))
   `(next-error ((,class (:inherit modus-theme-subtle-red))))
   `(shadow ((,class (:foreground ,fg-alt))))
   `(success ((,class (:foreground ,green :weight bold))))
   `(trailing-whitespace ((,class (:background ,red-alt))))
   `(warning ((,class (:foreground ,yellow :weight bold))))
   ;;;; all-the-icons
   `(all-the-icons-blue ((,class (:foreground ,blue))))
   `(all-the-icons-blue-alt ((,class (:foreground ,blue-alt))))
   `(all-the-icons-cyan ((,class (:foreground ,cyan))))
   `(all-the-icons-cyan-alt ((,class (:foreground ,cyan-alt))))
   `(all-the-icons-dblue ((,class (:foreground ,blue-alt-other))))
   `(all-the-icons-dcyan ((,class (:foreground ,cyan-alt-other))))
   `(all-the-icons-dgreen ((,class (:foreground ,green-alt-other))))
   `(all-the-icons-dired-dir-face ((,class (:foreground ,blue))))
   `(all-the-icons-dmaroon ((,class (:foreground ,magenta-alt-other))))
   `(all-the-icons-dorange ((,class (:foreground ,red-alt-other))))
   `(all-the-icons-dpink ((,class (:foreground ,magenta))))
   `(all-the-icons-dpurple ((,class (:foreground ,magenta-alt))))
   `(all-the-icons-dred ((,class (:foreground ,red))))
   `(all-the-icons-dsilver ((,class (:foreground ,fg-special-cold))))
   `(all-the-icons-dyellow ((,class (:foreground ,yellow))))
   `(all-the-icons-green ((,class (:foreground ,green))))
   `(all-the-icons-lblue ((,class (:foreground ,blue-refine-fg))))
   `(all-the-icons-lcyan ((,class (:foreground ,cyan-refine-fg))))
   `(all-the-icons-lgreen ((,class (:foreground ,green-refine-fg))))
   `(all-the-icons-lmaroon ((,class (:foreground ,magenta-refine-fg))))
   `(all-the-icons-lorange ((,class (:foreground ,red-refine-fg))))
   `(all-the-icons-lpink ((,class (:foreground ,magenta-refine-fg))))
   `(all-the-icons-lpurple ((,class (:foreground ,magenta-refine-fg))))
   `(all-the-icons-lred ((,class (:foreground ,red-refine-fg))))
   `(all-the-icons-lsilver ((,class (:foreground ,fg-special-cold))))
   `(all-the-icons-lyellow ((,class (:foreground ,yellow-refine-fg))))
   `(all-the-icons-maroon ((,class (:foreground ,magenta))))
   `(all-the-icons-orange ((,class (:foreground ,red-alt))))
   `(all-the-icons-pink ((,class (:foreground ,magenta))))
   `(all-the-icons-purple ((,class (:foreground ,magenta-alt))))
   `(all-the-icons-purple-alt ((,class (:foreground ,magenta-alt-other))))
   `(all-the-icons-red ((,class (:foreground ,red))))
   `(all-the-icons-red-alt ((,class (:foreground ,red-alt))))
   `(all-the-icons-silver ((,class (:foreground ,fg-special-cold))))
   `(all-the-icons-yellow ((,class (:foreground ,yellow))))
   ;;;; annotate
   `(annotate-annotation ((,class (:inherit modus-theme-intense-blue))))
   `(annotate-annotation-secondary ((,class (:inherit modus-theme-intense-yellow))))
   `(annotate-highlight ((,class (:underline (:color ,blue-intense :style line)))))
   `(annotate-highlight-secondary ((,class (:underline (:color ,yellow-intense :style line)))))
   ;;;; anzu
   `(anzu-match-1 ((,class (:inherit modus-theme-subtle-cyan))))
   `(anzu-match-2 ((,class (:inherit modus-theme-subtle-green))))
   `(anzu-match-3 ((,class (:inherit modus-theme-subtle-yellow))))
   `(anzu-mode-line ((,class (:foreground ,green-active :weight bold))))
   `(anzu-mode-line-no-match ((,class (:foreground ,red-active :weight bold))))
   `(anzu-replace-highlight ((,class (:inherit modus-theme-refine-yellow :underline t))))
   `(anzu-replace-to ((,class (:inherit modus-theme-intense-green :weight bold))))
   ;;;; apt-sources-list
   `(apt-sources-list-components ((,class (:foreground ,cyan))))
   `(apt-sources-list-options ((,class (:foreground ,yellow))))
   `(apt-sources-list-suite ((,class (:foreground ,green))))
   `(apt-sources-list-type ((,class (:foreground ,magenta))))
   `(apt-sources-list-uri ((,class (:foreground ,blue))))
   ;;;; artbollocks-mode
   `(artbollocks-face ((,class (:background ,bg-alt :foreground ,magenta-alt-other :underline t))))
   `(artbollocks-lexical-illusions-face ((,class (:inherit modus-theme-refine-magenta :underline t))))
   `(artbollocks-passive-voice-face ((,class (:background ,bg-alt :foreground ,cyan-alt-other :underline t))))
   `(artbollocks-weasel-words-face ((,class (:background ,bg-alt :foreground ,yellow-alt-other :underline t))))
   ;;;; auto-dim-other-buffers
   `(auto-dim-other-buffers-face ((,class (:background ,bg-alt))))
   ;;;; avy
   `(avy-background-face ((,class (:background ,bg-dim :foreground ,fg-dim))))
   `(avy-goto-char-timer-face ((,class (:inherit modus-theme-intense-yellow :weight bold))))
   `(avy-lead-face ((,class (:inherit modus-theme-intense-magenta :weight bold))))
   `(avy-lead-face-0 ((,class (:inherit modus-theme-intense-blue :weight bold))))
   `(avy-lead-face-1 ((,class (:inherit modus-theme-intense-red :weight bold))))
   `(avy-lead-face-2 ((,class (:inherit modus-theme-intense-green :weight bold))))
   ;;;; aw (ace-window)
   `(aw-background-face ((,class (:background ,bg-dim :foreground ,fg-dim))))
   `(aw-key-face ((,class (:foreground ,blue-intense :weight bold))))
   `(aw-leading-char-face ((,class (:height 1.5 :background ,bg-main :foreground ,red-intense :weight bold))))
   `(aw-minibuffer-leading-char-face ((,class (:foreground ,magenta-active))))
   `(aw-mode-line-face ((,class (:weight bold))))
   ;;;; buttons, links, widgets
   `(button ((,class (:foreground ,blue-alt-other :underline t))))
   `(link ((,class (:foreground ,blue-alt-other :underline t))))
   `(link-visited ((,class (:foreground ,magenta-alt-other :underline t))))
   `(tooltip ((,class (:inherit modus-theme-subtle-yellow))))
   `(widget-button ((,class (:inherit button))))
   `(widget-button-pressed ((,class (:inherit button :foreground ,magenta))))
   `(widget-documentation ((,class (:foreground ,green))))
   `(widget-field ((,class (:background ,bg-alt :foreground ,fg-dim))))
   `(widget-inactive ((,class (:background ,bg-inactive :foreground ,fg-inactive))))
   `(widget-single-line-field ((,class (:inherit widget-field))))
   ;;;; calendar
   `(calendar-today ((,class (:underline t))))
   `(calendar-weekday-header ((,class (:foreground ,blue-alt-other))))
   `(calendar-weekend-header ((,class (:foreground ,fg-alt :slant ,modus-theme-slant))))
   ;;;; calfw
   `(cfw:face-annotation ((,class (:background ,bg-alt :foreground ,fg-alt))))
   `(cfw:face-day-title ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(cfw:face-default-content ((,class (:foreground ,green-alt))))
   `(cfw:face-default-day ((,class (:inherit cfw:face-day-title :weight bold))))
   `(cfw:face-disable ((,class (:background ,bg-inactive :foreground ,fg-inactive))))
   `(cfw:face-grid ((,class (:foreground ,fg-inactive))))
   `(cfw:face-header ((,class (::foreground ,fg-main :weight bold))))
   `(cfw:face-holiday ((,class (:background ,bg-alt :foreground ,magenta :weight bold))))
   `(cfw:face-periods ((,class (:foreground ,cyan-alt-other))))
   `(cfw:face-saturday ((,class (:background ,bg-alt :foreground ,magenta-alt :weight bold))))
   `(cfw:face-select ((,class (:inherit modus-theme-intense-blue))))
   `(cfw:face-sunday ((,class (:background ,bg-alt :foreground ,magenta-alt-other :weight bold))))
   `(cfw:face-title ((,class (:inherit ,modus-theme-variable-pitch
                                       :foreground ,fg-special-warm :weight bold
                                       ,@(when modus-operandi-theme-scale-headings
                                           (list :height modus-operandi-theme-scale-4))))))
   `(cfw:face-today ((,class (:foreground ,blue :weight bold))))
   `(cfw:face-today-title ((,class (:inherit modus-theme-special-mild :box t))))
   `(cfw:face-toolbar ((,class (:background ,bg-active :foreground ,bg-active))))
   `(cfw:face-toolbar-button-off ((,class (:background ,bg-alt :foreground ,cyan))))
   `(cfw:face-toolbar-button-on ((,class (:background ,bg-main :foreground ,blue-intense :weight bold))))
   ;;;; column-enforce-mode
   `(column-enforce-face ((,class (:inherit modus-theme-refine-yellow))))
   ;;;; company-mode
   `(company-echo-common ((,class (:foreground ,magenta-alt-other))))
   `(company-preview ((,class (:background ,bg-dim :foreground ,fg-dim))))
   `(company-preview-common ((,class (:foreground ,blue-alt))))
   `(company-preview-search ((,class (:inherit modus-theme-special-calm))))
   `(company-scrollbar-bg ((,class (:background ,bg-active))))
   `(company-scrollbar-fg ((,class (:background ,fg-active))))
   `(company-template-field ((,class (:inherit modus-theme-intense-magenta))))
   `(company-tooltip ((,class (:background ,bg-alt :foreground ,fg-alt))))
   `(company-tooltip-annotation ((,class (:foreground ,fg-special-cold :slant ,modus-theme-slant))))
   `(company-tooltip-annotation-selection ((,class (:foreground ,fg-main :weight bold))))
   `(company-tooltip-common ((,class (:foreground ,blue-alt :weight bold))))
   `(company-tooltip-common-selection ((,class (:foreground ,fg-main))))
   `(company-tooltip-mouse ((,class (:inherit modus-theme-intense-blue))))
   `(company-tooltip-search ((,class (:inherit modus-theme-refine-cyan :weight bold))))
   `(company-tooltip-search-selection ((,class (:inherit modus-theme-intense-green :weight bold :underline t))))
   `(company-tooltip-selection ((,class (:inherit modus-theme-subtle-cyan :weight bold))))
   ;;;; company-posframe
   `(company-posframe-active-backend-name ((,class (:background ,bg-active :foreground ,blue-active :weight bold))))
   `(company-posframe-inactive-backend-name ((,class (:background ,bg-active :foreground ,fg-active))))
   `(company-posframe-metadata ((,class (:background ,bg-inactive :foreground ,fg-inactive))))
   ;;;; compilation feedback
   `(compilation-column-number ((,class (:foreground ,magenta-alt-other))))
   `(compilation-error ((,class (:foreground ,red :weight bold))))
   `(compilation-info ((,class (:foreground ,fg-special-cold))))
   `(compilation-line-number ((,class (:foreground ,magenta-alt))))
   `(compilation-mode-line-exit ((,class (:foreground ,blue-active :weight bold))))
   `(compilation-mode-line-fail ((,class (:foreground ,red-active :weight bold))))
   `(compilation-mode-line-run ((,class (:foreground ,magenta-active :weight bold))))
   `(compilation-warning ((,class (:foreground ,yellow :weight bold))))
   ;;;; completions
   `(completions-annotations ((,class (:foreground ,fg-special-cold :slant ,modus-theme-slant))))
   `(completions-common-part ((,class nil)))
   `(completions-first-difference ((,class (:weight bold))))
   ;;;; counsel
   `(counsel-active-mode ((,class (:foreground ,magenta-alt-other))))
   `(counsel-application-name ((,class (:foreground ,red-alt-other))))
   `(counsel-key-binding ((,class (:foreground ,blue-alt-other :weight bold))))
   `(counsel-outline-1 ((,class (:inherit outline-1))))
   `(counsel-outline-2 ((,class (:inherit outline-2))))
   `(counsel-outline-3 ((,class (:inherit outline-3))))
   `(counsel-outline-4 ((,class (:inherit outline-4))))
   `(counsel-outline-5 ((,class (:inherit outline-5))))
   `(counsel-outline-6 ((,class (:inherit outline-6))))
   `(counsel-outline-7 ((,class (:inherit outline-7))))
   `(counsel-outline-8 ((,class (:inherit outline-8))))
   `(counsel-outline-default ((,class (:foreground ,green-alt-other :weight bold))))
   `(counsel-variable-documentation ((,class (:foreground ,yellow-alt-other :slant ,modus-theme-slant))))
   ;;;; counsel-css
   `(counsel-css-selector-depth-face-1 ((,class (:foreground ,blue))))
   `(counsel-css-selector-depth-face-2 ((,class (:foreground ,cyan))))
   `(counsel-css-selector-depth-face-3 ((,class (:foreground ,green))))
   `(counsel-css-selector-depth-face-4 ((,class (:foreground ,yellow))))
   `(counsel-css-selector-depth-face-5 ((,class (:foreground ,magenta))))
   `(counsel-css-selector-depth-face-6 ((,class (:foreground ,red))))
   ;;;; counsel-notmuch
   `(counsel-notmuch-count-face ((,class (:foreground ,cyan))))
   `(counsel-notmuch-date-face ((,class (:foreground ,blue))))
   `(counsel-notmuch-people-face ((,class (:foreground ,magenta))))
   `(counsel-notmuch-subject-face ((,class (:foreground ,magenta-alt-other))))
   ;;;; counsel-org-capture-string
   `(counsel-org-capture-string-template-body-face ((,class (:foreground ,fg-special-cold))))
   ;;;; cov
   `(cov-coverage-not-run-face ((,class (:foreground ,red-intense))))
   `(cov-coverage-run-face ((,class (:foreground ,green-intense))))
   `(cov-heavy-face ((,class (:foreground ,magenta-intense))))
   `(cov-light-face ((,class (:foreground ,blue-intense))))
   `(cov-med-face ((,class (:foreground ,yellow-intense))))
   `(cov-none-face ((,class (:foreground ,cyan-intense))))
   ;;;; custom
   `(custom-changed ((,class (:inherit modus-theme-subtle-cyan))))
   `(custom-comment ((,class (:foreground ,fg-alt))))
   `(custom-comment-tag ((,class (:background ,bg-alt :foreground ,yellow-alt-other))))
   `(custom-face-tag ((,class (:foreground ,blue-intense :weight bold))))
   `(custom-group-tag ((,class (:foreground ,green-intense :weight bold))))
   `(custom-group-tag-1 ((,class (:inherit modus-theme-special-warm))))
   `(custom-invalid ((,class (:inherit modus-theme-intense-red :weight bold))))
   `(custom-modified ((,class (:inherit modus-theme-subtle-cyan))))
   `(custom-rogue ((,class (:inherit modus-theme-refine-magenta))))
   `(custom-set ((,class (:foreground ,blue-alt))))
   `(custom-state ((,class (:foreground ,cyan-alt-other))))
   `(custom-themed ((,class (:inherit modus-theme-subtle-blue))))
   `(custom-variable-tag ((,class (:foreground ,cyan :weight bold))))
   ;;;; dashboard (emacs-dashboard)
   `(dashboard-banner-logo-title ((,class (:foreground ,fg-special-cold :weight bold))))
   `(dashboard-footer ((,class (:foreground ,fg-special-mild :weight bold))))
   `(dashboard-heading ((,class (:foreground ,fg-special-warm :weight bold))))
   `(dashboard-navigator ((,class (:foreground ,cyan-alt-other))))
   `(dashboard-text-banner ((,class (:foreground ,fg-dim))))
   ;;;; deadgrep
   `(deadgrep-filename-face ((,class (:foreground ,cyan :weight bold))))
   `(deadgrep-match-face ((,class (:inherit modus-theme-subtle-blue))))
   `(deadgrep-meta-face ((,class (:foreground ,fg-alt))))
   `(deadgrep-regexp-metachar-face ((,class (:foreground ,yellow-intense :weight bold))))
   `(deadgrep-search-term-face ((,class (:foreground ,green-intense :weight bold))))
   ;;;; define-word
   `(define-word-face-1 ((,class (:foreground ,yellow))))
   `(define-word-face-2 ((,class (:foreground ,fg-main))))
   ;;;; disk-usage
   `(disk-usage-children ((,class (:foreground ,yellow))))
   `(disk-usage-inaccessible ((,class (:foreground ,red :weight bold))))
   `(disk-usage-percent ((,class (:foreground ,green))))
   `(disk-usage-size ((,class (:foreground ,cyan))))
   `(disk-usage-symlink ((,class (:foreground ,blue :underline t))))
   `(disk-usage-symlink-directory ((,class (:foreground ,blue-alt :weight bold))))
   ;;;; diff-hl
   `(diff-hl-change ((,class (:inherit modus-theme-intense-yellow))))
   `(diff-hl-delete ((,class (:inherit modus-theme-intense-red))))
   `(diff-hl-dired-change ((,class (:inherit diff-hl-change))))
   `(diff-hl-dired-delete ((,class (:inherit diff-hl-delete))))
   `(diff-hl-dired-ignored ((,class (:inherit dired-ignored))))
   `(diff-hl-dired-insert ((,class (:inherit diff-hl-insert))))
   `(diff-hl-dired-unknown ((,class (:inherit dired-ignored))))
   `(diff-hl-insert ((,class (:inherit modus-theme-intense-green))))
   `(diff-hl-reverted-hunk-highlight ((,class (:inherit modus-theme-intense-magenta))))
   ;;;; diff-mode
   `(diff-added ((,class (:inherit modus-theme-subtle-green))))
   `(diff-changed ((,class (:inherit modus-theme-subtle-yellow))))
   `(diff-context ((,class (:background ,bg-dim :foreground ,fg-alt))))
   `(diff-file-header ((,class (:inherit modus-theme-special-cold :weight bold))))
   `(diff-function ((,class (:inherit modus-theme-special-cold :weight bold))))
   `(diff-header ((,class (:inherit modus-theme-special-cold :weight bold))))
   `(diff-hunk-header ((,class (:inherit modus-theme-special-warm :weight bold))))
   `(diff-index-header ((,class (:inherit diff-function))))
   `(diff-indicator-added ((,class (:inherit diff-added))))
   `(diff-indicator-changed ((,class (:inherit diff-changed))))
   `(diff-indicator-removed ((,class (:inherit diff-removed))))
   `(diff-nonexistent ((,class (:inherit modus-theme-subtle-neutral))))
   `(diff-refine-added ((,class (:inherit modus-theme-intense-green))))
   `(diff-refine-changed ((,class (:inherit modus-theme-intense-yellow))))
   `(diff-refine-removed ((,class (:inherit modus-theme-intense-red))))
   `(diff-removed ((,class (:inherit modus-theme-subtle-red))))
   ;;;; dired
   `(dired-directory ((,class (:foreground ,blue :weight bold))))
   `(dired-flagged ((,class (:inherit modus-theme-intense-red))))
   `(dired-header ((,class (:foreground ,fg-main :weight bold))))
   `(dired-ignored ((,class (:foreground ,fg-alt))))
   `(dired-mark ((,class (:foreground ,magenta-alt :weight bold))))
   `(dired-marked ((,class (:inherit modus-theme-intense-magenta))))
   `(dired-perm-write ((,class (:foreground ,fg-special-warm))))
   `(dired-symlink ((,class (:foreground ,blue-alt :underline t))))
   `(dired-warning ((,class (:foreground ,yellow :weight bold))))
   ;;;; dired-async
   `(dired-async-failures ((,class (:foreground ,red-active :weight bold))))
   `(dired-async-message ((,class (:foreground ,green-active :weight bold))))
   `(dired-async-mode-message ((,class (:foreground ,cyan-active :weight bold))))
   ;;;; dired-git-info
   `(dgi-commit-message-face ((,class (:foreground ,fg-special-mild))))
   ;;;; dired-narrow
   `(dired-narrow-blink ((,class (:inherit modus-theme-subtle-cyan :weight bold))))
   ;;;; dired-subtree
   ;; remove background from dired-subtree, else it breaks
   ;; dired-{flagged,marked} and any other face that sets a background
   ;; such as hl-line
   `(dired-subtree-depth-1-face ((,class (:background nil))))
   `(dired-subtree-depth-2-face ((,class (:background nil))))
   `(dired-subtree-depth-3-face ((,class (:background nil))))
   `(dired-subtree-depth-4-face ((,class (:background nil))))
   `(dired-subtree-depth-5-face ((,class (:background nil))))
   `(dired-subtree-depth-6-face ((,class (:background nil))))
   ;;;; diredfl
   `(diredfl-autofile-name ((,class (:inherit modus-theme-refine-yellow))))
   `(diredfl-compressed-file-name ((,class (:foreground ,red-alt :weight bold))))
   `(diredfl-compressed-file-suffix ((,class (:inherit diredfl-compressed-file-name))))
   `(diredfl-date-time ((,class (:foreground ,fg-special-cold))))
   `(diredfl-deletion ((,class (:inherit dired-flagged))))
   `(diredfl-deletion-file-name ((,class (:inherit dired-flagged))))
   `(diredfl-dir-heading ((,class (:inherit dired-header))))
   `(diredfl-dir-name ((,class (:inherit dired-directory))))
   `(diredfl-dir-priv ((,class (:foreground ,blue-alt))))
   `(diredfl-exec-priv ((,class (:foreground ,cyan))))
   `(diredfl-executable-tag ((,class (:foreground ,cyan-alt))))
   `(diredfl-file-name ((,class (:foreground ,fg-main))))
   `(diredfl-file-suffix ((,class (:foreground ,fg-special-warm))))
   `(diredfl-flag-mark ((,class (:inherit dired-marked))))
   `(diredfl-flag-mark-line ((,class (:inherit dired-marked))))
   `(diredfl-ignored-file-name ((,class (:foreground ,fg-inactive))))
   `(diredfl-link-priv ((,class (:foreground ,blue-alt))))
   `(diredfl-no-priv ((,class (:foreground ,fg-inactive))))
   `(diredfl-number ((,class (:foreground ,fg-alt))))
   `(diredfl-other-priv ((,class (:foreground ,yellow))))
   `(diredfl-rare-priv ((,class (:foreground ,green))))
   `(diredfl-read-priv ((,class (:foreground ,magenta))))
   `(diredfl-symlink ((,class (:foreground ,blue-alt :underline t))))
   `(diredfl-tagged-autofile-name ((,class (:inherit modus-theme-refine-magenta))))
   `(diredfl-write-priv ((,class (:foreground ,magenta-alt-other))))
   ;;;; doom-modeline
   `(doom-modeline-bar ((,class (:inherit modus-theme-active-blue))))
   `(doom-modeline-bar-inactive ((,class (:background ,fg-inactive :foreground ,bg-main))))
   `(doom-modeline-battery-charging ((,class (:foreground ,green-active))))
   `(doom-modeline-battery-critical ((,class (:foreground ,red-active :weight bold))))
   `(doom-modeline-battery-error ((,class (:inherit modus-theme-active-red))))
   `(doom-modeline-battery-full ((,class (:foreground ,blue-active))))
   `(doom-modeline-battery-normal ((,class (:foreground ,fg-active))))
   `(doom-modeline-battery-warning ((,class (:foreground ,yellow-active :weight bold))))
   `(doom-modeline-buffer-file ((,class (:foreground ,fg-active :weight bold))))
   `(doom-modeline-buffer-major-mode ((,class (:foreground ,cyan-active :weight bold))))
   `(doom-modeline-buffer-minor-mode ((,class (:foreground ,fg-inactive))))
   `(doom-modeline-buffer-modified ((,class (:foreground ,magenta-active :weight bold))))
   `(doom-modeline-buffer-path ((,class (:foreground ,fg-active :weight bold))))
   `(doom-modeline-debug ((,class (:foreground ,yellow-active :weight bold))))
   `(doom-modeline-evil-emacs-state ((,class (:foreground ,magenta-active :weight bold))))
   `(doom-modeline-evil-insert-state ((,class (:foreground ,green-active :weight bold))))
   `(doom-modeline-evil-motion-state ((,class (:foreground ,fg-inactive :weight bold))))
   `(doom-modeline-evil-normal-state ((,class (:foreground ,fg-active :weight bold))))
   `(doom-modeline-evil-operator-state ((,class (:foreground ,blue-active :weight bold))))
   `(doom-modeline-evil-replace-state ((,class (:foreground ,red-active :weight bold))))
   `(doom-modeline-evil-visual-state ((,class (:foreground ,cyan-active :weight bold))))
   `(doom-modeline-highlight ((,class (:foreground ,blue-active :weight bold))))
   `(doom-modeline-host ((,class (:slant italic))))
   `(doom-modeline-info ((,class (:foreground ,green-active))))
   `(doom-modeline-lsp-error ((,class (:foreground ,red-active :weight bold))))
   `(doom-modeline-lsp-success ((,class (:foreground ,green-active :weight bold))))
   `(doom-modeline-lsp-warning ((,class (:foreground ,yellow-active :weight bold))))
   `(doom-modeline-panel ((,class (:inherit modus-theme-active-blue))))
   `(doom-modeline-persp-buffer-not-in-persp ((,class (:foreground ,yellow-active :slant italic))))
   `(doom-modeline-persp-name ((,class (:foreground ,fg-active))))
   `(doom-modeline-project-dir ((,class (:foreground ,blue-active :weight bold))))
   `(doom-modeline-project-parent-dir ((,class (:foreground ,blue-active))))
   `(doom-modeline-project-root-dir ((,class (:foreground ,fg-active))))
   `(doom-modeline-unread-number ((,class (:foreground ,fg-active :slant italic))))
   `(doom-modeline-urgent ((,class (:foreground ,red-active :weight bold :underline t))))
   `(doom-modeline-warning ((,class (:foreground ,yellow-active :weight bold))))
   ;;;; easy-jekyll
   `(easy-jekyll-help-face ((,class (:background ,bg-dim :foreground ,cyan-alt-other))))
   ;;;; easy-kill
   `(easy-kill-origin ((,class (:inherit modus-theme-subtle-red))))
   `(easy-kill-selection ((,class (:inherit modus-theme-subtle-yellow))))
   ;;;; ediff
   `(ediff-current-diff-A ((,class (:inherit modus-theme-special-warm))))
   `(ediff-current-diff-Ancestor ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(ediff-current-diff-B ((,class (:inherit modus-theme-special-cold))))
   `(ediff-current-diff-C ((,class (:inherit modus-theme-special-mild))))
   `(ediff-even-diff-A ((,class (:inherit modus-theme-subtle-yellow))))
   `(ediff-even-diff-Ancestor ((,class (:background ,bg-inactive :foreground ,fg-main))))
   `(ediff-even-diff-B ((,class (:inherit modus-theme-subtle-cyan))))
   `(ediff-even-diff-C ((,class (:inherit modus-theme-subtle-green))))
   `(ediff-fine-diff-A ((,class (:inherit modus-theme-intense-yellow))))
   `(ediff-fine-diff-Ancestor ((,class (:inherit modus-theme-intense-magenta))))
   `(ediff-fine-diff-B ((,class (:inherit modus-theme-intense-cyan))))
   `(ediff-fine-diff-C ((,class (:inherit modus-theme-intense-green))))
   `(ediff-odd-diff-A ((,class (:inherit modus-theme-refine-yellow))))
   `(ediff-odd-diff-Ancestor ((,class (:background ,bg-active :foreground ,fg-main))))
   `(ediff-odd-diff-B ((,class (:inherit modus-theme-refine-cyan))))
   `(ediff-odd-diff-C ((,class (:inherit modus-theme-refine-green))))
   ;;;; eldoc-box
   `(eldoc-box-body ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(eldoc-box-border ((,class (:background ,fg-alt))))
   ;;;; elfeed
   `(elfeed-log-date-face ((,class (:foreground ,blue))))
   `(elfeed-log-debug-level-face ((,class (:inherit modus-theme-intense-magenta))))
   `(elfeed-log-error-level-face ((,class (:inherit modus-theme-intense-red))))
   `(elfeed-log-info-level-face ((,class (:inherit modus-theme-subtle-cyan))))
   `(elfeed-log-warn-level-face ((,class (:inherit modus-theme-subtle-yellow))))
   `(elfeed-search-date-face ((,class (:foreground ,fg-special-cold))))
   `(elfeed-search-feed-face ((,class (:foreground ,cyan))))
   `(elfeed-search-filter-face ((,class (:foreground ,fg-special-cold))))
   `(elfeed-search-last-update-face ((,class (:foreground ,fg-special-calm))))
   `(elfeed-search-tag-face ((,class (:foreground ,fg-special-mild))))
   `(elfeed-search-title-face ((,class (:foreground ,fg-main))))
   `(elfeed-search-unread-count-face ((,class (:foreground ,fg-special-warm))))
   `(elfeed-search-unread-title-face ((,class (:weight bold))))
   ;;;; emms
   `(emms-playlist-track-face ((,class (:foreground ,blue))))
   `(emms-playlist-selected-face ((,class (:foreground ,magenta :weight bold))))
   ;;;; epa
   `(epa-field-body ((,class (:foreground ,fg-main))))
   `(epa-field-name ((,class (:foreground ,fg-dim :weight bold))))
   `(epa-mark ((,class (:foreground ,magenta :weight bold))))
   `(epa-string ((,class (:foreground ,blue-alt))))
   `(epa-validity-disabled ((,class (:inherit modus-theme-refine-red))))
   `(epa-validity-high ((,class (:foreground ,cyan :weight bold))))
   `(epa-validity-low ((,class (:foreground ,fg-alt))))
   `(epa-validity-medium ((,class (:foreground ,yellow))))
   ;;;; equake
   `(equake-buffer-face ((,class (:background ,bg-main :foreground ,fg-main))))
   `(equake-shell-type-eshell ((,class (:background ,bg-inactive :foreground ,green-active))))
   `(equake-shell-type-rash ((,class (:background ,bg-inactive :foreground ,red-active))))
   `(equake-shell-type-shell ((,class (:background ,bg-inactive :foreground ,cyan-active))))
   `(equake-shell-type-term ((,class (:background ,bg-inactive :foreground ,yellow-active))))
   `(equake-shell-type-vterm ((,class (:background ,bg-inactive :foreground ,magenta-active))))
   `(equake-tab-active ((,class (:background ,fg-alt :foreground ,bg-alt))))
   `(equake-tab-inactive ((,class (:foreground ,fg-inactive))))
   ;;;; erc
   `(erc-action-face ((,class (:foreground ,cyan :weight bold))))
   `(erc-bold-face ((,class (:weight bold))))
   `(erc-button ((,class (:inherit button))))
   `(erc-command-indicator-face ((,class (:foreground ,cyan-alt :weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,blue))))
   `(erc-dangerous-host-face ((,class (:inherit modus-theme-intense-red))))
   `(erc-direct-msg-face ((,class (:foreground ,fg-special-mild))))
   `(erc-error-face ((,class (:foreground ,red :weight bold))))
   `(erc-fool-face ((,class (:foreground ,fg-inactive))))
   `(erc-header-line ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(erc-input-face ((,class (:foreground ,fg-special-calm))))
   `(erc-inverse-face ((,class (:inherit erc-default-face :inverse-video t))))
   `(erc-keyword-face ((,class (:foreground ,magenta-alt :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,magenta :weight bold))))
   `(erc-my-nick-prefix-face ((,class (:inherit erc-my-nick-face))))
   `(erc-nick-default-face ((,class (:foreground ,fg-special-cold :weight bold))))
   `(erc-nick-msg-face ((,class (:foreground ,green :weight bold))))
   `(erc-nick-prefix-face ((,class (:inherit erc-nick-default-face :weight bold))))
   `(erc-notice-face ((,class (:foreground ,fg-special-warm))))
   `(erc-pal-face ((,class (:foreground ,magenta-alt-other :weight bold))))
   `(erc-prompt-face ((,class (:foreground ,cyan-alt-other :weight bold))))
   `(erc-timestamp-face ((,class (:foreground ,blue-alt))))
   `(erc-underline-face ((,class (:underline t))))
   ;;;; ert
   `(ert-test-result-expected ((,class (:inherit modus-theme-intense-green))))
   `(ert-test-result-unexpected ((,class (:inherit modus-theme-intense-red))))
   ;;;; eshell
   `(eshell-ls-archive ((,class (:foreground ,cyan-alt :weight bold))))
   `(eshell-ls-backup ((,class (:foreground ,yellow-alt))))
   `(eshell-ls-clutter ((,class (:foreground ,red-alt))))
   `(eshell-ls-directory ((,class (:foreground ,blue-alt :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,magenta-alt))))
   `(eshell-ls-missing ((,class (:inherit modus-theme-intense-red))))
   `(eshell-ls-product ((,class (:foreground ,fg-special-warm))))
   `(eshell-ls-readonly ((,class (:foreground ,fg-special-cold))))
   `(eshell-ls-special ((,class (:foreground ,magenta :weight bold))))
   `(eshell-ls-symlink ((,class (:foreground ,cyan :underline t))))
   `(eshell-ls-unreadable ((,class (:background ,bg-inactive :foreground ,fg-inactive))))
   `(eshell-prompt ((,class (:foreground ,cyan-alt-other :weight bold))))
   ;;;; evil-mode
   `(evil-ex-commands ((,class (:foreground ,magenta-alt-other))))
   `(evil-ex-info ((,class (:foreground ,cyan-alt-other))))
   `(evil-ex-lazy-highlight ((,class (:inherit modus-theme-refine-cyan))))
   `(evil-ex-search ((,class (:inherit modus-theme-intense-green))))
   `(evil-ex-substitute-matches ((,class (:inherit modus-theme-refine-yellow :underline t))))
   `(evil-ex-substitute-replacement ((,class (:inherit modus-theme-intense-green :weight bold))))
   ;;;; evil-goggles
   `(evil-goggles-change-face ((,class (:inherit modus-theme-refine-yellow))))
   `(evil-goggles-commentary-face ((,class (:inherit modus-theme-subtle-neutral :slant ,modus-theme-slant))))
   `(evil-goggles-default-face ((,class (:inherit modus-theme-subtle-neutral))))
   `(evil-goggles-delete-face ((,class (:inherit modus-theme-refine-red))))
   `(evil-goggles-fill-and-move-face ((,class (:inherit evil-goggles-default-face))))
   `(evil-goggles-indent-face ((,class (:inherit evil-goggles-default-face))))
   `(evil-goggles-join-face ((,class (:inherit modus-theme-subtle-green))))
   `(evil-goggles-nerd-commenter-face ((,class (:inherit evil-goggles-commentary-face))))
   `(evil-goggles-paste-face ((,class (:inherit modus-theme-subtle-cyan))))
   `(evil-goggles-record-macro-face ((,class (:inherit modus-theme-special-cold))))
   `(evil-goggles-replace-with-register-face ((,class (:inherit modus-theme-refine-magenta))))
   `(evil-goggles-set-marker-face ((,class (:inherit modus-theme-intense-magenta))))
   `(evil-goggles-shift-face ((,class (:inherit evil-goggles-default-face))))
   `(evil-goggles-surround-face ((,class (:inherit evil-goggles-default-face))))
   `(evil-goggles-yank-face ((,class (:inherit modus-theme-subtle-blue))))
   ;;;; evil-visual-mark-mode
   `(evil-visual-mark-face ((,class (:inherit modus-theme-intense-magenta))))
   ;;;; eww
   `(eww-invalid-certificate ((,class (:foreground ,red-active))))
   `(eww-valid-certificate ((,class (:foreground ,green-active))))
   `(eww-form-checkbox ((,class (:box (:line-width 1 :color ,fg-inactive :style released-button) :background ,bg-inactive :foreground ,fg-main))))
   `(eww-form-file ((,class (:box (:line-width 1 :color ,fg-inactive :style released-button) :background ,bg-active :foreground ,fg-main))))
   `(eww-form-select ((,class (:inherit eww-form-checkbox))))
   `(eww-form-submit ((,class (:inherit eww-form-file))))
   `(eww-form-text ((,class (:box (:line-width 1 :color ,fg-inactive :style none) :background ,bg-active :foreground ,fg-active))))
   `(eww-form-textarea ((,class (:background ,bg-alt :foreground ,fg-main))))
   ;;;; eyebrowse
   `(eyebrowse-mode-line-active ((,class (:foreground ,blue-active :weight bold))))
   ;;;; fancy-dabbrev
   `(fancy-dabbrev-menu-face ((,class (:background ,bg-alt :foreground ,fg-alt))))
   `(fancy-dabbrev-preview-face ((,class (:foreground ,fg-alt :underline t))))
   `(fancy-dabbrev-selection-face ((,class (:inherit modus-theme-intense-cyan :weight bold))))
   ;;;; focus
   `(focus-unfocused ((,class (:foreground ,fg-alt))))
   ;;;; font-lock
   `(font-lock-builtin-face ((,class (:foreground ,magenta-alt :weight ,modus-theme-bold))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-alt :slant ,modus-theme-slant))))
   `(font-lock-comment-face ((,class (:foreground ,fg-alt :slant ,modus-theme-slant))))
   `(font-lock-constant-face ((,class (:foreground ,blue-alt-other))))
   `(font-lock-doc-face ((,class (:foreground ,fg-special-cold :slant ,modus-theme-slant))))
   `(font-lock-function-name-face ((,class (:foreground ,magenta))))
   `(font-lock-keyword-face ((,class (:foreground ,magenta-alt-other :weight ,modus-theme-bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,yellow :weight ,modus-theme-bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,magenta))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,green :weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,magenta :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,blue-alt))))
   `(font-lock-type-face ((,class (:foreground ,magenta-alt))))
   `(font-lock-variable-name-face ((,class (:foreground ,cyan))))
   `(font-lock-warning-face ((,class (:background ,bg-alt :foreground ,yellow-alt-other :weight bold))))
   ;;;; flycheck
   `(flycheck-error ((,class (:foreground ,red :underline t))))
   `(flycheck-error-list-checker-name ((,class (:foreground ,magenta-active))))
   `(flycheck-error-list-column-number ((,class (:foreground ,fg-special-cold))))
   `(flycheck-error-list-error ((,class (:inherit error))))
   `(flycheck-error-list-filename ((,class (:foreground ,blue))))
   `(flycheck-error-list-highlight ((,class (:inherit modus-theme-special-warm))))
   `(flycheck-error-list-id ((,class (:foreground ,magenta-alt-other))))
   `(flycheck-error-list-id-with-explainer ((,class (:inherit flycheck-error-list-id :box t))))
   `(flycheck-error-list-info ((,class (:foreground ,green))))
   `(flycheck-error-list-line-number ((,class (:foreground ,fg-special-warm))))
   `(flycheck-error-list-warning ((,class (:foreground ,yellow))))
   `(flycheck-fringe-error ((,class (:foreground ,red-intense :weight bold))))
   `(flycheck-fringe-info ((,class (:foreground ,green-intense :weight bold))))
   `(flycheck-fringe-warning ((,class (:foreground ,yellow-intense :weight bold))))
   `(flycheck-info ((,class (:foreground ,green :underline t))))
   `(flycheck-verify-select-checker ((,class (:box (:line-width 1 :color nil :style released-button)))))
   `(flycheck-warning ((,class (:foreground ,yellow :underline t))))
   ;;;; flycheck-indicator
   `(flycheck-indicator-disabled ((,class (:foreground ,fg-inactive :slant ,modus-theme-slant))))
   `(flycheck-indicator-error ((,class (:foreground ,red-active :weight bold))))
   `(flycheck-indicator-info ((,class (:foreground ,blue-active :weight bold))))
   `(flycheck-indicator-running ((,class (:foreground ,magenta-active :weight bold))))
   `(flycheck-indicator-success ((,class (:foreground ,green-active :weight bold))))
   `(flycheck-indicator-warning ((,class (:foreground ,yellow-active :weight bold))))
   ;;;; flycheck-posframe
   `(flycheck-posframe-background-face ((,class (:background ,bg-alt))))
   `(flycheck-posframe-border-face ((,class (:foreground ,fg-alt))))
   `(flycheck-posframe-error-face ((,class (:foreground ,red :weight bold))))
   `(flycheck-posframe-face ((,class (:foreground ,fg-main :slant ,modus-theme-slant))))
   `(flycheck-posframe-info-face ((,class (:foreground ,green :weight bold))))
   `(flycheck-posframe-warning-face ((,class (:foreground ,yellow :weight bold))))
   ;;;; flymake
   `(flymake-error ((,class (:foreground ,red :underline t))))
   `(flymake-note ((,class (:foreground ,green :underline t))))
   `(flymake-warning ((,class (:foreground ,yellow :underline t))))
   ;;;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) class)
       (:foreground ,yellow :underline (:style wave)))
      (,class (:foreground ,yellow :underline t))))
   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) class)
       (:foreground ,red :underline (:style wave)))
      (,class (:foreground ,red :underline t))))
   ;;;; fountain-mode
   `(fountain-character ((,class (:foreground ,magenta-alt-other))))
   `(fountain-comment ((,class (:foreground ,fg-alt :slant ,modus-theme-slant))))
   `(fountain-dialog ((,class (:foreground ,blue))))
   `(fountain-metadata-key ((,class (:foreground ,blue-alt-other))))
   `(fountain-metadata-value ((,class (:foreground ,cyan-alt-other))))
   `(fountain-non-printing ((,class (:inherit fountain-comment))))
   `(fountain-note ((,class (:foreground ,fg-special-warm :slant ,modus-theme-slant))))
   `(fountain-page-break ((,class (:foreground ,yellow :weight bold))))
   `(fountain-page-number ((,class (:foreground ,yellow-alt :weight bold))))
   `(fountain-paren ((,class (:foreground ,cyan))))
   `(fountain-scene-heading ((,class (:foreground ,fg-special-calm :weight bold))))
   `(fountain-section-heading ((,class (:foreground ,green-alt-other :weight bold))))
   `(fountain-synopsis ((,class (:foreground ,green))))
   `(fountain-template ((,class (:foreground ,magenta-alt))))
   `(fountain-trans ((,class (:foreground ,magenta :weight bold))))
   ;;;; geiser
   `(geiser-font-lock-autodoc-current-arg ((,class (:foreground ,magenta))))
   `(geiser-font-lock-autodoc-identifier ((,class (:foreground ,blue))))
   `(geiser-font-lock-doc-button ((,class (:foreground ,cyan-alt :underline t))))
   `(geiser-font-lock-doc-link ((,class (:inherit link))))
   `(geiser-font-lock-error-link ((,class (:foreground ,red-alt :underline t))))
   `(geiser-font-lock-image-button ((,class (:foreground ,green-alt :underline t))))
   `(geiser-font-lock-repl-input ((,class (:weight bold))))
   `(geiser-font-lock-repl-output ((,class (:foreground ,magenta-alt-other))))
   `(geiser-font-lock-repl-prompt ((,class (:foreground ,cyan-alt-other))))
   `(geiser-font-lock-xref-header ((,class (:weight bold))))
   `(geiser-font-lock-xref-link ((,class (:inherit link))))
   ;;;; git-commit
   `(git-commit-comment-action ((,class (:foreground ,fg-special-calm))))
   `(git-commit-comment-branch-local ((,class (:foreground ,cyan))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,blue))))
   `(git-commit-comment-detached ((,class (:foreground ,yellow))))
   `(git-commit-comment-file ((,class (:foreground ,blue))))
   `(git-commit-comment-heading ((,class (:foreground ,fg-main :weight bold))))
   `(git-commit-keyword ((,class (:foreground ,magenta))))
   `(git-commit-known-pseudo-header ((,class (:foreground ,fg-special-warm :weight bold))))
   `(git-commit-nonempty-second-line ((,class (:inherit modus-theme-refine-yellow :weight bold))))
   `(git-commit-overlong-summary ((,class (:inherit modus-theme-subtle-yellow))))
   `(git-commit-pseudo-header ((,class (:foreground ,fg-alt :weight bold))))
   `(git-commit-summary ((,class (:foreground ,magenta-alt-other))))
   ;;;; git-gutter
   `(git-gutter:added ((,class (:foreground ,green-intense :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,red-intense :weight bold))))
   `(git-gutter:modified ((,class (:foreground ,yellow-intense :weight bold))))
   `(git-gutter:separator ((,class (:foreground ,cyan-intense :weight bold))))
   `(git-gutter:unchanged ((,class (:inherit modus-theme-refine-magenta))))
   ;;;; git-gutter-fr
   `(git-gutter-fr:added ((,class (:foreground ,green-intense :weight bold))))
   `(git-gutter-fr:deleted ((,class (:foreground ,red-intense :weight bold))))
   `(git-gutter-fr:modified ((,class (:foreground ,yellow-intense :weight bold))))
   ;;;; git-{gutter,fringe}+
   `(git-gutter+-added ((,class (:foreground ,green-intense :weight bold))))
   `(git-gutter+-deleted ((,class (:foreground ,red-intense :weight bold))))
   `(git-gutter+-modified ((,class (:foreground ,yellow-intense :weight bold))))
   `(git-gutter+-separator ((,class (:foreground ,cyan-intense :weight bold))))
   `(git-gutter+-unchanged ((,class (:inherit modus-theme-refine-magenta))))
   `(git-gutter-fr+-added ((,class (:foreground ,green-intense :weight bold))))
   `(git-gutter-fr+-deleted ((,class (:foreground ,red-intense :weight bold))))
   `(git-gutter-fr+-modified ((,class (:foreground ,yellow-intense :weight bold))))
   ;;;; git-lens
   `(git-lens-added ((,class (:foreground ,green :weight bold))))
   `(git-lens-deleted ((,class (:foreground ,red :weight bold))))
   `(git-lens-header ((,class (:height 1.1 :foreground ,cyan :weight bold))))
   `(git-lens-modified ((,class (:foreground ,yellow :weight bold))))
   `(git-lens-renamed ((,class (:foreground ,magenta :weight bold))))
   ;;;; git-timemachine
   `(git-timemachine-commit ((,class (:foreground ,yellow-active :weight bold))))
   `(git-timemachine-minibuffer-author-face ((,class (:foreground ,fg-special-warm))))
   `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,red-alt))))
   ;;;; gnus
   `(gnus-button ((,class (:inherit button))))
   `(gnus-cite-1 ((,class (:foreground ,blue-alt))))
   `(gnus-cite-10 ((,class (:foreground ,magenta-alt-other))))
   `(gnus-cite-11 ((,class (:foreground ,yellow-alt-other))))
   `(gnus-cite-2 ((,class (:foreground ,red-alt))))
   `(gnus-cite-3 ((,class (:foreground ,green-alt))))
   `(gnus-cite-4 ((,class (:foreground ,magenta-alt))))
   `(gnus-cite-5 ((,class (:foreground ,yellow-alt))))
   `(gnus-cite-6 ((,class (:foreground ,cyan-alt))))
   `(gnus-cite-7 ((,class (:foreground ,blue-alt-other))))
   `(gnus-cite-8 ((,class (:foreground ,red-alt-other))))
   `(gnus-cite-9 ((,class (:foreground ,green-alt-other))))
   `(gnus-cite-attribution ((,class (:foreground ,fg-main :slant italic))))
   `(gnus-emphasis-highlight-words ((,class (:inherit modus-theme-intense-yellow))))
   `(gnus-group-mail-1 ((,class (:foreground ,magenta :weight bold))))
   `(gnus-group-mail-1-empty ((,class (:foreground ,magenta))))
   `(gnus-group-mail-2 ((,class (:foreground ,magenta-alt :weight bold))))
   `(gnus-group-mail-2-empty ((,class (:foreground ,magenta-alt))))
   `(gnus-group-mail-3 ((,class (:foreground ,magenta-alt-other :weight bold))))
   `(gnus-group-mail-3-empty ((,class (:foreground ,magenta-alt-other))))
   `(gnus-group-mail-low ((,class (:foreground ,fg-alt :weight bold))))
   `(gnus-group-mail-low-empty ((,class (:foreground ,fg-alt))))
   `(gnus-group-news-1 ((,class (:foreground ,cyan :weight bold))))
   `(gnus-group-news-1-empty ((,class (:foreground ,cyan))))
   `(gnus-group-news-2 ((,class (:foreground ,cyan-alt :weight bold))))
   `(gnus-group-news-2-empty ((,class (:foreground ,cyan-alt))))
   `(gnus-group-news-3 ((,class (:foreground ,cyan-alt-other :weight bold))))
   `(gnus-group-news-3-empty ((,class (:foreground ,cyan-alt-other))))
   `(gnus-group-news-4 ((,class (:foreground ,green :weight bold))))
   `(gnus-group-news-4-empty ((,class (:foreground ,green))))
   `(gnus-group-news-5 ((,class (:foreground ,green-alt :weight bold))))
   `(gnus-group-news-5-empty ((,class (:foreground ,green-alt))))
   `(gnus-group-news-6 ((,class (:foreground ,green-alt-other :weight bold))))
   `(gnus-group-news-6-empty ((,class (:foreground ,green-alt-other))))
   `(gnus-group-news-low ((,class (:foreground ,fg-alt :weight bold))))
   `(gnus-group-news-low-empty ((,class (:foreground ,fg-alt))))
   `(gnus-header-content ((,class (:foreground ,fg-main))))
   `(gnus-header-from ((,class (:foreground ,fg-main))))
   `(gnus-header-name ((,class (:foreground ,fg-special-cold :weight bold))))
   `(gnus-header-newsgroup ((,class (:foreground ,blue))))
   `(gnus-header-subject ((,class (:foreground ,magenta-alt-other))))
   `(gnus-server-agent ((,class (:foreground ,cyan :weight bold))))
   `(gnus-server-closed ((,class (:foreground ,magenta))))
   `(gnus-server-cloud ((,class (:foreground ,cyan-alt :weight bold))))
   `(gnus-server-cloud-host ((,class (:background ,cyan-alt :foreground ,bg-main))))
   `(gnus-server-denied ((,class (:foreground ,red :weight bold))))
   `(gnus-server-offline ((,class (:foreground ,yellow :weight bold))))
   `(gnus-server-opened ((,class (:foreground ,green :weight bold))))
   `(gnus-signature ((,class (:foreground ,yellow-alt-other :slant italic))))
   `(gnus-splash ((,class (:foreground ,fg-alt))))
   `(gnus-summary-cancelled ((,class (:inherit modus-theme-intense-yellow))))
   `(gnus-summary-high-ancient ((,class (:foreground ,fg-alt :weight bold))))
   `(gnus-summary-high-read ((,class (:foreground ,fg-dim :weight bold))))
   `(gnus-summary-high-ticked ((,class (:foreground ,red-alt :weight bold))))
   `(gnus-summary-high-undownloaded ((,class (:foreground ,yellow :weight bold))))
   `(gnus-summary-high-unread ((,class (:foreground ,fg-main :weight bold))))
   `(gnus-summary-low-ancient ((,class (:foreground ,fg-alt :slant italic))))
   `(gnus-summary-low-read ((,class (:foreground ,fg-dim :slant italic))))
   `(gnus-summary-low-ticked ((,class (:foreground ,red-alt :slant italic))))
   `(gnus-summary-low-undownloaded ((,class (:foreground ,yellow :slant italic))))
   `(gnus-summary-low-unread ((,class (:foreground ,fg-main :slant italic))))
   `(gnus-summary-normal-ancient ((,class (:foreground ,fg-alt))))
   `(gnus-summary-normal-read ((,class (:foreground ,fg-dim))))
   `(gnus-summary-normal-ticked ((,class (:foreground ,red-alt))))
   `(gnus-summary-normal-undownloaded ((,class (:foreground ,yellow))))
   `(gnus-summary-normal-unread ((,class (:foreground ,fg-main))))
   `(gnus-summary-selected ((,class (:inherit modus-theme-subtle-cyan))))
   ;;;; helm
   `(helm-M-x-key ((,class (:foreground ,magenta-alt-other :weight bold))))
   `(helm-action ((,class (:underline t))))
   `(helm-bookmark-addressbook ((,class (:foreground ,green-alt))))
   `(helm-bookmark-directory ((,class (:foreground ,blue :weight bold))))
   `(helm-bookmark-file ((,class (:foreground ,fg-main))))
   `(helm-bookmark-file-not-found ((,class (:background ,bg-alt :foreground ,fg-alt))))
   `(helm-bookmark-gnus ((,class (:foreground ,magenta))))
   `(helm-bookmark-info ((,class (:foreground ,cyan-alt))))
   `(helm-bookmark-man ((,class (:foreground ,yellow-alt))))
   `(helm-bookmark-w3m ((,class (:foreground ,blue-alt))))
   `(helm-buffer-archive ((,class (:foreground ,cyan :weight bold))))
   `(helm-buffer-directory ((,class (:foreground ,blue :weight bold))))
   `(helm-buffer-file ((,class (:foreground ,fg-main))))
   `(helm-buffer-modified ((,class (:foreground ,yellow-alt))))
   `(helm-buffer-not-saved ((,class (:foreground ,red-alt))))
   `(helm-buffer-process ((,class (:foreground ,magenta))))
   `(helm-buffer-saved-out ((,class (:background ,bg-alt :foreground ,red :weight bold))))
   `(helm-buffer-size ((,class (:foreground ,fg-alt))))
   `(helm-candidate-number ((,class (:foreground ,cyan-active))))
   `(helm-candidate-number-suspended ((,class (:foreground ,yellow-active))))
   `(helm-delete-async-message ((,class (:foreground ,magenta-active :weight bold))))
   `(helm-eob-line ((,class (:background ,bg-main :foreground ,fg-main))))
   `(helm-etags-file ((,class (:foreground ,fg-dim :underline t))))
   `(helm-ff-denied ((,class (:inherit modus-theme-intense-red))))
   `(helm-ff-directory ((,class (:inherit helm-buffer-directory))))
   `(helm-ff-dirs ((,class (:foreground ,blue-alt-other :weight bold))))
   `(helm-ff-dotted-directory ((,class (:background ,bg-alt :foreground ,fg-alt :weight bold))))
   `(helm-ff-dotted-symlink-directory ((,class (:inherit helm-ff-dotted-directory :underline t))))
   `(helm-ff-executable ((,class (:foreground ,magenta-alt))))
   `(helm-ff-file ((,class (:foreground ,fg-main))))
   `(helm-ff-invalid-symlink ((,class (:foreground ,red :underline t))))
   `(helm-ff-pipe ((,class (:inherit modus-theme-subtle-magenta))))
   `(helm-ff-prefix ((,class (:inherit modus-theme-subtle-yellow))))
   `(helm-ff-socket ((,class (:foreground ,red-alt-other))))
   `(helm-ff-suid ((,class (:inherit modus-theme-refine-red))))
   `(helm-ff-symlink ((,class (:foreground ,cyan :underline t))))
   `(helm-ff-truename ((,class (:foreground ,blue-alt-other))))
   `(helm-grep-cmd-line ((,class (:foreground ,yellow-alt-other))))
   `(helm-grep-file ((,class (:foreground ,cyan-alt :weight bold))))
   `(helm-grep-finish ((,class (:foreground ,green))))
   `(helm-grep-lineno ((,class (:foreground ,fg-special-warm))))
   `(helm-grep-match ((,class (:inherit modus-theme-subtle-blue))))
   `(helm-header ((,class (:background ,bg-alt :foreground ,fg-main :weight bold))))
   `(helm-header-line-left-margin ((,class (:foreground ,yellow-intense :weight bold))))
   `(helm-history-deleted ((,class (:inherit modus-theme-intense-red :weight bold))))
   `(helm-history-remote ((,class (:foreground ,red-alt-other))))
   `(helm-lisp-completion-info ((,class (:foreground ,fg-special-warm))))
   `(helm-lisp-show-completion ((,class (:inherit modus-theme-refine-yellow))))
   `(helm-locate-finish ((,class (:foreground ,green))))
   `(helm-match ((,class (:inherit modus-theme-refine-green :weight bold))))
   `(helm-match-item ((,class (:inherit modus-theme-intense-blue))))
   `(helm-minibuffer-prompt ((,class (:inherit minibuffer-prompt))))
   `(helm-moccur-buffer ((,class (:foreground ,cyan-alt-other :underline t))))
   `(helm-mode-prefix ((,class (:inherit modus-theme-intense-magenta))))
   `(helm-non-file-buffer ((,class (:foreground ,fg-alt))))
   `(helm-prefarg ((,class (:foreground ,red-alt))))
   `(helm-resume-need-update ((,class (:inherit modus-theme-intense-red))))
   `(helm-selection ((,class (:inherit modus-theme-special-mild :weight bold :box t))))
   `(helm-separator ((,class (:foreground ,fg-special-mild))))
   `(helm-source-header ((,class (:inherit modus-theme-special-cold :weight bold
                                           ,@(when modus-operandi-theme-scale-headings
                                               (list :height modus-operandi-theme-scale-4))))))
   `(helm-top-columns ((,class (:inherit helm-header))))
   `(helm-visible-mark ((,class (:inherit modus-theme-subtle-cyan))))
   ;;;; highlight region or ad-hoc regexp
   `(hi-black-b ((,class ((:background ,fg-main :foreground ,bg-main)))))
   `(hi-blue ((,class (:background ,bg-alt :foreground ,blue :underline t))))
   `(hi-blue-b ((,class (:inherit modus-theme-intense-blue))))
   `(hi-green ((,class (:background ,bg-alt :foreground ,green :underline t))))
   `(hi-green-b ((,class (:inherit modus-theme-intense-green))))
   `(hi-pink ((,class (:background ,bg-alt :foreground ,magenta :underline t))))
   `(hi-red-b ((,class (:inherit modus-theme-intense-red))))
   `(hi-yellow ((,class (:background ,bg-alt :foreground ,yellow :underline t))))
   `(highlight ((,class (:inherit modus-theme-subtle-blue))))
   `(highlight-changes ((,class (:foreground ,yellow-alt-other))))
   `(highlight-changes-delete ((,class (:foreground ,red-alt-other :underline t))))
   `(hl-line ((,class (:background ,bg-hl-line))))
   `(region ((,class (:background ,bg-region :foreground ,fg-main))))
   `(secondary-selection ((,class (:background ,bg-inactive :foreground ,fg-inactive))))
   ;;;; highlight-blocks
   `(highlight-blocks-depth-1-face ((,class (:background ,bg-dim :foreground ,fg-main))))
   `(highlight-blocks-depth-2-face ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(highlight-blocks-depth-3-face ((,class (:background ,bg-special-cold :foreground ,fg-main))))
   `(highlight-blocks-depth-4-face ((,class (:background ,bg-special-calm :foreground ,fg-main))))
   `(highlight-blocks-depth-5-face ((,class (:background ,bg-special-warm :foreground ,fg-main))))
   `(highlight-blocks-depth-6-face ((,class (:background ,bg-special-mild :foreground ,fg-main))))
   `(highlight-blocks-depth-7-face ((,class (:background ,bg-inactive :foreground ,fg-main))))
   `(highlight-blocks-depth-8-face ((,class (:background ,bg-active :foreground ,fg-main))))
   `(highlight-blocks-depth-9-face ((,class (:background ,cyan-subtle-bg :foreground ,fg-main))))
   ;;;; hl-fill-column
   `(hl-fill-column-face ((,class (:background ,bg-active :foreground ,fg-active))))
   ;;;; hydra
   `(hydra-face-amaranth ((,class (:foreground ,yellow-intense :weight bold))))
   `(hydra-face-blue ((,class (:foreground ,blue-intense :weight bold))))
   `(hydra-face-pink ((,class (:foreground ,magenta-intense :weight bold))))
   `(hydra-face-red ((,class (:foreground ,red-intense :weight bold))))
   `(hydra-face-teal ((,class (:foreground ,cyan-intense :weight bold))))
   ;;;; ido-mode
   `(flx-highlight-face ((,class (:inherit modus-theme-intense-magenta))))
   `(ido-first-match ((,class (:foreground ,magenta :weight bold))))
   `(ido-incomplete-regexp ((,class (:inherit error))))
   `(ido-indicator ((,class (:inherit modus-theme-subtle-yellow))))
   `(ido-only-match ((,class (:foreground ,magenta-intense :weight bold))))
   `(ido-subdir ((,class (:foreground ,blue-alt-other))))
   `(ido-virtual ((,class (:foreground ,yellow-alt-other))))
   ;;;; iedit
   `(iedit-occurrence ((,class (:inherit modus-theme-refine-blue))))
   `(iedit-read-only-occurrence ((,class (:inherit modus-theme-intense-yellow))))
   ;;;; info
   `(Info-quoted ((,class (:foreground ,magenta)))) ; the capitalisation is canonical
   `(info-header-node ((,class (:foreground ,fg-special-warm))))
   `(info-header-xref ((,class (:foreground ,blue-active))))
   `(info-index-match ((,class (:inherit match))))
   `(info-menu-star ((,class (:foreground ,fg-main))))
   `(info-node ((,class ((:weight bold)))))
   ;;;; info-colors
   `(info-colors-lisp-code-block ((,class (:inherit fixed-pitch))))
   `(info-colors-ref-item-command ((,class (:foreground ,magenta))))
   `(info-colors-ref-item-constant ((,class (:foreground ,blue-alt-other))))
   `(info-colors-ref-item-function ((,class (:foreground ,magenta))))
   `(info-colors-ref-item-macro ((,class (:foreground ,magenta-alt-other :weight ,modus-theme-bold))))
   `(info-colors-ref-item-other ((,class (:foreground ,cyan))))
   `(info-colors-ref-item-special-form ((,class (:foreground ,magenta-alt-other :weight ,modus-theme-bold))))
   `(info-colors-ref-item-syntax-class ((,class (:foreground ,magenta))))
   `(info-colors-ref-item-type ((,class (:foreground ,magenta-alt))))
   `(info-colors-ref-item-user-option ((,class (:foreground ,cyan))))
   `(info-colors-ref-item-variable ((,class (:foreground ,cyan))))
   ;;;; isearch, occur, and the like
   `(isearch ((,class (:inherit modus-theme-intense-green :weight bold))))
   `(isearch-fail ((,class (:inherit modus-theme-refine-red))))
   `(lazy-highlight ((,class (:inherit modus-theme-refine-cyan))))
   `(match ((,class (:inherit modus-theme-special-calm))))
   `(query-replace ((,class (:inherit modus-theme-intense-yellow :weight bold))))
   ;;;; ivy
   `(ivy-action ((,class (:foreground ,red-alt :weight bold))))
   `(ivy-completions-annotations ((,class (:foreground ,fg-special-cold :slant ,modus-theme-slant))))
   `(ivy-confirm-face ((,class (:foreground ,cyan))))
   `(ivy-current-match ((,class (,@(and (>= emacs-major-version 27) '(:extend t))
                                 :inherit modus-theme-intense-cyan :weight bold))))
   `(ivy-cursor ((,class (:background ,fg-main :foreground ,bg-main))))
   `(ivy-grep-info ((,class (:foreground ,cyan-alt))))
   `(ivy-grep-line-number ((,class (:foreground ,fg-special-warm))))
   `(ivy-highlight-face ((,class (:foreground ,magenta))))
   `(ivy-match-required-face ((,class (:inherit error))))
   `(ivy-minibuffer-match-face-1 ((,class (:inherit modus-theme-intense-neutral))))
   `(ivy-minibuffer-match-face-2 ((,class (:inherit modus-theme-refine-green :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:inherit modus-theme-refine-cyan :weight bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:inherit modus-theme-refine-magenta :weight bold))))
   `(ivy-minibuffer-match-highlight ((,class (:inherit modus-theme-subtle-blue :weight bold))))
   `(ivy-modified-buffer ((,class (:foreground ,yellow :slant ,modus-theme-slant))))
   `(ivy-modified-outside-buffer ((,class (:foreground ,yellow-alt :slant ,modus-theme-slant))))
   `(ivy-org ((,class (:foreground ,cyan-alt-other))))
   `(ivy-prompt-match ((,class (:inherit ivy-current-match))))
   `(ivy-remote ((,class (:foreground ,magenta))))
   `(ivy-separator ((,class (:foreground ,fg-alt))))
   `(ivy-subdir ((,class (:foreground ,blue-alt-other))))
   `(ivy-virtual ((,class (:foreground ,magenta-alt-other))))
   `(ivy-yanked-word ((,class (:inherit modus-theme-refine-blue))))
   ;;;; ivy-posframe
   `(ivy-posframe ((,class (:background ,bg-dim :foreground ,fg-main))))
   `(ivy-posframe-border ((,class (:background ,bg-active))))
   `(ivy-posframe-cursor ((,class (:background ,fg-main :foreground ,bg-main))))
   ;;;; keycast
   `(keycast-command ((,class (:foreground ,red-active :weight bold))))
   `(keycast-key ((,class (:height 1.2 :inherit modus-theme-special-warm :weight bold :box (:line-width -3 :style released-button)))))
   ;;;; line numbers (display-line-numbers-mode and global variant)
   `(line-number ((,class (:background ,bg-dim :foreground ,fg-alt))))
   `(line-number-current-line ((,class (:background ,bg-active :foreground ,fg-active :weight bold))))
   ;;;; lsp-mode
   `(lsp-face-highlight-read ((,class (:inherit modus-theme-subtle-blue :underline t))))
   `(lsp-face-highlight-textual ((,class (:inherit modus-theme-subtle-blue))))
   `(lsp-face-highlight-write ((,class (:inherit modus-theme-refine-blue :weight bold))))
   `(lsp-face-semhl-constant ((,class (:foreground ,blue-alt-other))))
   `(lsp-face-semhl-deprecated
     ((,(append '((supports :underline (:style wave))) class)
       (:foreground ,yellow :underline (:style wave)))
      (,class (:foreground ,yellow :underline t))))
   `(lsp-face-semhl-enummember ((,class (:foreground ,blue-alt-other))))
   `(lsp-face-semhl-field ((,class (:foreground ,cyan-alt))))
   `(lsp-face-semhl-field-static ((,class (:foreground ,cyan-alt :slant ,modus-theme-slant))))
   `(lsp-face-semhl-function ((,class (:foreground ,magenta))))
   `(lsp-face-semhl-method ((,class (:foreground ,magenta))))
   `(lsp-face-semhl-namespace ((,class (:foreground ,magenta-alt :weight ,modus-theme-bold))))
   `(lsp-face-semhl-preprocessor ((,class (:foreground ,magenta))))
   `(lsp-face-semhl-static-method ((,class (:foreground ,magenta :slant ,modus-theme-slant))))
   `(lsp-face-semhl-type-class ((,class (:foreground ,magenta-alt))))
   `(lsp-face-semhl-type-enum ((,class (:foreground ,magenta-alt))))
   `(lsp-face-semhl-type-primitive ((,class (:foreground ,magenta-alt :slant ,modus-theme-slant))))
   `(lsp-face-semhl-type-template ((,class (:foreground ,magenta-alt :slant ,modus-theme-slant))))
   `(lsp-face-semhl-type-typedef ((,class (:foreground ,magenta-alt :slant ,modus-theme-slant))))
   `(lsp-face-semhl-variable ((,class (:foreground ,cyan))))
   `(lsp-face-semhl-variable-local ((,class (:foreground ,cyan))))
   `(lsp-face-semhl-variable-parameter ((,class (:foreground ,cyan-alt-other))))
   `(lsp-lens-face ((,class (:height 0.8 :foreground ,fg-alt))))
   `(lsp-lens-mouse-face ((,class (:height 0.8 :foreground ,blue-alt-other :underline t))))
   `(lsp-ui-doc-background ((,class (:background ,bg-alt))))
   `(lsp-ui-doc-header ((,class (:background ,bg-header :foreground ,fg-header))))
   `(lsp-ui-doc-url ((,class (:foreground ,blue-alt-other :underline t))))
   `(lsp-ui-peek-filename ((,class (:foreground ,fg-special-warm))))
   `(lsp-ui-peek-footer ((,class (:background ,bg-header :foreground ,fg-header))))
   `(lsp-ui-peek-header ((,class (:background ,bg-header :foreground ,fg-header))))
   `(lsp-ui-peek-highlight ((,class (:inherit modus-theme-subtle-blue))))
   `(lsp-ui-peek-line-number ((,class (:foreground ,fg-alt))))
   `(lsp-ui-peek-list ((,class (:background ,bg-dim))))
   `(lsp-ui-peek-peek ((,class (:background ,bg-alt))))
   `(lsp-ui-peek-selection ((,class (:inherit modus-theme-subtle-cyan))))
   `(lsp-ui-sideline-code-action ((,class (:foreground ,yellow))))
   `(lsp-ui-sideline-current-symbol ((,class (:height 0.99 :box (:line-width -1 :style nil) :foreground ,fg-main :weight bold))))
   `(lsp-ui-sideline-symbol ((,class (:height 0.99 :box (:line-width -1 :style nil) :foreground ,fg-alt :weight bold))))
   `(lsp-ui-sideline-symbol-info ((,class (:height 0.99 :slant italic))))
   ;;;; magit
   `(magit-bisect-bad ((,class (:foreground ,red-alt-other))))
   `(magit-bisect-good ((,class (:foreground ,green-alt-other))))
   `(magit-bisect-skip ((,class (:foreground ,yellow-alt-other))))
   `(magit-blame-date ((,class (:foreground ,fg-dim))))
   `(magit-blame-dimmed ((,class (:foreground ,fg-inactive))))
   `(magit-blame-hash ((,class (:foreground ,fg-special-warm))))
   `(magit-blame-heading ((,class (:background ,bg-main :foreground ,fg-special-cold :weight bold))))
   `(magit-blame-highlight ((,class (:inherit modus-theme-special-cold :weight bold))))
   `(magit-blame-margin ((,class (:inherit magit-blame-highlight))))
   `(magit-blame-name ((,class (:foreground ,fg-main))))
   `(magit-blame-summary ((,class (:foreground ,fg-main))))
   `(magit-branch-current ((,class (:foreground ,magenta-alt-other))))
   `(magit-branch-local ((,class (:foreground ,blue-alt-other))))
   `(magit-branch-remote ((,class (:foreground ,magenta-alt))))
   `(magit-branch-remote-head ((,class (:foreground ,magenta :box t))))
   `(magit-branch-upstream ((,class (:slant italic))))
   `(magit-cherry-equivalent ((,class (:background ,bg-main :foreground ,magenta-intense))))
   `(magit-cherry-unmatched ((,class (:background ,bg-main :foreground ,cyan-intense))))
   `(magit-diff-added ((,class (:inherit modus-theme-subtle-green))))
   `(magit-diff-added-highlight ((,class (:inherit modus-theme-refine-green))))
   `(magit-diff-base ((,class (:inherit modus-theme-subtle-yellow))))
   `(magit-diff-base-highlight ((,class (:inherit modus-theme-refine-yellow))))
   `(magit-diff-changed ((,class (:weight bold))))
   `(magit-diff-changed-highlight ((,class (:weight bold))))
   `(magit-diff-context ((,class (:background ,bg-main :foreground ,fg-alt))))
   `(magit-diff-context-highlight ((,class (:background ,bg-alt :foreground ,fg-alt))))
   `(magit-diff-file-heading ((,class (:background ,bg-main :foreground ,fg-special-cold :weight bold))))
   `(magit-diff-file-heading-highlight ((,class (:inherit modus-theme-special-cold :weight bold))))
   `(magit-diff-file-heading-selection ((,class (:background ,bg-alt :foreground ,cyan))))
   `(magit-diff-hunk-heading ((,class (:background ,bg-main :foreground ,fg-special-warm :weight bold))))
   `(magit-diff-hunk-heading-highlight ((,class (:inherit modus-theme-special-cold :weight bold))))
   `(magit-diff-hunk-heading-selection ((,class (:inherit magit-diff-file-heading-selection))))
   `(magit-diff-hunk-region ((,class (:weight bold)))) ; has no effect?
   `(magit-diff-lines-boundary ((,class (:background ,fg-main))))
   `(magit-diff-lines-heading ((,class (:inherit modus-theme-refine-magenta))))
   `(magit-diff-removed ((,class (:inherit modus-theme-subtle-red))))
   `(magit-diff-removed-highlight ((,class (:inherit modus-theme-refine-red))))
   `(magit-diffstat-added ((,class (:background ,bg-main :foreground ,green))))
   `(magit-diffstat-removed ((,class (:background ,bg-main :foreground ,red))))
   `(magit-dimmed ((,class (:foreground ,fg-alt))))
   `(magit-filename ((,class (:foreground ,fg-special-cold))))
   `(magit-hash ((,class (:foreground ,fg-special-warm))))
   `(magit-log-author ((,class (:foreground ,fg-special-mild))))
   `(magit-log-date ((,class (:foreground ,fg-dim))))
   `(magit-log-graph ((,class (:foreground ,fg-dim))))
   `(magit-mode-line-process ((,class (:foreground ,blue-active :weight bold))))
   `(magit-mode-line-process-error ((,class (:foreground ,red-active :weight bold))))
   `(magit-process-ng ((,class (:inherit error))))
   `(magit-process-ok ((,class (:inherit success))))
   `(magit-reflog-amend ((,class (:background ,bg-main :foreground ,magenta-intense))))
   `(magit-reflog-checkout ((,class (:background ,bg-main :foreground ,blue-intense))))
   `(magit-reflog-cherry-pick ((,class (:background ,bg-main :foreground ,green-intense))))
   `(magit-reflog-commit ((,class (:background ,bg-main :foreground ,green-intense))))
   `(magit-reflog-merge ((,class (:background ,bg-main :foreground ,green-intense))))
   `(magit-reflog-other ((,class (:background ,bg-main :foreground ,cyan-intense))))
   `(magit-reflog-rebase ((,class (:background ,bg-main :foreground ,magenta-intense))))
   `(magit-reflog-remote ((,class (:background ,bg-main :foreground ,cyan-intense))))
   `(magit-reflog-reset ((,class (:background ,bg-main :foreground ,red-intense))))
   `(magit-refname ((,class (:foreground ,fg-alt))))
   `(magit-refname-pullreq ((,class (:foreground ,fg-alt))))
   `(magit-refname-stash ((,class (:foreground ,fg-alt))))
   `(magit-refname-wip ((,class (:foreground ,fg-alt))))
   `(magit-section ((,class (:background ,bg-dim :foreground ,fg-main))))
   `(magit-section-heading ((,class (:foreground ,cyan-active))))
   `(magit-section-heading-selection ((,class (:inherit modus-theme-refine-cyan :weight bold))))
   `(magit-section-highlight ((,class (:background ,bg-alt))))
   `(magit-sequence-done ((,class (:foreground ,green-alt))))
   `(magit-sequence-drop ((,class (:foreground ,red-alt))))
   `(magit-sequence-exec ((,class (:foreground ,magenta-alt))))
   `(magit-sequence-head ((,class (:foreground ,cyan-alt))))
   `(magit-sequence-onto ((,class (:foreground ,fg-alt))))
   `(magit-sequence-part ((,class (:foreground ,yellow-alt))))
   `(magit-sequence-pick ((,class (:foreground ,blue-alt))))
   `(magit-sequence-stop ((,class (:foreground ,red))))
   `(magit-signature-bad ((,class (:background ,bg-main :foreground ,red-intense :weight bold))))
   `(magit-signature-error ((,class (:background ,bg-main :foreground ,red-intense))))
   `(magit-signature-expired ((,class (:background ,bg-main :foreground ,yellow-intense))))
   `(magit-signature-expired-key ((,class (:background ,bg-main :foreground ,yellow-intense))))
   `(magit-signature-good ((,class (:background ,bg-main :foreground ,green-intense))))
   `(magit-signature-revoked ((,class (:background ,bg-main :foreground ,magenta-intense))))
   `(magit-signature-untrusted ((,class (:background ,bg-main :foreground ,cyan-intense))))
   `(magit-tag ((,class (:foreground ,yellow-alt-other))))
   ;;;; markdown-mode
   `(markdown-blockquote-face ((,class (:background ,bg-dim :foreground ,fg-special-warm :slant ,modus-theme-slant))))
   `(markdown-bold-face ((,class (:weight bold))))
   `(markdown-code-face ((,class (:inherit fixed-pitch))))
   `(markdown-comment-face ((,class (:foreground ,fg-alt :slant ,modus-theme-slant))))
   `(markdown-footnote-marker-face ((,class (:foreground ,cyan-alt :weight bold))))
   `(markdown-footnote-text-face ((,class (:foreground ,fg-main :slant ,modus-theme-slant))))
   `(markdown-gfm-checkbox-face ((,class (:foreground ,cyan-alt-other))))
   `(markdown-header-delimiter-face ((,class (:foreground ,fg-dim :weight normal))))
   `(markdown-header-face ((,class (:weight bold))))
   `(markdown-header-rule-face ((,class (:foreground ,fg-special-warm :weight bold))))
   `(markdown-hr-face ((,class (:foreground ,fg-special-warm :weight bold))))
   `(markdown-html-attr-name-face ((,class (:foreground ,cyan))))
   `(markdown-html-attr-value-face ((,class (:foreground ,blue))))
   `(markdown-html-entity-face ((,class (:foreground ,cyan))))
   `(markdown-html-tag-delimiter-face ((,class (:foreground ,fg-special-mild))))
   `(markdown-html-tag-name-face ((,class (:foreground ,magenta-alt))))
   `(markdown-inline-code-face ((,class (:foreground ,magenta))))
   `(markdown-italic-face ((,class (:slant italic))))
   `(markdown-language-info-face ((,class (:foreground ,fg-special-cold))))
   `(markdown-language-keyword-face ((,class (:foreground ,green-alt-other))))
   `(markdown-line-break-face ((,class (:inherit modus-theme-refine-cyan :underline t))))
   `(markdown-link-face ((,class (:inherit link))))
   `(markdown-link-title-face ((,class (:foreground ,fg-special-cold :slant ,modus-theme-slant))))
   `(markdown-list-face ((,class (:foreground ,fg-dim))))
   `(markdown-markup-face ((,class (:foreground ,fg-alt))))
   `(markdown-math-face ((,class (:foreground ,magenta-alt-other))))
   `(markdown-metadata-key-face ((,class (:foreground ,cyan-alt-other))))
   `(markdown-metadata-value-face ((,class (:foreground ,blue-alt))))
   `(markdown-missing-link-face ((,class (:foreground ,yellow :weight bold))))
   `(markdown-plain-url-face ((,class (:inherit markdown-link-face))))
   `(markdown-pre-face ((,class (:foreground ,fg-special-mild))))
   `(markdown-reference-face ((,class (:inherit markdown-markup-face))))
   `(markdown-strike-through-face ((,class (:strike-through t))))
   `(markdown-table-face ((,class (:foreground ,fg-special-cold))))
   `(markdown-url-face ((,class (:foreground ,blue))))
   ;;;; mentor
   `(mentor-download-message ((,class (:foreground ,fg-special-warm))))
   `(mentor-download-name ((,class (:foreground ,fg-special-cold))))
   `(mentor-download-progress ((,class (:foreground ,blue-alt-other))))
   `(mentor-download-size ((,class (:foreground ,magenta-alt-other))))
   `(mentor-download-speed-down ((,class (:foreground ,cyan-alt))))
   `(mentor-download-speed-up ((,class (:foreground ,red-alt))))
   `(mentor-download-state ((,class (:foreground ,yellow-alt))))
   `(mentor-highlight-face ((,class (:inherit modus-theme-subtle-blue))))
   `(mentor-tracker-name ((,class (:foreground ,magenta-alt))))
   ;;;; messages
   `(message-cited-text-1 ((,class (:foreground ,cyan))))
   `(message-cited-text-2 ((,class (:foreground ,green))))
   `(message-cited-text-3 ((,class (:foreground ,yellow))))
   `(message-cited-text-4 ((,class (:foreground ,red))))
   `(message-header-cc ((,class (:foreground ,blue-alt))))
   `(message-header-name ((,class (:foreground ,cyan-alt))))
   `(message-header-newsgroups ((,class (:foreground ,blue :weight bold))))
   `(message-header-other ((,class (:foreground ,cyan-alt-other :weight bold))))
   `(message-header-subject ((,class (:foreground ,magenta-alt-other :weight bold))))
   `(message-header-to ((,class (:foreground ,magenta-alt :weight bold))))
   `(message-header-xheader ((,class (:foreground ,blue-alt-other))))
   `(message-mml ((,class (:foreground ,green-alt))))
   `(message-separator ((,class (:background ,bg-alt :foreground ,fg-special-warm))))
   ;;;; modeline
   `(mode-line ((,class (:box (:line-width 1 :color ,fg-inactive) :background ,bg-active :foreground ,fg-active))))
   `(mode-line-buffer-id ((,class (:weight bold))))
   `(mode-line-emphasis ((,class (:foreground ,blue-active :weight bold :box t))))
   `(mode-line-highlight ((,class (:inherit modus-theme-active-blue :box (:line-width -1 :style pressed-button)))))
   `(mode-line-inactive ((,class (:box (:color ,bg-inactive) :background ,bg-inactive :foreground ,fg-inactive))))
   ;;;; mood-line
   `(mood-line-modified ((,class (:foreground ,magenta-active))))
   `(mood-line-status-error ((,class (:foreground ,red-active :weight bold))))
   `(mood-line-status-info ((,class (:foreground ,cyan-active))))
   `(mood-line-status-neutral ((,class (:foreground ,blue-active))))
   `(mood-line-status-success ((,class (:foreground ,green-active))))
   `(mood-line-status-warning ((,class (:foreground ,yellow-active :weight bold))))
   `(mood-line-unimportant ((,class (:foreground ,fg-inactive))))
   ;;;; mu4e
   `(mu4e-attach-number-face ((,class (:foreground ,cyan-alt :weight bold))))
   `(mu4e-cited-1-face ((,class (:foreground ,blue-alt))))
   `(mu4e-cited-2-face ((,class (:foreground ,red-alt))))
   `(mu4e-cited-3-face ((,class (:foreground ,green-alt))))
   `(mu4e-cited-4-face ((,class (:foreground ,magenta-alt))))
   `(mu4e-cited-5-face ((,class (:foreground ,cyan-alt))))
   `(mu4e-cited-6-face ((,class (:foreground ,blue-alt-other))))
   `(mu4e-cited-7-face ((,class (:foreground ,red-alt-other))))
   `(mu4e-compose-header-face ((,class (:foreground ,green-alt))))
   `(mu4e-compose-separator-face ((,class (:background ,bg-active :foreground ,fg-main :box t))))
   `(mu4e-contact-face ((,class (:foreground ,cyan))))
   `(mu4e-context-face ((,class (:foreground ,blue-active))))
   `(mu4e-draft-face ((,class (:foreground ,magenta-refine-fg))))
   `(mu4e-flagged-face ((,class (:foreground ,red-alt-other))))
   `(mu4e-footer-face ((,class (:foreground ,fg-alt))))
   `(mu4e-forwarded-face ((,class (:foreground ,green-alt-other))))
   `(mu4e-header-face ((,class (:foreground ,fg-main))))
   `(mu4e-header-highlight-face ((,class (,@(and (>= emacs-major-version 27) '(:extend t))
                                          :background ,bg-hl-line))))
   `(mu4e-header-key-face ((,class (:foreground ,fg-dim :weight bold))))
   `(mu4e-header-marks-face ((,class (:foreground ,magenta-alt :weight bold))))
   `(mu4e-header-title-face ((,class (:foreground ,fg-special-warm))))
   `(mu4e-header-value-face ((,class (:foreground ,magenta-alt-other))))
   `(mu4e-highlight-face ((,class (:foreground ,blue-intense :weight bold))))
   `(mu4e-link-face ((,class (:inherit link))))
   `(mu4e-modeline-face ((,class (:foreground ,magenta-active))))
   `(mu4e-moved-face ((,class (:foreground ,blue-refine-fg))))
   `(mu4e-ok-face ((,class (:foreground ,green-intense :weight bold))))
   `(mu4e-region-code ((,class (:inherit modus-theme-special-calm))))
   `(mu4e-replied-face ((,class (:foreground ,cyan-refine-fg))))
   `(mu4e-special-header-value-face ((,class (:foreground ,magenta :weight bold))))
   `(mu4e-system-face ((,class (:foreground ,fg-alt))))
   `(mu4e-title-face ((,class (:foreground ,fg-main))))
   `(mu4e-unread-face ((,class (:foreground ,fg-main :weight bold))))
   `(mu4e-url-number-face ((,class (:foreground ,blue-alt-other :weight bold))))
   `(mu4e-view-body-face ((,class (:background ,bg-main :foreground ,fg-main))))
   `(mu4e-warning-face ((,class (:inherit warning))))
   ;;;; mu4e-conversation
   `(mu4e-conversation-header ((,class (:inherit modus-theme-special-cold))))
   `(mu4e-conversation-sender-1 ((,class (:foreground ,fg-special-warm))))
   `(mu4e-conversation-sender-2 ((,class (:foreground ,fg-special-cold))))
   `(mu4e-conversation-sender-3 ((,class (:foreground ,fg-special-mild))))
   `(mu4e-conversation-sender-4 ((,class (:foreground ,fg-alt))))
   `(mu4e-conversation-sender-5 ((,class (:foreground ,yellow-refine-fg))))
   `(mu4e-conversation-sender-6 ((,class (:foreground ,cyan-refine-fg))))
   `(mu4e-conversation-sender-7 ((,class (:foreground ,green-refine-fg))))
   `(mu4e-conversation-sender-8 ((,class (:foreground ,blue-refine-fg))))
   `(mu4e-conversation-sender-me ((,class (:foreground ,fg-main))))
   `(mu4e-conversation-unread ((,class (:weight bold))))
   ;;;; neotree
   `(neo-banner-face ((,class (:foreground ,magenta))))
   `(neo-button-face ((,class (:inherit button))))
   `(neo-dir-link-face ((,class (:foreground ,blue :weight bold))))
   `(neo-expand-btn-face ((,class (:foreground ,cyan))))
   `(neo-file-link-face ((,class (:foreground ,fg-main))))
   `(neo-header-face ((,class (:foreground ,fg-main :weight bold))))
   `(neo-root-dir-face ((,class (:foreground ,cyan-alt :weight bold))))
   `(neo-vc-added-face ((,class (:foreground ,green))))
   `(neo-vc-conflict-face ((,class (:foreground ,red :Weight bold))))
   `(neo-vc-default-face ((,class (:foreground ,fg-main))))
   `(neo-vc-edited-face ((,class (:foreground ,yellow))))
   `(neo-vc-ignored-face ((,class (:foreground ,fg-inactive))))
   `(neo-vc-missing-face ((,class (:foreground ,red-alt))))
   `(neo-vc-needs-merge-face ((,class (:foreground ,magenta-alt))))
   `(neo-vc-needs-update-face ((,class (:underline t))))
   `(neo-vc-removed-face ((,class (:strike-through t))))
   `(neo-vc-unlocked-changes-face ((,class (:inherit modus-theme-refine-blue))))
   `(neo-vc-up-to-date-face ((,class (:foreground ,fg-alt))))
   `(neo-vc-user-face ((,class (:foreground ,magenta))))
   ;;;; org
   `(org-agenda-calendar-event ((,class (:foreground ,blue-alt))))
   `(org-agenda-calendar-sexp ((,class (:foreground ,cyan-alt))))
   `(org-agenda-clocking ((,class (:inherit modus-theme-special-cold))))
   `(org-agenda-column-dateline ((,class (:inherit modus-theme-subtle-neutral))))
   `(org-agenda-current-time ((,class (:inherit modus-theme-intense-cyan))))
   `(org-agenda-date ((,class (:foreground ,fg-main))))
   `(org-agenda-date-today ((,class (:inherit modus-theme-subtle-cyan :weight bold :box t))))
   `(org-agenda-date-weekend ((,class (:foreground ,fg-alt))))
   `(org-agenda-diary ((,class (:background ,bg-main :foreground ,fg-main))))
   `(org-agenda-dimmed-todo-face ((,class (:inherit modus-theme-subtle-neutral))))
   `(org-agenda-done ((,class (:foreground ,green))))
   `(org-agenda-filter-category ((,class (:background ,bg-active :foreground ,fg-main :box t))))
   `(org-agenda-filter-effort ((,class (:background ,bg-active :foreground ,fg-main :box t))))
   `(org-agenda-filter-regexp ((,class (:background ,bg-active :foreground ,fg-main :box t))))
   `(org-agenda-filter-tags ((,class (:background ,bg-active :foreground ,fg-main :box t))))
   `(org-agenda-restriction-lock ((,class (:background ,bg-dim :foreground ,fg-dim))))
   `(org-agenda-structure ((,class (:foreground ,fg-special-mild))))
   `(org-archived ((,class (:background ,bg-alt :foreground ,fg-alt))))
   `(org-block ((,class (:background ,bg-main :foreground ,fg-main))))
   `(org-block-begin-line ((,class (:background ,bg-dim :foreground ,fg-special-mild))))
   `(org-block-end-line ((,class (:inherit org-block-begin-line))))
   `(org-checkbox ((,class (:weight bold))))
   `(org-checkbox-statistics-done ((,class (:foreground ,green :weight bold))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,yellow :weight bold))))
   `(org-clock-overlay ((,class (:inherit modus-theme-special-cold))))
   `(org-code ((,class (:foreground ,magenta))))
   `(org-column ((,class (:background ,bg-alt))))
   `(org-column-title ((,class (:underline t :background ,bg-alt :weight bold))))
   `(org-date ((,class (:foreground ,cyan))))
   `(org-date-selected ((,class (:inherit modus-theme-intense-cyan :weight bold))))
   `(org-default ((,class (:background ,bg-main :foreground ,fg-main))))
   `(org-document-info ((,class (:foreground ,fg-special-cold))))
   `(org-document-title ((,class (:foreground ,fg-special-cold :weight bold))))
   `(org-done ((,class (:foreground ,green))))
   `(org-drawer ((,class (:foreground ,cyan-alt))))
   `(org-ellipsis ((,class (:foreground nil)))) ; inherits from the heading's colour
   `(org-footnote ((,class (:foreground ,blue-alt :underline t))))
   `(org-formula ((,class (:foreground ,red-alt))))
   `(org-habit-alert-face ((,class (:inherit modus-theme-intense-yellow))))
   `(org-habit-alert-future-face ((,class (:inherit modus-theme-subtle-yellow))))
   `(org-habit-clear-face ((,class (:inherit modus-theme-intense-magenta))))
   `(org-habit-clear-future-face ((,class (:inherit modus-theme-subtle-magenta))))
   `(org-habit-overdue-face ((,class (:inherit modus-theme-intense-red))))
   `(org-habit-overdue-future-face ((,class (:inherit modus-theme-subtle-red))))
   `(org-habit-ready-face ((,class (:inherit modus-theme-intense-blue))))
   `(org-habit-ready-future-face ((,class (:inherit modus-theme-subtle-blue))))
   `(org-headline-done ((,class (:foreground ,green-refine-fg))))
   `(org-hide ((,class (:foreground ,fg-main))))
   `(org-latex-and-related ((,class (:foreground ,magenta-refine-fg))))
   `(org-level-1 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-main :weight bold
                                       ,@(when modus-operandi-theme-scale-headings
                                           (list :height modus-operandi-theme-scale-4))))))
   `(org-level-2 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-special-warm :weight bold
                                       ,@(when modus-operandi-theme-scale-headings
                                          (list :height modus-operandi-theme-scale-3))))))
   `(org-level-3 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-special-cold :weight bold
                                       ,@(when modus-operandi-theme-scale-headings
                                          (list :height modus-operandi-theme-scale-2))))))
   `(org-level-4 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-special-mild :weight bold
                                       ,@(when modus-operandi-theme-scale-headings
                                          (list :height modus-operandi-theme-scale-1))))))
   `(org-level-5 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-dim :weight bold))))
   `(org-level-6 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-alt :weight bold))))
   `(org-level-7 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,cyan-active :weight bold))))
   `(org-level-8 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,magenta-active :weight bold))))
   `(org-link ((,class (:inherit link))))
   `(org-list-dt ((,class (:foreground ,fg-dim :weight bold))))
   `(org-macro ((,class (:inherit org-latex-and-related))))
   `(org-meta-line ((,class (:foreground ,fg-alt :slant ,modus-theme-slant))))
   `(org-mode-line-clock ((,class (:background ,bg-main :foreground ,fg-main))))
   `(org-mode-line-clock-overrun ((,class (:inherit modus-theme-active-red))))
   `(org-priority ((,class (:foreground ,magenta))))
   `(org-quote ((,class (:inherit org-block :foreground ,fg-special-cold))))
   `(org-scheduled ((,class (:foreground ,fg-special-cold))))
   `(org-scheduled-previously ((,class (:foreground ,fg-special-warm))))
   `(org-scheduled-today ((,class (:foreground ,yellow-alt-other))))
   `(org-sexp-date ((,class (:inherit org-date))))
   `(org-special-keyword ((,class (:foreground ,cyan-alt))))
   `(org-table ((,class (:foreground ,fg-special-cold))))
   `(org-tag ((,class (:foreground ,fg-inactive :weight bold))))
   `(org-tag-group ((,class (:inherit org-tag))))
   `(org-target ((,class (:underline t))))
   `(org-time-grid ((,class (:foreground ,yellow-alt-other))))
   `(org-todo ((,class (:foreground ,magenta-alt-other))))
   `(org-upcoming-deadline ((,class (:foreground ,red-alt-other))))
   `(org-verbatim ((,class (:background ,bg-alt :foreground ,fg-special-calm))))
   `(org-verse ((,class (:inherit org-quote))))
   `(org-warning ((,class (:foreground ,blue-intense))))
   ;;;; org-journal
   `(org-journal-calendar-entry-face ((,class (:foreground ,yellow-alt-other :slant ,modus-theme-slant))))
   `(org-journal-calendar-scheduled-face ((,class (:foreground ,red-alt-other :slant ,modus-theme-slant))))
   `(org-journal-highlight ((,class (:foreground ,magenta-alt))))
   ;;;; org-noter
   `(org-noter-no-notes-exist-face ((,class (:foreground ,red-active :weight bold))))
   `(org-noter-notes-exist-face ((,class (:foreground ,green-active :weight bold))))
   ;;;; org-recur
   `(org-recur ((,class (:foreground ,magenta-active))))
   ;;;; outline-mode
   `(outline-1 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-main :weight bold
                                       ,@(when modus-operandi-theme-scale-headings
                                           (list :height modus-operandi-theme-scale-4))))))
   `(outline-2 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-special-warm :weight bold
                                       ,@(when modus-operandi-theme-scale-headings
                                          (list :height modus-operandi-theme-scale-3))))))
   `(outline-3 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-special-cold :weight bold
                                       ,@(when modus-operandi-theme-scale-headings
                                          (list :height modus-operandi-theme-scale-2))))))
   `(outline-4 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-special-mild :weight bold
                                       ,@(when modus-operandi-theme-scale-headings
                                          (list :height modus-operandi-theme-scale-1))))))
   `(outline-5 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-dim :weight bold))))
   `(outline-6 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,fg-alt :weight bold))))
   `(outline-7 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,cyan-active :weight bold))))
   `(outline-8 ((,class (:inherit ,modus-theme-variable-pitch
                           :foreground ,magenta-active :weight bold))))
   ;;;; package (M-x list-packages)
   `(package-description ((,class (:foreground ,fg-special-cold))))
   `(package-help-section-name ((,class (:foreground ,magenta-alt-other :weight bold))))
   `(package-name ((,class (:inherit link))))
   `(package-status-avail-obso ((,class (:foreground ,red :weight bold))))
   `(package-status-available ((,class (:foreground ,fg-special-mild))))
   `(package-status-built-in ((,class (:foreground ,magenta))))
   `(package-status-dependency ((,class (:foreground ,magenta-alt-other))))
   `(package-status-disabled ((,class (:inherit modus-theme-subtle-red))))
   `(package-status-external ((,class (:foreground ,cyan-alt-other))))
   `(package-status-held ((,class (:foreground ,yellow-alt))))
   `(package-status-incompat ((,class (:foreground ,yellow :weight bold))))
   `(package-status-installed ((,class (:foreground ,fg-special-warm))))
   `(package-status-new ((,class (:foreground ,green :weight bold))))
   `(package-status-unsigned ((,class (:foreground ,red-alt :weight bold))))
   ;;;; paren-face
   `(parenthesis ((,class (:foreground ,fg-alt))))
   ;;;; pass
   `(pass-mode-directory-face ((,class (:foreground ,fg-special-cold :weight bold))))
   `(pass-mode-entry-face ((,class (:background ,bg-main :foreground ,fg-main))))
   `(pass-mode-header-face ((,class (:foreground ,fg-special-warm))))
   ;;;; persp-mode
   `(persp-face-lighter-buffer-not-in-persp ((,class (:inherit modus-theme-intense-red))))
   `(persp-face-lighter-default ((,class (:foreground ,blue-active :weight bold))))
   `(persp-face-lighter-nil-persp ((,class (:foreground ,fg-active :weight bold))))
   ;;;; perspective
   `(persp-selected-face ((,class (:foreground ,blue-active :weight bold))))
   ;;;; powerline
   `(powerline-active0 ((,class (:background ,fg-inactive :foreground ,bg-inactive))))
   `(powerline-active1 ((,class (:background ,bg-active :foreground ,fg-active))))
   `(powerline-active2 ((,class (:background ,bg-alt :foreground ,fg-active))))
   `(powerline-inactive0 ((,class (:background ,bg-active :foreground ,fg-inactive))))
   `(powerline-inactive1 ((,class (:background ,bg-inactive :foreground ,fg-inactive))))
   `(powerline-inactive2 ((,class (:background ,bg-main :foreground ,fg-alt))))
   ;;;; powerline-evil
   `(powerline-evil-base-face ((,class (:background ,fg-main :foreground ,bg-main))))
   `(powerline-evil-emacs-face ((,class (:inherit modus-theme-active-magenta))))
   `(powerline-evil-insert-face ((,class (:inherit modus-theme-active-green))))
   `(powerline-evil-motion-face ((,class (:inherit modus-theme-active-blue))))
   `(powerline-evil-normal-face ((,class (:background ,fg-alt :foreground ,bg-main))))
   `(powerline-evil-operator-face ((,class (:inherit modus-theme-active-yellow))))
   `(powerline-evil-replace-face ((,class (:inherit modus-theme-active-red))))
   `(powerline-evil-visual-face ((,class (:inherit modus-theme-active-cyan))))
   ;;;; proced
   `(proced-mark ((,class (:foreground ,magenta-alt :weight bold))))
   `(proced-marked ((,class (:inherit modus-theme-intense-magenta))))
   `(proced-sort-header ((,class (:foreground ,fg-special-calm :weight bold :underline t))))
   ;;;; prodigy
   `(prodigy-green-face ((,class (:foreground ,green))))
   `(prodigy-red-face ((,class (:foreground ,red))))
   `(prodigy-yellow-face ((,class (:foreground ,yellow))))
   ;;;; rainbow-blocks
   `(rainbow-blocks-depth-1-face ((,class (:foreground ,magenta-alt-other))))
   `(rainbow-blocks-depth-2-face ((,class (:foreground ,blue))))
   `(rainbow-blocks-depth-3-face ((,class (:foreground ,magenta-alt))))
   `(rainbow-blocks-depth-4-face ((,class (:foreground ,green))))
   `(rainbow-blocks-depth-5-face ((,class (:foreground ,magenta))))
   `(rainbow-blocks-depth-6-face ((,class (:foreground ,cyan))))
   `(rainbow-blocks-depth-7-face ((,class (:foreground ,yellow))))
   `(rainbow-blocks-depth-8-face ((,class (:foreground ,cyan-alt))))
   `(rainbow-blocks-depth-9-face ((,class (:foreground ,red-alt))))
   `(rainbow-blocks-unmatched-face ((,class (:foreground ,red))))
   ;;;; rainbow-delimiters
   `(rainbow-delimiters-base-face-error ((,class (:foreground ,red))))
   `(rainbow-delimiters-base-face ((,class (:foreground ,fg-main))))
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,green-alt-other))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,magenta-alt-other))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,cyan-alt-other))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,yellow-alt-other))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,blue-alt-other))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,green-alt))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,magenta-alt))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,cyan-alt))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,yellow-alt))))
   `(rainbow-delimiters-mismatched-face ((,class (:foreground ,red-alt :weight bold))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red :weight bold))))
   ;;;; regexp-builder (re-builder)
   `(reb-match-0 ((,class (:inherit modus-theme-intense-blue))))
   `(reb-match-1 ((,class (:inherit modus-theme-intense-magenta))))
   `(reb-match-2 ((,class (:inherit modus-theme-intense-green))))
   `(reb-match-3 ((,class (:inherit modus-theme-intense-red))))
   `(reb-regexp-grouping-backslash ((,class (:foreground ,green :weight bold))))
   `(reb-regexp-grouping-construct ((,class (:foreground ,magenta :weight bold))))
   ;;;; ruler-mode
   `(ruler-mode-column-number ((,class (:inherit ruler-mode-default :foreground ,fg-main :weight bold))))
   `(ruler-mode-comment-column ((,class (:inherit ruler-mode-default :foreground ,red-active))))
   `(ruler-mode-current-column ((,class (:inherit ruler-mode-default :foreground ,cyan-active :box t))))
   `(ruler-mode-default ((,class (:background ,bg-inactive :foreground ,fg-inactive))))
   `(ruler-mode-fill-column ((,class (:inherit ruler-mode-default :foreground ,green-active))))
   `(ruler-mode-fringes ((,class (:inherit ruler-mode-default :foreground ,blue-active))))
   `(ruler-mode-goal-column ((,class (:inherit ruler-mode-default :foreground ,magenta-active))))
   `(ruler-mode-margins ((,class (:inherit ruler-mode-default :foreground ,bg-main))))
   `(ruler-mode-pad ((,class (:background ,bg-active :foreground ,fg-inactive))))
   `(ruler-mode-tab-stop ((,class (:inherit ruler-mode-default :foreground ,yellow-active))))
   ;;;; shell scripts
   `(sh-heredoc ((,class (:foreground ,blue-alt))))
   `(sh-quoted-exec ((,class (:foreground ,magenta-alt :weight ,modus-theme-bold))))
   ;;;; show-paren-mode
   `(show-paren-match ((,class (:background ,bg-paren-match :foreground ,fg-paren-match :weight bold))))
   `(show-paren-match-expression ((,class (:inherit modus-theme-special-calm))))
   `(show-paren-mismatch ((,class (:inherit modus-theme-intense-red))))
   ;;;; smart-mode-line
   `(sml/charging ((,class (:foreground ,green-active))))
   `(sml/discharging ((,class (:foreground ,red-active))))
   `(sml/filename ((,class (:foreground ,blue-active :weight bold))))
   `(sml/folder ((,class (:foreground ,fg-active))))
   `(sml/git ((,class (:foreground ,green-active :weight bold))))
   `(sml/global ((,class (:foreground ,fg-active))))
   `(sml/line-number ((,class (:inherit sml/global))))
   `(sml/minor-modes ((,class (:inherit sml/global))))
   `(sml/modes ((,class (:foreground ,fg-active :weight bold))))
   `(sml/modified ((,class (:foreground ,magenta-active :weight bold))))
   `(sml/mule-info ((,class (:inherit sml/global))))
   `(sml/name-filling ((,class (:foreground ,yellow-active))))
   `(sml/not-modified ((,class (:inherit sml/global))))
   `(sml/numbers-separator ((,class (:inherit sml/global))))
   `(sml/outside-modified ((,class (:inherit modus-theme-intense-red))))
   `(sml/position-percentage ((,class (:inherit sml/global))))
   `(sml/prefix ((,class (:foreground ,green-active))))
   `(sml/process ((,class (:inherit sml/prefix))))
   `(sml/projectile ((,class (:inherit sml/git))))
   `(sml/read-only ((,class (:foreground ,cyan-active :weight bold))))
   `(sml/remote ((,class (:inherit sml/global))))
   `(sml/sudo ((,class (:inherit modus-theme-subtle-red))))
   `(sml/time ((,class (:inherit sml/global))))
   `(sml/vc ((,class (:inherit sml/git))))
   `(sml/vc-edited ((,class (:foreground ,yellow-active :weight bold))))
   ;;;; smartparens
   `(sp-pair-overlay-face ((,class (:inherit modus-theme-special-warm))))
   `(sp-show-pair-enclosing ((,class (:inherit modus-theme-special-mild))))
   `(sp-show-pair-match-face ((,class (:background ,bg-paren-match :foreground ,fg-paren-match :weight bold))))
   `(sp-show-pair-mismatch-face ((,class (:inherit modus-theme-intense-red))))
   `(sp-wrap-overlay-closing-pair ((,class (:inherit sp-pair-overlay-face :weight bold))))
   `(sp-wrap-overlay-face ((,class (:inherit sp-pair-overlay-face))))
   `(sp-wrap-overlay-opening-pair ((,class (:inherit sp-pair-overlay-face :weight bold))))
   `(sp-wrap-tag-overlay-face ((,class (:inherit sp-pair-overlay-face))))
   ;;;; smerge
   `(smerge-base ((,class (:inherit modus-theme-special-warm))))
   `(smerge-lower ((,class (:inherit modus-theme-subtle-green))))
   `(smerge-markers ((,class (:inherit modus-theme-special-cold))))
   `(smerge-refined-added ((,class (:inherit modus-theme-intense-green))))
   `(smerge-refined-changed ((,class (:inherit modus-theme-intense-yellow))))
   `(smerge-refined-removed ((,class (:inherit modus-theme-intense-red))))
   `(smerge-refined-upper ((,class (:inherit modus-theme-intense-red))))
   `(smerge-upper ((,class (:inherit modus-theme-subtle-red))))
   ;;;; speedbar
   `(speedbar-button-face ((,class (:inherit link))))
   `(speedbar-directory-face ((,class (:foreground ,blue :weight bold))))
   `(speedbar-file-face ((,class (:foreground ,fg-main))))
   `(speedbar-highlight-face ((,class (:inherit modus-theme-subtle-blue))))
   `(speedbar-selected-face ((,class (:foreground ,cyan :weight bold))))
   `(speedbar-separator-face ((,class (:inherit modus-theme-intense-neutral))))
   `(speedbar-tag-face ((,class (:foreground ,yellow-alt-other))))
   ;;;; success
   `(suggest-heading ((,class (:foreground ,yellow-alt-other :weight bold))))
   ;;;; swiper
   `(swiper-background-match-face-1 ((,class (:inherit modus-theme-subtle-neutral))))
   `(swiper-background-match-face-2 ((,class (:inherit modus-theme-subtle-cyan))))
   `(swiper-background-match-face-3 ((,class (:inherit modus-theme-subtle-magenta))))
   `(swiper-background-match-face-4 ((,class (:inherit modus-theme-subtle-green))))
   `(swiper-line-face ((,class (,@(and (>= emacs-major-version 27) '(:extend t))
                                :inherit modus-theme-special-cold))))
   `(swiper-match-face-1 ((,class (:inherit swiper-line-face))))
   `(swiper-match-face-2 ((,class (:inherit swiper-line-face))))
   `(swiper-match-face-3 ((,class (:inherit swiper-line-face))))
   `(swiper-match-face-4 ((,class (:inherit swiper-line-face))))
   ;;;; sx
   `(sx-inbox-item-type ((,class (:foreground ,magenta-alt-other))))
   `(sx-inbox-item-type-unread ((,class (:inherit sx-inbox-item-type :weight bold))))
   `(sx-question-list-answers ((,class (:foreground ,green))))
   `(sx-question-list-answers-accepted ((,class (:box t :foreground ,green))))
   `(sx-question-list-bounty ((,class (:background ,bg-alt :foreground ,yellow :weight bold))))
   `(sx-question-list-date ((,class (:foreground ,fg-special-cold))))
   `(sx-question-list-favorite ((,class (:foreground ,fg-special-warm :weight bold))))
   `(sx-question-list-parent ((,class (:foreground ,fg-main))))
   `(sx-question-list-read-question ((,class (:foreground ,fg-alt))))
   `(sx-question-list-score ((,class (:foreground ,fg-special-mild))))
   `(sx-question-list-score-upvoted ((,class (:inherit sx-question-list-score :weight bold))))
   `(sx-question-list-unread-question ((,class (:foreground ,fg-main :weight bold))))
   `(sx-question-mode-accepted ((,class (:height 1.3 :foreground ,green :weight bold))))
   `(sx-question-mode-closed ((,class (:box (:line-width 2 :color nil) :inherit modus-theme-active-yellow))))
   `(sx-question-mode-closed-reason ((,class (:box (:line-width 2 :color nil) :foreground ,fg-main))))
   `(sx-question-mode-content-face ((,class (:background ,bg-dim))))
   `(sx-question-mode-date ((,class (:foreground ,blue))))
   `(sx-question-mode-header ((,class (:foreground ,cyan :weight bold))))
   `(sx-question-mode-kbd-tag ((,class (:height 0.9 :box (:line-width 3 :color ,fg-main :style released-button) :foreground ,fg-main :weight bold))))
   `(sx-question-mode-score ((,class (:foreground ,fg-dim))))
   `(sx-question-mode-score-downvoted ((,class (:foreground ,yellow))))
   `(sx-question-mode-score-upvoted ((,class (:foreground ,magenta :weight bold))))
   `(sx-question-mode-title ((,class (:foreground ,fg-main :weight bold))))
   `(sx-question-mode-title-comments ((,class (:foreground ,fg-alt :weight bold))))
   `(sx-tag ((,class (:foreground ,magenta-alt))))
   `(sx-user-name ((,class (:foreground ,blue-alt))))
   `(sx-user-reputation ((,class (:foreground ,fg-alt))))
   ;;;; telephone-line
   `(telephone-line-accent-active ((,class (:background ,fg-inactive :foreground ,bg-inactive))))
   `(telephone-line-accent-inactive ((,class (:background ,bg-active :foreground ,fg-active))))
   `(telephone-line-error ((,class (:foreground ,red-active :weight bold))))
   `(telephone-line-evil ((,class (:foreground ,fg-main))))
   `(telephone-line-evil-emacs ((,class (:inherit telephone-line-evil :background ,magenta-intense-bg))))
   `(telephone-line-evil-insert ((,class (:inherit telephone-line-evil :background ,green-intense-bg))))
   `(telephone-line-evil-motion ((,class (:inherit telephone-line-evil :background ,yellow-intense-bg))))
   `(telephone-line-evil-normal ((,class (:inherit telephone-line-evil :background ,bg-alt))))
   `(telephone-line-evil-operator ((,class (:inherit telephone-line-evil :background ,yellow-subtle-bg))))
   `(telephone-line-evil-replace ((,class (:inherit telephone-line-evil :background ,red-intense-bg))))
   `(telephone-line-evil-visual ((,class (:inherit telephone-line-evil :background ,cyan-intense-bg))))
   `(telephone-line-projectile ((,class (:foreground ,cyan-active))))
   `(telephone-line-unimportant ((,class (:foreground ,fg-inactive))))
   `(telephone-line-warning ((,class (:foreground ,yellow-active :weight bold))))
   ;;;; term
   `(term ((,class (:background ,bg-main :foreground ,fg-main))))
   `(term-bold ((,class (:weight bold))))
   `(term-color-blue ((,class (:background ,blue :foreground ,blue))))
   `(term-color-cyan ((,class (:background ,cyan :foreground ,cyan))))
   `(term-color-green ((,class (:background ,green :foreground ,green))))
   `(term-color-magenta ((,class (:background ,magenta :foreground ,magenta))))
   `(term-color-red ((,class (:background ,red :foreground ,red))))
   `(term-color-yellow ((,class (:background ,yellow :foreground ,yellow))))
   `(term-underline ((,class (:underline t))))
   ;;;; transient
   `(transient-active-infix ((,class (:inherit modus-theme-special-mild))))
   `(transient-argument ((,class (:foreground ,green :weight bold))))
   `(transient-disabled-suffix ((,class (:inherit modus-theme-intense-red))))
   `(transient-enabled-suffix ((,class (:inherit modus-theme-intense-green))))
   `(transient-heading ((,class (:foreground ,fg-special-warm :weight bold))))
   `(transient-inactive-argument ((,class (:foreground ,fg-alt))))
   `(transient-inactive-value ((,class (:foreground ,fg-alt))))
   `(transient-key ((,class (:foreground ,magenta-intense))))
   `(transient-mismatched-key ((,class (:underline t))))
   `(transient-nonstandard-key ((,class (:underline t))))
   `(transient-unreachable ((,class (:foreground ,fg-inactive))))
   `(transient-unreachable-key ((,class (:foreground ,fg-inactive))))
   `(transient-value ((,class (:foreground ,blue))))
   ;;;; treemacs
   `(treemacs-directory-collapsed-face ((,class (:foreground ,magenta-alt))))
   `(treemacs-directory-face ((,class (:inherit dired-directory))))
   `(treemacs-file-face ((,class (:foreground ,fg-main))))
   `(treemacs-fringe-indicator-face ((,class (:foreground ,fg-main))))
   `(treemacs-git-added-face ((,class (:foreground ,green-intense))))
   `(treemacs-git-conflict-face ((,class (:inherit modus-theme-intense-red :weight bold))))
   `(treemacs-git-ignored-face ((,class (:foreground ,fg-alt))))
   `(treemacs-git-modified-face ((,class (:foreground ,yellow-alt-other))))
   `(treemacs-git-renamed-face ((,class (:foreground ,cyan-alt-other))))
   `(treemacs-git-unmodified-face ((,class (:foreground ,fg-main))))
   `(treemacs-git-untracked-face ((,class (:foreground ,red-alt-other))))
   `(treemacs-help-column-face ((,class (:foreground ,magenta-alt-other :weight ,modus-theme-bold :underline t))))
   `(treemacs-help-title-face ((,class (:foreground ,blue-alt-other))))
   `(treemacs-on-failure-pulse-face ((,class (:inherit modus-theme-intense-red))))
   `(treemacs-on-success-pulse-face ((,class (:inherit modus-theme-intense-green))))
   `(treemacs-root-face ((,class (:foreground ,blue-alt-other :height 1.2 :weight bold :underline t))))
   `(treemacs-root-remote-disconnected-face ((,class (:inherit treemacs-root-remote-face :foreground ,yellow))))
   `(treemacs-root-remote-face ((,class (:inherit treemacs-root-face :foreground ,magenta))))
   `(treemacs-root-remote-unreadable-face ((,class (:inherit treemacs-root-unreadable-face))))
   `(treemacs-root-unreadable-face ((,class (:inherit treemacs-root-face :strike-through t))))
   `(treemacs-tags-face ((,class (:foreground ,blue-alt))))
   `(treemacs-tags-face ((,class (:foreground ,magenta-alt))))
   ;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,fg-main :weight bold))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,blue-intense))))
   `(undo-tree-visualizer-default-face ((,class (:foreground ,fg-alt))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,magenta-intense))))
   `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,green-intense))))
   ;;;; vc
   `(vc-conflict-state ((,class (:foreground ,red-active :weight bold))))
   `(vc-edited-state ((,class (:foreground ,fg-special-warm))))
   `(vc-locally-added-state ((,class (:foreground ,cyan-active))))
   `(vc-locked-state ((,class (:foreground ,magenta-active :weight bold))))
   `(vc-missing-state ((,class (:foreground ,yellow-active :weight bold))))
   `(vc-needs-update-state ((,class (:foreground ,fg-special-mild :weight bold))))
   `(vc-removed-state ((,class (:foreground ,red-active))))
   `(vc-state-base ((,class (:foreground ,fg-active))))
   `(vc-up-to-date-state ((,class (:foreground ,fg-special-cold))))
   ;;;; visual-regexp
   `(vr/group-0 ((,class (:inherit modus-theme-intense-blue))))
   `(vr/group-1 ((,class (:inherit modus-theme-intense-magenta))))
   `(vr/group-2 ((,class (:inherit modus-theme-intense-green))))
   `(vr/match-0 ((,class (:inherit modus-theme-refine-yellow))))
   `(vr/match-1 ((,class (:inherit modus-theme-refine-yellow))))
   `(vr/match-separator-face ((,class (:inherit modus-theme-intense-neutral :weight bold))))
   ;;;; wgrep
   `(wgrep-delete-face ((,class (:inherit modus-theme-refine-yellow))))
   `(wgrep-done-face ((,class (:inherit modus-theme-refine-blue))))
   `(wgrep-face ((,class (:inherit modus-theme-refine-green))))
   `(wgrep-file-face ((,class (:foreground ,fg-special-warm))))
   `(wgrep-reject-face ((,class (:inherit modus-theme-intense-red :weight bold))))
   ;;;; which-function-mode
   `(which-func ((,class (:foreground ,magenta-active))))
   ;;;; which-key
   `(which-key-command-description-face ((,class (:foreground ,cyan))))
   `(which-key-group-description-face ((,class (:foreground ,magenta-alt))))
   `(which-key-highlighted-command-face ((,class (:foreground ,cyan-alt :underline t))))
   `(which-key-key-face ((,class (:foreground ,blue-intense :weight bold))))
   `(which-key-local-map-description-face ((,class (:foreground ,fg-main))))
   `(which-key-note-face ((,class (:background ,bg-dim :foreground ,fg-special-mild))))
   `(which-key-separator-face ((,class (:foreground ,fg-alt))))
   `(which-key-special-key-face ((,class (:foreground ,yellow-intense :weight bold))))
   ;;;; whitespace-mode
   `(whitespace-big-indent ((,class (:inherit modus-theme-subtle-red))))
   `(whitespace-empty ((,class (:inherit modus-theme-intense-magenta))))
   `(whitespace-hspace ((,class (:background ,bg-whitespace :foreground ,fg-whitespace))))
   `(whitespace-indentation ((,class (:background ,bg-whitespace :foreground ,fg-whitespace))))
   `(whitespace-line ((,class (:inherit modus-theme-special-warm))))
   `(whitespace-newline ((,class (:background ,bg-whitespace :foreground ,fg-whitespace))))
   `(whitespace-space ((,class (:background ,bg-whitespace :foreground ,fg-whitespace))))
   `(whitespace-space-after-tab ((,class (:inherit modus-theme-subtle-magenta))))
   `(whitespace-space-before-tab ((,class (:inherit modus-theme-subtle-cyan))))
   `(whitespace-tab ((,class (:background ,bg-whitespace :foreground ,fg-whitespace))))
   `(whitespace-trailing ((,class (:inherit modus-theme-intense-red))))
   ;;;; writegood-mode
   `(writegood-duplicates-face ((,class (:background ,bg-alt :foreground ,red-alt-other :underline t))))
   `(writegood-passive-voice-face ((,class (:background ,bg-alt :foreground ,cyan-alt-other :underline t))))
   `(writegood-weasels-face ((,class (:background ,bg-alt :foreground ,yellow-alt-other :underline t))))
   ;;;; xah-elisp-mode
   `(xah-elisp-at-symbol ((,class (:foreground ,red-alt :weight bold))))
   `(xah-elisp-cap-variable ((,class (:foreground ,red-alt-other))))
   `(xah-elisp-command-face ((,class (:foreground ,cyan-alt-other))))
   `(xah-elisp-dollar-symbol ((,class (:foreground ,green))))
   ;;;; ztree
   `(ztreep-arrow-face ((,class (:foreground ,fg-inactive))))
   `(ztreep-diff-header-face ((,class (:height 1.2 :foreground ,fg-special-cold :weight bold))))
   `(ztreep-diff-header-small-face ((,class (:foreground ,fg-special-mild :weight bold))))
   `(ztreep-diff-model-add-face ((,class (:foreground ,green))))
   `(ztreep-diff-model-diff-face ((,class (:foreground ,red))))
   `(ztreep-diff-model-ignored-face ((,class (:foreground ,fg-alt :strike-through t))))
   `(ztreep-diff-model-normal-face ((,class (:foreground ,fg-alt))))
   `(ztreep-expand-sign-face ((,class (:foreground ,blue))))
   `(ztreep-header-face ((,class (:height 1.2 :foreground ,fg-special-cold :weight bold))))
   `(ztreep-leaf-face ((,class (:foreground ,cyan))))
   `(ztreep-node-count-children-face ((,class (:foreground ,fg-special-warm))))
   `(ztreep-node-face ((,class (:foreground ,fg-main))))
   ;;; Theme Variables
   (custom-theme-set-variables
    'modus-operandi
    ;;;; ansi-colors
    `(ansi-color-faces-vector [default bold shadow italic underline success warning error])
    `(ansi-color-names-vector [,fg-main ,red ,green ,yellow ,blue ,magenta ,cyan ,bg-main])
    ;;;; ibuffer
    `(ibuffer-deletion-face 'dired-flagged)
    `(ibuffer-marked-face 'dired-marked)
    ;;;; xterm-color
    `(xterm-color-names [,fg-main ,red ,green ,yellow ,blue ,magenta ,cyan ,bg-alt])
    `(xterm-color-names-bright [,fg-alt ,red-alt ,green-alt ,yellow-alt ,blue-alt ,magenta-alt ,cyan-alt ,bg-main]))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
    (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'modus-operandi)

(provide 'modus-operandi-theme)
;;; modus-operandi-theme.el ends here
