;;; nordic-night-theme.el --- A darker, more colorful version of the lovely Nord theme -*- lexical-binding: t -*-

;; Copyright (c) 2023 Ashton Wiersdorf

;; Title: Nordic-Night Theme
;; Author: Ashton Wiersdorf <mail@wiersdorf.dev>
;; Created: 2023
;; Version: 2.0.2
;; Package-Requires: ((emacs "24.1"))
;; SPDX-License-Identifier: MIT
;; Homepage: https://sr.ht/~ashton314/nordic-night/

;;; Commentary:

;; Nordic-Night is a modification of the lovely Nord theme. It features a much
;; darker background and more liberal use of the color palliate to make your
;; buffer both more colorful as well as easier to read, whilst still retaining
;; the calm, harmonious tones of the Nord theme.

;;; Code:

(unless (>= emacs-major-version 25)
  (error "Nordic-Night theme requires Emacs 25.1 or later!"))

(deftheme nordic-night "A darker, more colorful version of the lovely Nord theme.")

(defgroup nordic-night nil
  "Nordic-Night theme customizations.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defun nordic-night--fullcolorp ()
  "Return whether the display can display nordic-night colors."
  (or (>= (display-color-cells) 16777216) (display-graphic-p)))

(defvar nordic-night-nord-colors
  '(:nord00 "#2e3440"
    :nord01 "#3b4252"
    :nord02 "#434c5e"
    :nord03 "#4c566a"
    :nord04 "#d8dee9"
    :nord05 "#e5e9f0"
    :nord06 "#eceff4"
    :nord07 "#8fbcbb"
    :nord08 "#88c0d0"
    :nord09 "#81a1c1"
    :nord10 "#5e81ac"
    :nord11 "#bf616a"
    :nord12 "#d08770"
    :nord13 "#ebcb8b"
    :nord14 "#a3be8c"
    :nord15 "#b48ead")
  "Nord colors for full-color displays")

(defvar nordic-night-auroa-bg
  '(:nord07b "#3f4f4f"
    :nord08b "#3d5056"
    :nord09b "#3b4551"
    :nord10b "#233949"
    :nord11b "#512e31"
    :nord12b "#573d35"
    :nord13b "#61553d"
    :nord14b "#46503e"
    :nord15b "#4c3e4a")
  "Nord colors tuned for background")

(defvar nordic-night-colors
  '(:nn00 "#121212"
    :nn01 "#181818"
    :nn02 "#202024"
    :nn03 "#6B7386"
    :nn04 "#8892A4"
    :nn05 "#B5BDCC")
  "Nordic night colors for full-color displays")

;; shell gray cells are 00, 08, 12, 1c 26, 30, 3a, 44, 4e, 58, 62, 6c, 76, 80,
;; 8a, 94, 9e, a8, b2, bc, c6, d0, da, e4, ee
;;
;; shell color cells are 00, 5f, 87, af, d7, ff
(defvar nordic-night-nord-colors-shell
  '(:nord00 "#303030"
    :nord01 "#3a3a3a"
    :nord02 "#444444"
    :nord03 "#4e4e4e"
    :nord04 "#d0d0d0"
    :nord05 "#e4e4e4"
    :nord06 "#eeeeee"
    :nord07 "#008787"
    :nord08 "#00afd7"
    :nord09 "#00afff"
    :nord10 "#0087af"
    :nord11 "#df005f"
    :nord12 "#d75f00"
    :nord13 "#d7af00"
    :nord14 "#87af5f"
    :nord15 "#af87af")
  "Nord colors for limited-color displays")

(defvar nordic-night-auroa-bg-shell
  '(:nord07b "#3a4e4e"
    :nord08b "#3a4e58"
    :nord09b "#3a444e"
    :nord10b "#263a4e"
    :nord11b "#582630"
    :nord12b "#583a30"
    :nord13b "#62583a"
    :nord14b "#444e3a"
    :nord15b "#4e3a4e")
  "Nord colors for limited-color displays tuned for background")

(defvar nordic-night-colors-shell
  '(:nn00 "#121212"
    :nn01 "#1c1c1c"
    :nn02 "#262626"
    :nn03 "#6c6c6c"
    :nn04 "#8a8a8a"
    :nn05 "#c6c6c6")
  "Nordic night colors for limited-color displays")

(defun nordic-night--build-theme (theme-name color-set)
  (let ((class '((class color) (min-colors 89)))
        (nn-nearblack    (plist-get color-set :nn00))
        (nn-brightblack1 (plist-get color-set :nn01))
        (nn-brightblack2 (plist-get color-set :nn02))
        (nn-dark0        (plist-get color-set :nord00))
        (nn-dark1        (plist-get color-set :nord01))
        (nn-dark2        (plist-get color-set :nord02))
        (nn-dark3        (plist-get color-set :nord03))
        (nn-lessdark3-1  (plist-get color-set :nn03))
        (nn-lessdark3-2  (plist-get color-set :nn04))
        (nn-lessdark3-3  (plist-get color-set :nn05))
        (nn-snowy4       (plist-get color-set :nord04))
        (nn-snowy5       (plist-get color-set :nord05))
        (nn-snowy6       (plist-get color-set :nord06))
        (nn-bluegreen7   (plist-get color-set :nord07))
        (nn-brightblue8  (plist-get color-set :nord08))
        (nn-lapis9       (plist-get color-set :nord09))
        (nn-blue10       (plist-get color-set :nord10))
        (nn-red11        (plist-get color-set :nord11))
        (nn-orange12     (plist-get color-set :nord12))
        (nn-yellow13     (plist-get color-set :nord13))
        (nn-green14      (plist-get color-set :nord14))
        (nn-purple15     (plist-get color-set :nord15))
        (nn-bg07         (plist-get color-set :nord07b))
        (nn-bg08         (plist-get color-set :nord08b))
        (nn-bg09         (plist-get color-set :nord09b))
        (nn-bg10         (plist-get color-set :nord10b))
        (nn-bg11         (plist-get color-set :nord11b))
        (nn-bg12         (plist-get color-set :nord12b))
        (nn-bg13         (plist-get color-set :nord13b))
        (nn-bg14         (plist-get color-set :nord14b))
        (nn-bg15         (plist-get color-set :nord15b))
        (nn-annotation   (plist-get color-set :nord12))
        (nn-attribute    (plist-get color-set :nord07))
        (nn-class        (plist-get color-set :nord07))
        (nn-comment      (plist-get color-set :nn04))
        (nn-doc          (plist-get color-set :nord14))
        (nn-method       (plist-get color-set :nord08))
        (nn-keyword      (plist-get color-set :nord09))
        (nn-numeric      (plist-get color-set :nord15))
        (nn-operator     (plist-get color-set :nord09))
        (nn-preprocessor (plist-get color-set :nord10))
        (nn-punctuation  (plist-get color-set :nord04))
        (nn-string       (plist-get color-set :nord14))
        (nn-tag          (plist-get color-set :nord09))
        (nn-variable     (plist-get color-set :nord04)))
    (custom-theme-set-faces
     theme-name
;;; -- Base
     `(bold ((,class (:weight bold))))
     `(bold-italic ((,class (:weight bold :slant italic))))
     `(default ((,class (:foreground ,nn-snowy4 :background ,nn-nearblack))))
     `(error ((,class (:foreground ,nn-red11 :weight bold))))
     `(escape-glyph ((,class (:foreground ,nn-orange12))))
     `(font-lock-builtin-face ((,class (:foreground ,nn-lapis9))))
     `(font-lock-comment-face ((,class (:foreground ,nn-comment :weight light))))
     `(font-lock-comment-delimiter-face ((,class (:foreground ,nn-comment))))
     `(font-lock-constant-face ((,class (:foreground ,nn-lapis9))))
     `(font-lock-doc-face ((,class (:foreground ,nn-doc))))
     `(font-lock-function-name-face ((,class (:foreground ,nn-brightblue8))))
     `(font-lock-keyword-face ((,class (:foreground ,nn-purple15))))
     `(font-lock-negation-char-face ((,class (:foreground ,nn-lapis9))))
     `(font-lock-preprocessor-face ((,class (:foreground ,nn-blue10 :weight bold))))
     `(font-lock-reference-face ((,class (:foreground ,nn-lapis9))))
     `(font-lock-regexp-grouping-backslash ((,class (:foreground ,nn-yellow13))))
     `(font-lock-regexp-grouping-construct ((,class (:foreground ,nn-yellow13))))
     `(font-lock-string-face ((,class (:foreground ,nn-green14))))
     `(font-lock-type-face ((,class (:foreground ,nn-green14))))
     `(font-lock-variable-name-face ((,class (:foreground ,nn-brightblue8))))
     `(font-lock-warning-face ((,class (:foreground ,nn-orange12))))
     `(italic ((,class (:slant italic :foreground ,nn-brightblue8))))
     `(shadow ((,class (:foreground ,nn-dark3))))
     `(line-number-current-line ((,class (:foreground ,nn-snowy4))))
     `(underline ((,class (:underline t))))
     `(warning ((,class (:foreground ,nn-yellow13 :weight bold))))

;;; -- Syntax
;;; --- C
     `(c-annotation-face ((,class (:foreground ,nn-annotation))))

;;; --- diff
     `(diff-added ((,class (:foreground ,nn-green14))))
     `(diff-changed ((,class (:foreground ,nn-yellow13))))
     `(diff-context ((,class (:inherit default))))
     `(diff-file-header ((,class (:foreground ,nn-brightblue8))))
     `(diff-function ((,class (:foreground ,nn-bluegreen7))))
     `(diff-header ((,class (:foreground ,nn-lapis9 :weight bold))))
     `(diff-hunk-header ((,class (:foreground ,nn-lapis9 :background ,nn-dark0))))
     `(diff-indicator-added ((,class (:foreground ,nn-green14))))
     `(diff-indicator-changed ((,class (:foreground ,nn-yellow13))))
     `(diff-indicator-removed ((,class (:foreground ,nn-red11))))
     `(diff-nonexistent ((,class (:foreground ,nn-red11))))
     `(diff-refine-added ((,class (:foreground ,nn-green14))))
     `(diff-refine-changed ((,class (:foreground ,nn-yellow13))))
     `(diff-refine-removed ((,class (:foreground ,nn-red11))))
     `(diff-removed ((,class (:foreground ,nn-red11))))

;;; -- UI
     `(border ((,class (:foreground ,nn-snowy4))))
     `(buffer-menu-buffer ((,class (:foreground ,nn-snowy4 :weight bold))))
     `(button ((,class (:background ,nn-dark0 :foreground ,nn-brightblue8 :box (:line-width 2 :color ,nn-snowy4 :style sunken-button)))))
     `(completions-annotations ((,class (:foreground ,nn-lapis9))))
     `(completions-common-part ((,class (:foreground ,nn-brightblue8 :weight bold))))
     `(completions-first-difference ((,class (:foreground ,nn-red11))))
     `(custom-button ((,class (:background ,nn-dark0 :foreground ,nn-brightblue8 :box (:line-width 2 :color ,nn-snowy4 :style sunken-button)))))
     `(custom-button-mouse ((,class (:background ,nn-snowy4 :foreground ,nn-dark0 :box (:line-width 2 :color ,nn-snowy4 :style sunken-button)))))
     `(custom-button-pressed ((,class (:background ,nn-snowy6 :foreground ,nn-dark0 :box (:line-width 2 :color ,nn-snowy4 :style sunken-button)))))
     `(custom-button-pressed-unraised ((,class (:background ,nn-snowy4 :foreground ,nn-dark0 :box (:line-width 2 :color ,nn-snowy4 :style sunken-button)))))
     `(custom-button-unraised ((,class (:background ,nn-dark0 :foreground ,nn-brightblue8 :box (:line-width 2 :color ,nn-snowy4 :style sunken-button)))))
     `(custom-changed ((,class (:foreground ,nn-yellow13))))
     `(custom-comment ((,class (:foreground ,nn-comment))))
     `(custom-comment-tag ((,class (:foreground ,nn-bluegreen7))))
     `(custom-documentation ((,class (:foreground ,nn-snowy4))))
     `(custom-group-tag ((,class (:foreground ,nn-brightblue8 :weight bold))))
     `(custom-group-tag-1 ((,class (:foreground ,nn-brightblue8 :weight bold))))
     `(custom-invalid ((,class (:foreground ,nn-red11))))
     `(custom-modified ((,class (:foreground ,nn-yellow13))))
     `(custom-rogue ((,class (:foreground ,nn-orange12 :background ,nn-dark2))))
     `(custom-saved ((,class (:foreground ,nn-green14))))
     `(custom-set ((,class (:foreground ,nn-brightblue8))))
     `(custom-state ((,class (:foreground ,nn-green14))))
     `(custom-themed ((,class (:foreground ,nn-brightblue8 :background ,nn-dark2))))
     `(cursor ((,class (:background ,nn-snowy4))))
     `(fringe ((,class (:foreground ,nn-dark3 :background ,nn-nearblack))))
     `(file-name-shadow ((,class (:inherit shadow))))
     `(header-line ((,class (:foreground ,nn-snowy4 :background ,nn-dark2))))
     `(help-argument-name ((,class (:foreground ,nn-brightblue8))))
     `(highlight ((,class (:background ,nn-bg15))))
     ;; `(hi-yellow ((,class (:background ,nn-bg13))))
     ;; `(hi-green ((,class (:background ,nn-bg14))))
     ;; `(hi-blue ((,class (:background ,nn-bg10))))
     ;; `(hi-pink ((,class (:background ,nn-bg11))))
     ;; `(hi-salmon ((,class (:background ,nn-bg13))))
     ;; `(hi-aquamarine ((,class (:background ,nn-bg07))))
     `(hi-yellow ((,class (:background ,nn-yellow13))))
     `(hi-green ((,class (:background ,nn-green14 :foreground ,nn-dark0))))
     `(hi-blue ((,class (:background ,nn-blue10))))
     `(hi-pink ((,class (:background ,nn-red11 :foreground ,nn-dark0))))
     `(hi-salmon ((,class (:background ,nn-orange12 :foreground ,nn-dark0))))
     `(hi-aquamarine ((,class (:background ,nn-bluegreen7))))
     `(hl-line ((,class (:background ,nn-brightblack2))))
     `(info-menu-star ((,class (:foreground ,nn-lapis9))))
     `(isearch ((,class (:foreground ,nn-dark0 :background ,nn-brightblue8))))
     `(isearch-fail ((,class (:foreground ,nn-red11))))
     `(link ((,class (:underline t))))
     `(link-visited ((,class (:underline t))))
     `(linum ((,class (:foreground ,nn-dark3 :background ,nn-dark0))))
     `(linum-relative-current-face ((,class (:foreground ,nn-dark3 :background ,nn-dark0))))
     `(match ((,class (:inherit isearch))))
     `(message-cited-text ((,class (:foreground ,nn-green14))))
     `(message-cited-text-1 ((,class (:foreground ,nn-green14))))
     `(message-cited-text-2 ((,class (:foreground ,nn-brightblue8))))
     `(message-cited-text-3 ((,class (:foreground ,nn-purple15))))
     `(message-cited-text-4 ((,class (:foreground ,nn-red11))))
     `(message-header-cc ((,class (:foreground ,nn-lapis9))))
     `(message-header-name ((,class (:foreground ,nn-bluegreen7))))
     `(message-header-newsgroup ((,class (:foreground ,nn-green14))))
     `(message-header-other ((,class (:foreground ,nn-snowy4))))
     `(message-header-subject ((,class (:foreground ,nn-brightblue8))))
     `(message-header-to ((,class (:foreground ,nn-lapis9))))
     `(message-header-xheader ((,class (:foreground ,nn-yellow13))))
     `(message-mml ((,class (:foreground ,nn-blue10))))
     `(message-separator ((,class (:inherit shadow))))
     `(minibuffer-prompt ((,class (:foreground ,nn-brightblue8 :weight bold))))
     `(mm-command-output ((,class (:foreground ,nn-brightblue8))))
     `(mode-line ((,class (:foreground ,nn-snowy4 :background ,nn-dark3 :width normal))))
     `(mode-line-inactive ((,class (:foreground ,nn-blue10 :background ,nn-dark0 :width normal))))
     `(mode-line-buffer-id ((,class (:weight bold))))
     `(mode-line-highlight ((,class (:inherit highlight))))
     `(next-error ((,class (:inherit error))))
     `(nobreak-space ((,class (:foreground ,nn-dark3))))
     `(outline-1 ((,class (:foreground ,nn-brightblue8 :weight bold))))
     `(outline-2 ((,class (:inherit outline-1))))
     `(outline-3 ((,class (:inherit outline-1))))
     `(outline-4 ((,class (:inherit outline-1))))
     `(outline-5 ((,class (:inherit outline-1))))
     `(outline-6 ((,class (:inherit outline-1))))
     `(outline-7 ((,class (:inherit outline-1))))
     `(outline-8 ((,class (:inherit outline-1))))
     `(package-description ((,class (:foreground ,nn-snowy4))))
     `(package-help-section-name ((,class (:foreground ,nn-brightblue8 :weight bold))))
     `(package-name ((,class (:foreground ,nn-brightblue8))))
     `(package-status-available ((,class (:foreground ,nn-bluegreen7))))
     `(package-status-avail-obso ((,class (:foreground ,nn-bluegreen7 :slant italic))))
     `(package-status-built-in ((,class (:foreground ,nn-lapis9))))
     `(package-status-dependency ((,class (:foreground ,nn-brightblue8 :slant italic))))
     `(package-status-disabled ((,class (:foreground ,nn-dark3))))
     `(package-status-external ((,class (:foreground ,nn-orange12 :slant italic))))
     `(package-status-held ((,class (:foreground ,nn-snowy4 :weight bold))))
     `(package-status-new ((,class (:foreground ,nn-green14))))
     `(package-status-incompat ((,class (:foreground ,nn-red11))))
     `(package-status-installed ((,class (:foreground ,nn-bluegreen7 :weight bold))))
     `(package-status-unsigned ((,class (:underline ,nn-yellow13))))
     `(pulse-highlight-start-face ((,class (:background ,nn-bg13))))
     `(query-replace ((,class (:foreground ,nn-brightblue8 :background ,nn-dark2))))
     `(region ((,class (:extend t :background ,nn-bg08))))
     `(scroll-bar ((,class (:background ,nn-dark3))))
     `(secondary-selection ((,class (:background ,nn-dark2))))

     `(show-paren-match ((,class (:background ,nn-bg10))))
     `(show-paren-mismatch ((,class (:background ,nn-bg11))))
     `(success ((,class (:foreground ,nn-green14))))
     `(tab-bar ((,class (:background ,nn-dark0 :foreground ,nn-snowy4 :box (:line-width (1 . 2) :color ,nn-dark0)))))
     `(tab-bar-tab ((,class (:inherit tab-bar :background ,nn-dark3 :box (:color ,nn-dark3)))))
     `(tab-bar-tab-inactive ((,class (:foreground ,nn-blue10))))
     `(term ((,class (:inherit default))))
     `(term-color-black ((,class (:foreground ,nn-dark1 :background ,nn-dark1))))
     `(term-color-white ((,class (:foreground ,nn-snowy5 :background ,nn-snowy5))))
     `(term-color-cyan ((,class (:foreground ,nn-bluegreen7 :background ,nn-bluegreen7))))
     `(term-color-blue ((,class (:foreground ,nn-brightblue8 :background ,nn-brightblue8))))
     `(term-color-red ((,class (:foreground ,nn-red11 :background ,nn-red11))))
     `(term-color-yellow ((,class (:foreground ,nn-yellow13 :background ,nn-yellow13))))
     `(term-color-green ((,class (:foreground ,nn-green14 :background ,nn-green14))))
     `(term-color-magenta ((,class (:foreground ,nn-purple15 :background ,nn-purple15))))
     `(tool-bar ((,class (:foreground ,nn-snowy4 :background ,nn-dark3))))
     `(tooltip ((,class (:foreground ,nn-dark0 :background ,nn-snowy4))))
     `(trailing-whitespace ((,class (:underline (:color ,nn-yellow13 :style wave)))))
     `(tty-menu-disabled-face ((,class (:foreground ,nn-dark1))))
     `(tty-menu-enabled-face ((,class (:background ,nn-dark2 foreground ,nn-snowy4))))
     `(tty-menu-selected-face ((,class (:foreground ,nn-brightblue8 :underline t))))
     `(undo-tree-visualizer-current-face ((,class (:foreground ,nn-brightblue8))))
     `(undo-tree-visualizer-default-face ((,class (:foreground ,nn-snowy4))))
     `(undo-tree-visualizer-unmodified-face ((,class (:foreground ,nn-snowy4))))
     `(undo-tree-visualizer-register-face ((,class (:foreground ,nn-lapis9))))
     `(vc-conflict-state ((,class (:foreground ,nn-orange12))))
     `(vc-edited-state ((,class (:foreground ,nn-yellow13))))
     `(vc-locally-added-state ((,class (:underline ,nn-green14))))
     `(vc-locked-state ((,class (:foreground ,nn-blue10))))
     `(vc-missing-state ((,class (:foreground ,nn-red11))))
     `(vc-needs-update-state ((,class (:foreground ,nn-orange12))))
     `(vc-removed-state ((,class (:foreground ,nn-red11))))
     `(vc-state-base ((,class (:foreground ,nn-snowy4))))
     `(vc-up-to-date-state ((,class (:foreground ,nn-brightblue8))))
     `(vertical-border ((,class (:foreground ,nn-dark2))))
     `(which-func ((,class (:foreground ,nn-brightblue8))))
     `(whitespace-big-indent ((,class (:foreground ,nn-dark3 :background ,nn-dark0))))
     `(whitespace-empty ((,class (:foreground ,nn-dark3 :background ,nn-dark0))))
     `(whitespace-hspace ((,class (:foreground ,nn-dark3 :background ,nn-dark0))))
     `(whitespace-indentation ((,class (:foreground ,nn-dark3 :background ,nn-dark0))))
     `(whitespace-line ((,class (:background ,nn-dark0))))
     `(whitespace-newline ((,class (:foreground ,nn-dark3 :background ,nn-dark0))))
     `(whitespace-space ((,class (:foreground ,nn-dark3 :background ,nn-dark0))))
     `(whitespace-space-after-tab ((,class (:foreground ,nn-dark3 :background ,nn-dark0))))
     `(whitespace-space-before-tab ((,class (:foreground ,nn-dark3 :background ,nn-dark0))))
     `(whitespace-tab ((,class (:foreground ,nn-dark3 :background ,nn-dark0))))
     `(whitespace-trailing ((,class (:inherit trailing-whitespace))))
     `(widget-button-pressed ((,class (:foreground ,nn-lapis9 :background ,nn-dark1))))
     `(widget-documentation ((,class (:foreground ,nn-snowy4))))
     `(widget-field ((,class (:background ,nn-dark2 :foreground ,nn-snowy4))))
     `(widget-single-line-field ((,class (:background ,nn-dark2 :foreground ,nn-snowy4))))
     `(window-divider ((,class (:background ,nn-dark3))))
     `(window-divider-first-pixel ((,class (:background ,nn-dark3))))
     `(window-divider-last-pixel ((,class (:background ,nn-dark3))))

;;; - Package Support

;;; -- Syntax
;;; --- Auctex
     `(font-latex-bold-face ((,class (:inherit bold))))
     `(font-latex-italic-face ((,class (:inherit italic))))
     `(font-latex-math-face ((,class (:foreground ,nn-brightblue8))))
     `(font-latex-sectioning-0-face ((,class (:foreground ,nn-brightblue8 :weight bold))))
     `(font-latex-sectioning-1-face ((,class (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-2-face ((,class (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-3-face ((,class (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-4-face ((,class (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-5-face ((,class (:inherit font-latex-sectioning-0-face))))
     `(font-latex-script-char-face ((,class (:inherit font-lock-warning-face))))
     `(font-latex-string-face ((,class (:inherit font-lock-string-face))))
     `(font-latex-warning-face ((,class (:inherit font-lock-warning-face))))

;;; --- Elixir
     `(elixir-attribute-face ((,class (:foreground ,nn-annotation))))
     `(elixir-atom-face ((,class (:foreground ,nn-lapis9))))

;;; --- Enhanced Ruby
     `(enh-ruby-heredoc-delimiter-face ((,class (:foreground ,nn-green14))))
     `(enh-ruby-op-face ((,class (:foreground ,nn-lapis9))))
     `(enh-ruby-regexp-delimiter-face ((,class (:foreground ,nn-yellow13))))
     `(enh-ruby-regexp-face ((,class (:foreground ,nn-yellow13))))
     `(enh-ruby-string-delimiter-face ((,class (:foreground ,nn-green14))))
     `(erm-syn-errline ((,class (:foreground ,nn-red11 :underline t))))
     `(erm-syn-warnline ((,class (:foreground ,nn-yellow13 :underline t))))

;;; --- Java Development Environment for Emacs
     `(jdee-db-active-breakpoint-face ((,class (:background ,nn-dark2 :weight bold))))
     `(jdee-bug-breakpoint-cursor ((,class (:background ,nn-dark2))))
     `(jdee-db-requested-breakpoint-face ((,class (:foreground ,nn-yellow13 :background ,nn-dark2 :weight bold))))
     `(jdee-db-spec-breakpoint-face ((,class (:foreground ,nn-green14 :background ,nn-dark2 :weight bold))))
     `(jdee-font-lock-api-face ((,class (:foreground ,nn-snowy4))))
     `(jdee-font-lock-code-face ((,class (:slant italic))))
     `(jdee-font-lock-constant-face ((,class (:foreground ,nn-keyword))))
     `(jdee-font-lock-constructor-face ((,class (:foreground ,nn-method))))
     `(jdee-font-lock-doc-tag-face ((,class (:foreground ,nn-bluegreen7))))
     `(jdee-font-lock-link-face ((,class (:underline t))))
     `(jdee-font-lock-modifier-face ((,class (:foreground ,nn-keyword))))
     `(jdee-font-lock-number-face ((,class (:foreground ,nn-numeric))))
     `(jdee-font-lock-operator-fac ((,class (:foreground ,nn-operator))))
     `(jdee-font-lock-package-face ((,class (:foreground ,nn-class))))
     `(jdee-font-lock-pre-face ((,class (:foreground ,nn-comment :slant italic))))
     `(jdee-font-lock-private-face ((,class (:foreground ,nn-keyword))))
     `(jdee-font-lock-public-face ((,class (:foreground ,nn-keyword))))
     `(jdee-font-lock-variable-face ((,class (:foreground ,nn-variable))))

;;; --- JavaScript 2
     `(js2-function-call ((,class (:foreground ,nn-brightblue8))))
     `(js2-private-function-call ((,class (:foreground ,nn-brightblue8))))
     `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,nn-snowy6))))
     `(js2-jsdoc-html-tag-name ((,class (:foreground ,nn-lapis9))))
     `(js2-external-variable ((,class (:foreground ,nn-snowy4))))
     `(js2-function-param ((,class (:foreground ,nn-snowy4))))
     `(js2-jsdoc-value ((,class (:foreground ,nn-comment))))
     `(js2-jsdoc-tag ((,class (:foreground ,nn-bluegreen7))))
     `(js2-jsdoc-type ((,class (:foreground ,nn-bluegreen7))))
     `(js2-private-member ((,class (:foreground ,nn-snowy4))))
     `(js2-object-property ((,class (:foreground ,nn-snowy4))))
     `(js2-error ((,class (:foreground ,nn-red11))))
     `(js2-warning ((,class (:foreground ,nn-yellow13))))
     `(js2-instance-member ((,class (:foreground ,nn-snowy4))))

;;; --- JavaScript 3
     `(js3-error-face ((,class (:foreground ,nn-red11))))
     `(js3-external-variable-face ((,class (:foreground ,nn-snowy4))))
     `(js3-function-param-face ((,class (:foreground ,nn-snowy4))))
     `(js3-instance-member-face ((,class (:foreground ,nn-snowy4))))
     `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,nn-snowy6))))
     `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,nn-lapis9))))
     `(js3-jsdoc-tag-face ((,class (:foreground ,nn-lapis9))))
     `(js3-jsdoc-type-face ((,class (:foreground ,nn-bluegreen7))))
     `(js3-jsdoc-value-face ((,class (:foreground ,nn-snowy4))))
     `(js3-magic-paren-face ((,class (:inherit show-paren-match-face))))
     `(js3-private-function-call-face ((,class (:foreground ,nn-brightblue8))))
     `(js3-private-member-face ((,class (:foreground ,nn-snowy4))))
     `(js3-warning-face ((,class (:foreground ,nn-yellow13))))

;;; --- Markdown
     `(markdown-blockquote-face ((,class (:inherit org-quote))))
     `(markdown-bold-face ((,class (:inherit bold))))
     `(markdown-header-face-1 ((,class (:inherit org-level-1))))
     `(markdown-header-face-2 ((,class (:inherit org-level-2))))
     `(markdown-header-face-3 ((,class (:inherit org-level-3))))
     `(markdown-header-face-4 ((,class (:inherit org-level-4))))
     `(markdown-header-face-5 ((,class (:inherit org-level-5))))
     `(markdown-header-face-6 ((,class (:inherit org-level-6))))
     `(markdown-inline-code-face ((,class (:foreground ,nn-bluegreen7))))
     `(markdown-italic-face ((,class (:inherit italic))))
     `(markdown-link-face ((,class (:foreground ,nn-brightblue8))))
     `(markdown-markup-face ((,class (:foreground ,nn-lapis9))))
     `(markdown-reference-face ((,class (:inherit markdown-link-face))))
     `(markdown-url-face ((,class (:foreground ,nn-snowy4 :underline t))))

;;; --- Proof General and Coq
     `(proof-locked-face ((,class (:background "#182316"))))
     `(proof-error-face ((,class (:foreground ,nn-orange12))))
     `(proof-queue-face ((,class (:foreground ,nn-yellow13))))
     `(proof-tactics-name-face ((,class (:foreground ,nn-lapis9))))
     `(proof-tacticals-name-face ((,class (:foreground ,nn-orange12))))
     `(proof-queue-face ((,class (:foreground ,nn-yellow13))))
     `(proof-declaration-name-face ((,class (:foreground ,nn-green14))))

     `(coq-solve-tactics-face ((,class (:foreground ,nn-red11))))
     `(coq-cheat-face ((,class (:background ,nn-red11 :box (:line-width -1 :color ,nn-red11 :style nil)))))

;;; --- Rainbow Delimeters
     `(rainbow-delimiters-depth-1-face ((,class :foreground ,nn-bluegreen7)))
     `(rainbow-delimiters-depth-2-face ((,class :foreground ,nn-brightblue8)))
     `(rainbow-delimiters-depth-3-face ((,class :foreground ,nn-lapis9)))
     `(rainbow-delimiters-depth-4-face ((,class :foreground ,nn-blue10)))
     `(rainbow-delimiters-depth-5-face ((,class :foreground ,nn-orange12)))
     `(rainbow-delimiters-depth-6-face ((,class :foreground ,nn-yellow13)))
     `(rainbow-delimiters-depth-7-face ((,class :foreground ,nn-green14)))
     `(rainbow-delimiters-depth-8-face ((,class :foreground ,nn-purple15)))
     `(rainbow-delimiters-unmatched-face ((,class :foreground ,nn-red11)))

;;; --- Web Mode
     `(web-mode-attr-tag-custom-face ((,class (:foreground ,nn-attribute))))
     `(web-mode-builtin-face ((,class (:foreground ,nn-keyword))))
     `(web-mode-comment-face ((,class (:foreground ,nn-comment))))
     `(web-mode-comment-keyword-face ((,class (:foreground ,nn-comment))))
     `(web-mode-constant-face ((,class (:foreground ,nn-variable))))
     `(web-mode-css-at-rule-face ((,class (:foreground ,nn-annotation))))
     `(web-mode-css-function-face ((,class (:foreground ,nn-method))))
     `(web-mode-css-property-name-face ((,class (:foreground ,nn-keyword))))
     `(web-mode-css-pseudo-class-face ((,class (:foreground ,nn-class))))
     `(web-mode-css-selector-face ((,class (:foreground ,nn-keyword))))
     `(web-mode-css-string-face ((,class (:foreground ,nn-string))))
     `(web-mode-doctype-face ((,class (:foreground ,nn-preprocessor))))
     `(web-mode-function-call-face ((,class (:foreground ,nn-method))))
     `(web-mode-function-name-face ((,class (:foreground ,nn-method))))
     `(web-mode-html-attr-name-face ((,class (:foreground ,nn-attribute))))
     `(web-mode-html-attr-equal-face ((,class (:foreground ,nn-punctuation))))
     `(web-mode-html-attr-value-face ((,class (:foreground ,nn-string))))
     `(web-mode-html-entity-face ((,class (:foreground ,nn-keyword))))
     `(web-mode-html-tag-bracket-face ((,class (:foreground ,nn-punctuation))))
     `(web-mode-html-tag-custom-face ((,class (:foreground ,nn-tag))))
     `(web-mode-html-tag-face ((,class (:foreground ,nn-tag))))
     `(web-mode-html-tag-namespaced-face ((,class (:foreground ,nn-keyword))))
     `(web-mode-json-key-face ((,class (:foreground ,nn-class))))
     `(web-mode-json-string-face ((,class (:foreground ,nn-string))))
     `(web-mode-keyword-face ((,class (:foreground ,nn-keyword))))
     `(web-mode-preprocessor-face ((,class (:foreground ,nn-preprocessor))))
     `(web-mode-string-face ((,class (:foreground ,nn-string))))
     `(web-mode-symbol-face ((,class (:foreground ,nn-variable))))
     `(web-mode-type-face ((,class (:foreground ,nn-class))))
     `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
     `(web-mode-variable-name-face ((,class (:foreground ,nn-variable))))

;;; --- Racket
     `(racket-xp-unused-face ((,class (:strike-through nil :underline (:color ,nn-yellow13 :style wave)))))

;;; -- UI
;;; --- Anzu
     `(anzu-mode-line ((,class (:foreground, nn-brightblue8))))
     `(anzu-mode-line-no-match ((,class (:foreground, nn-red11))))

;;; --- Avy
     `(avy-lead-face ((,class (:background ,nn-red11 :foreground ,nn-snowy5))))
     `(avy-lead-face-0 ((,class (:background ,nn-blue10 :foreground ,nn-snowy5))))
     `(avy-lead-face-1 ((,class (:background ,nn-dark3 :foreground ,nn-snowy5))))
     `(avy-lead-face-2 ((,class (:background ,nn-purple15 :foreground ,nn-snowy5))))

;;; --- Blamer
     `(blamer-face ((,class (:italic t :foreground ,nn-dark1))))

;;; --- Company
     `(company-echo-common ((,class (:foreground ,nn-dark0 :background ,nn-snowy4))))
     `(company-preview ((,class (:foreground ,nn-snowy4 :background ,nn-blue10))))
     `(company-preview-common ((,class (:foreground ,nn-dark0 :background ,nn-brightblue8))))
     `(company-preview-search ((,class (:foreground ,nn-dark0 :background ,nn-brightblue8))))
     `(company-scrollbar-bg ((,class (:foreground ,nn-dark1 :background ,nn-dark1))))
     `(company-scrollbar-fg ((,class (:foreground ,nn-dark2 :background ,nn-dark2))))
     `(company-template-field ((,class (:foreground ,nn-dark0 :background ,nn-bluegreen7))))
     `(company-tooltip ((,class (:foreground ,nn-snowy4 :background ,nn-dark2))))
     `(company-tooltip-annotation ((,class (:foreground ,nn-orange12))))
     `(company-tooltip-annotation-selection ((,class (:foreground ,nn-orange12 :weight bold))))
     `(company-tooltip-common ((,class (:foreground ,nn-brightblue8))))
     `(company-tooltip-common-selection ((,class (:foreground ,nn-brightblue8 :background ,nn-dark3))))
     `(company-tooltip-mouse ((,class (:inherit highlight))))
     `(company-tooltip-selection ((,class (:background ,nn-dark3 :weight bold))))

;;; --- Corfu
     `(corfu-border ((,class (:background ,nn-dark3))))
     `(corfu-default ((,class (:background ,nn-brightblack1))))

;;; --- diff-hl
     `(diff-hl-change ((,class (:background ,nn-yellow13))))
     `(diff-hl-insert ((,class (:background ,nn-green14))))
     `(diff-hl-delete ((,class (:background ,nn-red11))))

;;; --- Eglot
     `(eglot-highlight-symbol-face ((,class (:background ,nn-dark3))))

;;; --- Evil
     `(evil-ex-info ((,class (:foreground ,nn-brightblue8))))
     `(evil-ex-substitute-replacement ((,class (:foreground ,nn-lapis9))))
     `(evil-ex-substitute-matches ((,class (:inherit isearch))))

;;; --- Flycheck
     `(flycheck-error ((,class (:underline (:style wave :color ,nn-red11)))))
     `(flycheck-fringe-error ((,class (:foreground ,nn-red11 :weight bold))))
     `(flycheck-fringe-info ((,class (:foreground ,nn-brightblue8 :weight bold))))
     `(flycheck-fringe-warning ((,class (:foreground ,nn-yellow13 :weight bold))))
     `(flycheck-info ((,class (:underline (:style wave :color ,nn-brightblue8)))))
     `(flycheck-warning ((,class (:underline (:style wave :color ,nn-yellow13)))))

;;; --- Git Gutter
     `(git-gutter:modified ((,class (:foreground ,nn-yellow13))))
     `(git-gutter:added ((,class (:foreground ,nn-green14))))
     `(git-gutter:deleted ((,class (:foreground ,nn-red11))))

;;; --- Git Gutter Plus
     `(git-gutter+-modified ((,class (:foreground ,nn-yellow13))))
     `(git-gutter+-added ((,class (:foreground ,nn-green14))))
     `(git-gutter+-deleted ((,class (:foreground ,nn-red11))))

;;; --- Gnus
     `(gnus-cite-1 ((,class (:foreground ,nn-green14))))
     `(gnus-cite-2 ((,class (:foreground ,nn-brightblue8))))
     `(gnus-cite-3 ((,class (:foreground ,nn-purple15))))
     `(gnus-cite-4 ((,class (:foreground ,nn-red11))))
     `(gnus-cite-5 ((,class (:foreground ,nn-orange12))))
     `(gnus-cite-6 ((,class (:foreground ,nn-yellow13))))
     `(gnus-cite-7 ((,class (:foreground ,nn-green14))))
     `(gnus-cite-8 ((,class (:foreground ,nn-blue10))))
     `(gnus-cite-9 ((,class (:foreground ,nn-purple15))))
     `(gnus-cite-10 ((,class (:foreground ,nn-red11))))
     `(gnus-cite-11 ((,class (:foreground ,nn-orange12))))

;;; --- Helm
     `(helm-bookmark-addressbook ((,class (:foreground ,nn-bluegreen7))))
     `(helm-bookmark-directory ((,class (:foreground ,nn-lapis9))))
     `(helm-bookmark-file ((,class (:foreground ,nn-brightblue8))))
     `(helm-bookmark-gnus ((,class (:foreground ,nn-blue10))))
     `(helm-bookmark-info ((,class (:foreground ,nn-green14))))
     `(helm-bookmark-man ((,class (:foreground ,nn-snowy4))))
     `(helm-bookmark-w3m ((,class (:foreground ,nn-lapis9))))
     `(helm-buffer-directory ((,class (:foreground ,nn-lapis9))))
     `(helm-buffer-file ((,class (:foreground ,nn-brightblue8))))
     `(helm-buffer-not-saved ((,class (:foreground ,nn-yellow13))))
     `(helm-buffer-process ((,class (:foreground ,nn-blue10))))
     `(helm-candidate-number ((,class (:foreground ,nn-snowy4 :weight bold))))
     `(helm-candidate-number-suspended ((,class (:foreground ,nn-snowy4))))
     `(helm-ff-directory ((,class (:foreground ,nn-lapis9 :weight bold))))
     `(helm-ff-dirs ((,class (:foreground ,nn-lapis9))))
     `(helm-ff-dotted-director ((,class (:foreground ,nn-lapis9 :underline t))))
     `(helm-ff-dotted-symlink-director ((,class (:foreground ,nn-bluegreen7 :weight bold))))
     `(helm-ff-executable ((,class (:foreground ,nn-brightblue8))))
     `(helm-ff-file ((,class (:foreground ,nn-snowy4))))
     `(helm-ff-invalid-symlink ((,class (:foreground ,nn-red11 :weight bold))))
     `(helm-ff-prefix ((,class (:foreground ,nn-dark0 :background ,nn-lapis9))))
     `(helm-ff-symlink ((,class (:foreground ,nn-bluegreen7))))
     `(helm-grep-cmd-line ((,class (:foreground ,nn-snowy4 :background ,nn-dark0))))
     `(helm-grep-file ((,class (:foreground ,nn-brightblue8))))
     `(helm-grep-finish ((,class (:foreground ,nn-snowy5))))
     `(helm-grep-lineno ((,class (:foreground ,nn-snowy4))))
     `(helm-grep-match ((,class (:inherit isearch))))
     `(helm-grep-running ((,class (:foreground ,nn-brightblue8))))
     `(helm-header ((,class (:foreground ,nn-lapis9 :background ,nn-dark2))))
     `(helm-header-line-left-margin ((,class (:foreground ,nn-lapis9 :background ,nn-dark2))))
     `(helm-history-deleted ((,class (:foreground ,nn-red11))))
     `(helm-history-remote ((,class (:foreground ,nn-snowy4))))
     `(helm-lisp-completion-info ((,class (:foreground ,nn-snowy4 :weight bold))))
     `(helm-lisp-show-completion ((,class (:inherit isearch))))
     `(helm-locate-finish ((,class (:foreground ,nn-green14))))
     `(helm-match ((,class (:foreground ,nn-brightblue8))))
     `(helm-match-item ((,class (:inherit isearch))))
     `(helm-moccur-buffer ((,class (:foreground ,nn-brightblue8))))
     `(helm-resume-need-update ((,class (:foreground ,nn-dark0 :background ,nn-yellow13))))
     `(helm-selection ((,class (:inherit highlight))))
     `(helm-selection-line ((,class (:background ,nn-dark2))))
     `(helm-source-header ((,class (:height 1.44 :foreground ,nn-brightblue8 :background ,nn-dark2))))
     `(helm-swoop-line-number-face ((,class (:foreground ,nn-snowy4 :background ,nn-dark0))))
     `(helm-swoop-target-word-face ((,class (:foreground ,nn-dark0 :background ,nn-bluegreen7))))
     `(helm-swoop-target-line-face ((,class (:background ,nn-yellow13 :foreground ,nn-dark3))))
     `(helm-swoop-target-line-block-face ((,class (:background ,nn-yellow13 :foreground ,nn-dark3))))
     `(helm-separator ((,class (:background ,nn-dark2))))
     `(helm-visible-mark ((,class (:background ,nn-dark2))))

;;; --- Magit
     `(magit-branch ((,class (:foreground ,nn-bluegreen7 :weight bold))))
     `(magit-diff-context-highlight ((,class (:background ,nn-dark2))))
     `(magit-diff-file-header ((,class (:foreground ,nn-brightblue8 :box (:color ,nn-brightblue8)))))
     `(magit-diffstat-added ((,class (:foreground ,nn-green14))))
     `(magit-diffstat-removed ((,class (:foreground ,nn-red11))))
     `(magit-hash ((,class (:foreground ,nn-brightblue8))))
     `(magit-hunk-heading ((,class (:foreground ,nn-lapis9))))
     `(magit-hunk-heading-highlight ((,class (:foreground ,nn-lapis9 :background ,nn-dark2))))
     `(magit-item-highlight ((,class (:foreground ,nn-brightblue8 :background ,nn-dark2))))
     `(magit-log-author ((,class (:foreground ,nn-bluegreen7))))
     `(magit-process-ng ((,class (:foreground ,nn-yellow13 :weight bold))))
     `(magit-process-ok ((,class (:foreground ,nn-green14 :weight bold))))
     `(magit-section-heading ((,class (:foreground ,nn-bluegreen7 :weight bold))))
     `(magit-section-highlight ((,class (:background ,nn-brightblack1))))

;;; --- Minimap
     `(minimap-active-region-background ((,class (:background ,nn-dark0))))

;;; --- MU4E
     `(mu4e-header-marks-face ((,class (:foreground ,nn-lapis9))))
     `(mu4e-title-face ((,class (:foreground ,nn-brightblue8))))
     `(mu4e-header-key-face ((,class (:foreground ,nn-brightblue8))))
     `(mu4e-related-face ((,class (:foreground ,nn-lessdark3-1 :slant italic))))
     `(mu4e-highlight-face ((,class (:highlight))))
     `(mu4e-flagged-face ((,class (:foreground ,nn-yellow13))))
     `(mu4e-unread-face ((,class (:foreground ,nn-green14 :weight bold))))
     `(mu4e-link-face ((,class (:underline t))))

;;; --- Powerline
     `(powerline-active1 ((,class (:foreground ,nn-snowy4 :background ,nn-dark1))))
     `(powerline-active2 ((,class (:foreground ,nn-snowy4 :background ,nn-dark3))))
     `(powerline-inactive1 ((,class (:background ,nn-dark2))))
     `(powerline-inactive2 ((,class (:background ,nn-dark2))))

;;; --- Powerline Evil
     `(powerline-evil-base-face ((,class (:foreground ,nn-snowy4))))
     `(powerline-evil-normal-face ((,class (:background ,nn-brightblue8))))
     `(powerline-evil-insert-face ((,class (:foreground ,nn-dark0 :background ,nn-snowy4))))
     `(powerline-evil-visual-face ((,class (:foreground ,nn-dark0 :background ,nn-bluegreen7))))
     `(powerline-evil-replace-face ((,class (:foreground ,nn-dark0 :background ,nn-lapis9))))

;;; --- Prism
     `(prism-level-0 ((,class (:foreground ,nn-bluegreen7))))
     `(prism-level-1 ((,class (:foreground ,nn-purple15))))
     `(prism-level-2 ((,class (:foreground ,nn-brightblue8))))
     `(prism-level-3 ((,class (:foreground ,nn-green14))))
     `(prism-level-4 ((,class (:foreground ,nn-lapis9))))
     `(prism-level-5 ((,class (:foreground ,nn-yellow13))))
     `(prism-level-6 ((,class (:foreground ,nn-blue10))))
     `(prism-level-7 ((,class (:foreground ,nn-orange12))))
     `(prism-level-8 ((,class (:foreground ,nn-red11))))

;;; --- Pulsar
     `(pulsar-red ((,class (:background ,nn-bg11))))
     `(pulsar-green ((,class (:background ,nn-bg14))))
     `(pulsar-blue ((,class (:background ,nn-bg08))))
     `(pulsar-cyan ((,class (:background ,nn-bg07))))
     `(pulsar-yellow ((,class (:background ,nn-bg13))))
     `(pulsar-magenta ((,class (:background ,nn-bg15))))

;;; --- NeoTree
     `(neo-banner-face ((,class (:foreground ,nn-blue10))))
     `(neo-dir-link-face ((,class (:foreground ,nn-lapis9))))
     `(neo-expand-btn-face ((,class (:foreground ,nn-snowy6 :bold t))))
     `(neo-file-link-face ((,class (:foreground ,nn-snowy4))))
     `(neo-root-dir-face ((,class (:foreground ,nn-bluegreen7 :weight bold))))
     `(neo-vc-added-face ((,class (:foreground ,nn-green14))))
     `(neo-vc-conflict-face ((,class (:foreground ,nn-red11))))
     `(neo-vc-default-face ((,class (:foreground ,nn-snowy4))))
     `(neo-vc-edited-face ((,class (:foreground ,nn-yellow13))))
     `(neo-vc-ignored-face ((,class (:foreground ,nn-dark3))))
     `(neo-vc-missing-face ((,class (:foreground ,nn-orange12))))
     `(neo-vc-needs-merge-face ((,class (:background ,nn-orange12 :foreground ,nn-snowy4))))
     `(neo-vc-needs-update-face ((,class (:background ,nn-blue10 :foreground ,nn-snowy4))))
     `(neo-vc-removed-face ((,class (:foreground ,nn-red11 :strike-through nil))))
     `(neo-vc-up-to-date-face ((,class (:foreground ,nn-snowy4))))
     `(neo-vc-user-face ((,class (:foreground ,nn-snowy4))))

;;; --- Cider
     `(cider-result-overlay-face ((,class (:background unspecified))))

;;; --- Olivetti
     `(olivetti-fringe ((,class (:background ,nn-brightblack1))))

;;; --- Org
     `(org-level-1 ((,class (:foreground ,nn-brightblue8 :weight extra-bold))))
     `(org-level-2 ((,class (:foreground ,nn-lapis9 :weight bold))))
     `(org-level-3 ((,class (:foreground ,nn-blue10 :weight semi-bold))))
     `(org-level-4 ((,class (:foreground ,nn-bluegreen7 :weight normal))))
     `(org-level-5 ((,class (:inherit org-level-4))))
     `(org-level-6 ((,class (:inherit org-level-4))))
     `(org-level-7 ((,class (:inherit org-level-4))))
     `(org-level-8 ((,class (:inherit org-level-4))))
     `(org-agenda-clocking ((,class (:box ,nn-purple15))))
     `(org-agenda-date ((,class (:foreground ,nn-brightblue8 :underline t))))
     `(org-agenda-date-today ((,class (:foreground ,nn-blue10 :weight bold :inverse-video t :underline nil :extend t))))
     `(org-agenda-date-weekend ((,class (:foreground ,nn-lapis9))))
     `(org-agenda-dimmed-todo-face ((,class (:background ,nn-yellow13))))
     `(org-agenda-done ((,class (:foreground ,nn-lessdark3-2 :weight light :slant italic))))
     `(org-agenda-structure ((,class (:foreground ,nn-lapis9))))
     `(org-block ((,class (:foreground ,nn-snowy4))))
     `(org-block-background ((,class (:background ,nn-dark0))))
     `(org-block-begin-line ((,class (:foreground ,nn-bluegreen7))))
     `(org-block-end-line ((,class (:foreground ,nn-bluegreen7))))
     `(org-checkbox ((,class (:foreground ,nn-lapis9))))
     `(org-checkbox-statistics-done ((,class (:foreground ,nn-green14))))
     `(org-checkbox-statistics-todo ((,class (:foreground ,nn-yellow13))))
     `(org-code ((,class (:foreground ,nn-green14))))
     `(org-column ((,class (:background ,nn-dark2))))
     `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
     `(org-date ((,class (:foreground ,nn-brightblue8))))
     `(org-document-info ((,class (:foreground ,nn-snowy4))))
     `(org-document-info-keyword ((,class (:foreground ,nn-lessdark3-1 :weight bold))))
     `(org-document-title ((,class (:foreground ,nn-brightblue8 :weight bold))))
     `(org-done ((,class (:foreground ,nn-green14 :weight bold))))
     `(org-ellipsis ((,class (:foreground ,nn-dark3))))
     `(org-footnote ((,class (:foreground ,nn-brightblue8))))
     `(org-formula ((,class (:foreground ,nn-lapis9))))
     `(org-headline-done ((,class (:foreground ,nn-lessdark3-1))))
     `(org-hide ((,class (:foreground ,nn-dark0 :background ,nn-dark0))))
     `(org-link ((,class (:underline ,nn-brightblue8))))
     `(org-priority ((,class (:foreground ,nn-purple15))))
     `(org-quote ((,class (:inherit org-block :slant italic :foreground ,nn-green14))))
     `(org-scheduled ((,class (:foreground ,nn-green14))))
     `(org-scheduled-previously ((,class (:foreground ,nn-orange12))))
     `(org-scheduled-today ((,class (:foreground ,nn-yellow13))))
     `(org-special-keyword ((,class (:foreground ,nn-lapis9))))
     `(org-table ((,class (:foreground ,nn-lapis9))))
     `(org-todo ((,class (:foreground ,nn-orange12 :weight bold))))
     `(org-upcoming-deadline ((,class (:foreground ,nn-snowy4))))
     `(org-upcoming-distant-deadline ((,class (:foreground ,nn-lessdark3-1))))
     `(org-verbatim ((,class (:foreground ,nn-bluegreen7))))
     `(org-verse ((,class (:inherit org-block :slant italic))))
     `(org-warning ((,class (:foreground ,nn-red11 :weight bold))))

     `(font-latex-bold-face ((,class (:inherit bold))))
     `(font-latex-italic-face ((,class (:slant italic))))
     `(font-latex-string-face ((,class (:foreground ,nn-green14))))
     `(font-latex-match-reference-keywords ((,class (:foreground ,nn-lapis9))))
     `(font-latex-match-variable-keywords ((,class (:foreground ,nn-snowy4))))
     `(ido-only-match ((,class (:foreground ,nn-brightblue8))))
     `(org-sexp-date ((,class (:foreground ,nn-bluegreen7))))
     `(ido-first-match ((,class (:foreground ,nn-brightblue8 :weight bold))))
     `(ido-subdir ((,class (:foreground ,nn-lapis9))))

;;; --- Vertico
     `(vertico-current ((,class (:background ,nn-dark1))))

;;; --- ivy-mode
     `(ivy-current-match ((,class (:inherit region))))
     `(ivy-minibuffer-match-face-1 ((,class (:inherit default))))
     `(ivy-minibuffer-match-face-2 ((,class (:background ,nn-bluegreen7 :foreground ,nn-dark0))))
     `(ivy-minibuffer-match-face-3 ((,class (:background ,nn-brightblue8 :foreground ,nn-dark0))))
     `(ivy-minibuffer-match-face-4 ((,class (:background ,nn-lapis9 :foreground ,nn-dark0))))
     `(ivy-remote ((,class (:foreground ,nn-green14))))
     `(ivy-posframe ((,class (:background ,nn-dark1))))
     `(ivy-posframe-border ((,class (:background ,nn-dark1))))
     `(ivy-remote ((,class (:foreground ,nn-green14))))

;;; --- perspective
     `(persp-selected-face ((,class (:foreground ,nn-brightblue8 :weight bold))))

;;; --- xref
     `(xref-match ((,class (:background ,nn-lessdark3-1 :inherit nil)))))))

(nordic-night--build-theme 'nordic-night
                           (if (nordic-night--fullcolorp)
                               (append nordic-night-nord-colors nordic-night-colors nordic-night-auroa-bg)
                             (append nordic-night-nord-colors-shell nordic-night-colors-shell nordic-night-auroa-bg-shell)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'nordic-night)

(provide 'nordic-night-theme)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:

;;; nordic-night-theme.el ends here
