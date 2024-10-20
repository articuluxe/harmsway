;;; glass-tty-theme.el --- Reverse video-like theme for the Glass TTY VT220 font

;; Copyright (C) 2024 by Matthew X. Economou
;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Matthew X. Economou <xenophon+glass-tty-theme@irtnog.org>
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/irtnog/glass-tty-theme
;; Package-Version: 1.0.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Reverse video-like theme for use with the Glass TTY VT220 font.

;; Like the 'reverse' theme, this mimics the color theme applied when
;; starting Emacs in reverse video mode while adding support for
;; multiple frames.  Additionally, 'glass-tty' addresses some issues
;; 'reverse' has with multiple frames while remaining backwards
;; compatible with the default theme for tty frames.  Because this
;; theme is also designed around the Glass TTY VT220 font, it
;; automatically scales the font correctly depending on the windowing
;; system.  The end result is a color theme suitable for use when
;; Emacs runs as a service across multiple platforms.

;; Before using this theme, dowload and install the font from
;; https://caglrc.cc/~svo/glasstty/.  How to change Emacs' default
;; font depends on the windowing system:

;; - On Windows, create a Registry key named
;;   HKEY_CURRENT_USER\\Software\\GNU\\Emacs\\ containing a string
;;   value 'Emacs.Font' with the data '-*-Glass TTY
;;   VT220-*-*-*--*-150-*-*-*-*-*-*'.

;; - On X11, add the following to ~/.Xresources: 'Emacs.Font: -*-Glass
;;   TTY VT220-*-*-*--*-150-*-*-*-*-*-*'

;; - On macOS, run the following Emacs Lisp expression: '(add-to-list
;;   'default-frame-alist '(font . \"-*-Glass TTY
;;   VT220-*-*-*--*-200-*-*-*-*-*-*\"))'

;; See also:
;; https://www.emacswiki.org/emacs/SetFonts
;; https://www.emacswiki.org/emacs/ChangeFontsPermanentlyOnWindows

;;; Code:

(deftheme glass-tty
  "Reverse video-like theme for use with the Glass TTY VT220 font.

Like the 'reverse' theme, this mimics the color theme applied
when starting Emacs in reverse video mode while adding support
for multiple frames.  Additionally, 'glass-tty' addresses some
issues 'reverse' has with multiple frames while remaining
backwards compatible with the default theme for tty frames.
Because this theme is also designed around the Glass TTY VT220
font, it automatically scales the font correctly depending on the
windowing system.  The end result is a color theme suitable for
use when Emacs runs as a service across multiple platforms.

Before using this theme, dowload and install the font from
https://caglrc.cc/~svo/glasstty/.  How to change Emacs' default
font depends on the windowing system:

- On Windows, create a Registry key named
  HKEY_CURRENT_USER\\Software\\GNU\\Emacs\\ containing a string value
  'Emacs.Font' with the data
  '-*-Glass TTY VT220-*-*-*--*-150-*-*-*-*-*-*'.

- On X11, add the following to ~/.Xresources:
  'Emacs.Font: -*-Glass TTY VT220-*-*-*--*-150-*-*-*-*-*-*'

- On macOS, run the following Emacs Lisp expression:
  '(add-to-list 'default-frame-alist
                '(font . \"-*-Glass TTY VT220-*-*-*--*-200-*-*-*-*-*-*\"))'

See also:
https://www.emacswiki.org/emacs/SetFonts
https://www.emacswiki.org/emacs/ChangeFontsPermanentlyOnWindows"
  :background-mode 'dark)

;; For reference:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Creating-Custom-Themes.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Custom-Themes.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Applying-Customizations.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Systems.html

(custom-theme-set-faces
 'glass-tty
 '(default
   ((default (:height 150 :background "black" :foreground "white"))
    (((type ns)) (:height 200))
    (((type tty)) (:family "default" :height 1 :background "unspecified-bg" :foreground "unspecified-fg"))))

 '(ace-jump-face-foreground ((((type graphics)) (:foreground "yellow" :weight bold))))

 '(anzu-mode-line ((((type graphics)) (:foreground "yellow"))))
 '(anzu-replace-to ((((type graphics)) (:foreground "yellow" :background "grey10"))))

 '(button ((((type graphics)) (:underline t :foreground "cyan1"))))

 '(compilation-error ((((type graphics)) ( :underline nil))))
 '(compilation-line-number ((((type graphics)) ( :underline t))))
 '(completions-annotations ((((type graphics)) (:underline t))))
 '(completions-common-part ((((type graphics)) (:foreground "white" :background "black"))))
 '(completions-first-difference ((((type graphics)) (:weight bold))))

 '(cursor ((((type graphics)) (:foreground "white"))))

 '(diff-added-face ((((type graphics)) (:background nil :foreground "green" :weight normal))))
 '(diff-file-header-face ((((type graphics)) (:background nil :weight extra-bold))))
 '(diff-header-face ((((type graphics)) (:background nil :weight extra-bold))))
 '(diff-hunk-header-face ((((type graphics)) (:foreground "turquoise" :weight extra-bold :underline t))))
 '(diff-refine-added ((((type graphics)) (:background nil :underline "green"))))
 '(diff-refine-change ((((type graphics)) (:background nil))))
 '(diff-refine-removed ((((type graphics)) (:background nil :underline "red"))))
 '(diff-removed-face ((((type graphics)) (:background nil :foreground "firebrick1" :weight normal))))

 '(dired-directory ((((type graphics)) (:foreground "LightSkyBlue"))))
 '(dired-flagged ((((type graphics)) (:weight bold :foreground "Pink"))))
 '(dired-header ((((type graphics)) (:foreground "PaleGreen"))))
 '(dired-ignored ((((type graphics)) (:foreground "grey70"))))
 '(dired-marked ((((type graphics)) (:weight bold :foreground "DarkOrange"))))
 '(dired-mark ((((type graphics)) (:foreground "Aquamarine"))))
 '(dired-perm-write ((((type graphics)) (:foreground "chocolate1"))))
 '(dired-symlink ((((type graphics)) (:foreground "Cyan1"))))
 '(dired-warning ((((type graphics)) (:foreground "Pink" :weight bold))))

 '(emms-playlist-track-face ((((type graphics)) (:foreground "cyan"))))

 '(error ((((type graphics)) (:foreground "#d54e53" :weight bold))))

 '(escape-glyph ((((type graphics)) (:foreground "cyan"))))

 '(file-name-shadow ((((type graphics)) (:foreground "grey70"))))

 '(flycheck-error-list-highlight ((((type graphics)) (:background "grey15"))))
 '(flycheck-error ((((type graphics)) (:foreground "yellow" :weight bold :background "red"))))
 '(flycheck-info ((((type graphics)) (:style wave :color "green" :underline t))))
 '(flycheck-warning ((((type graphics)) (:weight bold :underline "darkorange" :foreground nil :background nil))))

 '(font-lock-builtin-face ((((type graphics)) (:foreground "LightSteelBlue"))))
 '(font-lock-comment-delimiter-face ((((type graphics)) (:foreground "chocolate1"))))
 '(font-lock-comment-face ((((type graphics)) (:foreground "chocolate1"))))
 '(font-lock-constant-face ((((type graphics)) (:foreground "Aquamarine"))))
 '(font-lock-doc-face ((((type graphics)) (:foreground "tomato"))))
 '(font-lock-function-name-face ((((type graphics)) (:foreground "LightSkyBlue"))))
 '(font-lock-keyword-face ((((type graphics)) (:foreground "Cyan1"))))
 '(font-lock-negation-char-face ((((type graphics)) (nil))))
 '(font-lock-preprocessor-face ((((type graphics)) (:foreground "LightSteelBlue"))))
 '(font-lock-regexp-grouping-backslash ((((type graphics)) (:weight bold))))
 '(font-lock-regexp-grouping-construct ((((type graphics)) (:weight bold))))
 '(font-lock-string-face ((((type graphics)) (:foreground "tomato"))))
 '(font-lock-type-face ((((type graphics)) (:foreground "PaleGreen"))))
 '(font-lock-variable-name-face ((((type graphics)) (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((((type graphics)) (:weight bold :foreground "Pink"))))
 '(fringe ((((type graphics)) (:background "grey10"))))

 '(glyphless-char ((((type graphics)) (:height 0.6))))

 '(gnus-cite-1 ((((type graphics)) ( :foreground "cyan"))))

 '(guide-key/highlight-command-face ((((type graphics)) (:foreground "green"))))
 '(guide-key/key-face ((((type graphics)) (:foreground "white"))))
 '(guide-key/prefix-command-face ((((type graphics)) (:inherit font-lock-keyword-face))))

 '(header-line ((((type graphics)) (:box (:line-width -1 :style released-button) :background "grey20" :foreground "grey90" :box nil))))

 '(helm-ff-directory ((((type graphics)) (:foreground "cyan" :background nil :underline t))))
 '(helm-ff-file ((((type graphics)) (:foreground "white" :background nil))))
 '(helm-grep-lineno ((((type graphics)) (:foreground "IndianRed1"))))
 '(helm-gtags-file ((((type graphics)) (:foreground "aquamarine1"))))
 '(helm-gtags-lineno ((((type graphics)) (:foreground "IndianRed1" :underline nil))))
 '(helm-moccur-buffer ((((type graphics)) (:foreground "aquamarine1" :underline nil))))
 '(helm-selection ((((type graphics)) (:inherit highlight))))

 '(help-argument-name ((((type graphics)) (nil))))

 '(highlight-symbol-face ((((type graphics)) (:foreground "black" :background "white"))))
 '(highlight ((((type graphics)) (:background "#484848"))))

 '(isearch-fail ((((type graphics)) (:background "red4"))))
 '(isearch ((((type graphics)) (:background "palevioletred2" :foreground "brown4"))))

 '(italic ((((type graphics)) (:underline t))))

 '(jedi:highlight-function-argument ((((type graphics)) (:foreground "green"))))

 '(lazy-highlight ((((type graphics)) (:background "paleturquoise4"))))

 '(link ((((type graphics)) (:foreground "cyan1" :underline t))))
 '(link-visited ((((type graphics)) (:underline t :foreground "violet"))))

 '(magit-branch ((((type graphics)) (:foreground "yellow" :weight bold :underline t))))
 '(magit-item-highlight ((((type graphics)) (:background "gray3" :weight normal))))

 '(match ((((type graphics)) (:background "RoyalBlue3"))))

 '(menu ((((type graphics)) (nil))))

 '(minibuffer-prompt ((((type graphics)) (:foreground "cyan"))))

 '(mode-line-buffer-id ((((type graphics)) (:foreground "orange" :weight bold))))
 '(mode-line-buffer-id ((((type graphics)) (:weight bold :foreground "orange"))))
 '(mode-line-emphasis ((((type graphics)) (:weight bold))))
 '(mode-line-highlight ((((type graphics)) (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((((type graphics)) (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
 '(mode-line ((((type graphics)) (:background "#333333" :foreground "#bbbbbc"))))
 '(mode-line ((((type graphics)) (:background "#333333" :foreground "#cccccd"))))

 '(mouse ((((type graphics)) (nil))))

 '(next-error ((((type graphics)) (:background "blue3"))))

 '(nobreak-space ((((type graphics)) (:foreground "cyan" :underline t))))

 '(org-block ((((type graphics)) (:foreground "green"))))
 '(org-checkbox ((((type graphics)) (:foreground "LawnGreen"))))
 '(org-document-title ((((type graphics)) (:foreground "cyan"))))
 '(org-level-1 ((((type graphics)) ( :foreground "hotpink" :weight bold))))
 '(org-level-2 ((((type graphics)) ( :foreground "yellow" :weight semi-bold))))
 '(org-level-4 ((((type graphics)) ( :foreground "grey80"))))
 '(org-tag ((((type graphics)) (:foreground "green yellow"))))
 '(org-warning ((((type graphics)) ( :foreground "hotpink"))))

 '(query-replace ((((type graphics)) (:foreground "brown4" :background "palevioletred2"))))

 '(region ((((type graphics)) (:background "blue3"))))

 '(scroll-bar ((((type graphics)) (nil))))

 '(secondary-selection ((((type graphics)) (:background "SkyBlue4"))))

 '(shadow ((((type graphics)) (:foreground "grey70"))))

 '(success ((((type graphics)) (:foreground "Green1" :weight bold))))

 '(tool-bar ((((type graphics)) (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))

 '(tooltip ((((type graphics)) (:background "lightyellow" :foreground "black"))))

 '(trailing-whitespace ((((type graphics)) (:background "red1"))))

 '(underline ((((type graphics)) (:underline t))))

 '(vertical-border ((((type graphics)) (nil))))

 '(warning ((((type graphics)) (:foreground "DarkOrange" :weight bold))))

 '(widget-button-pressed ((((type graphics)) (:foreground "red1"))))
 '(widget-button ((((type graphics)) (:weight bold))))
 '(widget-documentation ((((type graphics)) (:foreground "lime green"))))
 '(widget-field ((((type graphics)) (:background "dim gray"))))
 '(widget-inactive ((((type graphics)) (:foreground "grey70")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'glass-tty)

;;; glass-tty-theme.el ends here
