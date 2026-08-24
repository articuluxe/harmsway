;;; ronny-theme.el --- A dark colorscheme inspired by Monokai -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;;
;; Author: Vadym-Valdis Yudaiev <judaew@outlook.com>
;; URL: https://github.com/judaew/ronny.el
;; SPDX-License-Identifier: MIT
;; Version: 0.1
;; Package-Requires: ((emacs "27"))

;;; Commentary:
;;
;; ronny.el is a dark colorscheme for Emacs, mostly inspired by the original
;; Monokai created by Wimer Hazenberg.
;;
;; It aims to preserve the familiar Monokai aesthetic while offering:
;; - a cooler, more neutral background and UI
;; - subtle interface elements that keep the focus on the code
;; - muted yet readable comments that reduce visual noise
;; - expanded semantic highlighting for modern font-lock faces,
;;   Tree-sitter, and modern packages.
;;
;; Credits:
;;
;; Wimer Hazenberg created the original theme.
;; - http://www.monokai.nl/blog/2006/07/15/textmate-color-theme/
;;
;;; Code:

(unless (>= emacs-major-version 27)
  (error "The ronny theme requires Emacs 27 or later!"))

(deftheme ronny "The Ronny colour theme.")

(defgroup ronny nil
  "Ronny theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom ronny-black "#1B1D1E"
  "ANSI 0 - black."
  :type 'string
  :group 'ronny)

(defcustom ronny-red "#FF0044"
  "ANSI 1 - red."
  :type 'string
  :group 'ronny)

(defcustom ronny-green "#82B414"
  "ANSI 2 - green."
  :type 'string
  :group 'ronny)

(defcustom ronny-yellow "#FD971F"
  "ANSI 3 - yellow."
  :type 'string
  :group 'ronny)

(defcustom ronny-blue "#266C98"
  "ANSI 4 - blue."
  :type 'string
  :group 'ronny)

(defcustom ronny-purple "#AC0CB1"
  "ANSI 5 - purple."
  :type 'string
  :group 'ronny)

(defcustom ronny-cyan "#00AAAA"
  "ANSI 6 - cyan."
  :type 'string
  :group 'ronny)

(defcustom ronny-white "#CCCCCC"
  "ANSI 7 - white."
  :type 'string
  :group 'ronny)

(defcustom ronny-bright-black "#808080"
  "ANSI 8 - bright black."
  :type 'string
  :group 'ronny)

(defcustom ronny-bright-red "#F92672"
  "ANSI 9 - bright red."
  :type 'string
  :group 'ronny)

(defcustom ronny-bright-green "#A6E22E"
  "ANSI 10 - bright green."
  :type 'string
  :group 'ronny)

(defcustom ronny-bright-yellow "#E6DB74"
  "ANSI 11 - bright yellow."
  :type 'string
  :group 'ronny)

(defcustom ronny-bright-blue "#7070F0"
  "ANSI 12 - bright blue."
  :type 'string
  :group 'ronny)

(defcustom ronny-bright-purple "#D63AE1"
  "ANSI 13 - bright purple."
  :type 'string
  :group 'ronny)

(defcustom ronny-bright-cyan "#66D9EF"
  "ANSI 14 - bright cyan."
  :type 'string
  :group 'ronny)

(defcustom ronny-bright-white "#F8F8F2"
  "ANSI 15 - bright white."
  :type 'string
  :group 'ronny)

(defcustom ronny-violet "#AE81FF"
  "Other color - violet."
  :type 'string
  :group 'ronny)

(defcustom ronny-bright-violet "#CEB3FF"
  "Other color - bright violet."
  :type 'string
  :group 'ronny)

(defcustom ronny-gray "#7E8E91"
  "Other color - gray."
  :type 'string
  :group 'ronny)

(defcustom ronny-gray2 "#465457"
  "Other color - gray2."
  :type 'string
  :group 'ronny)

(defcustom ronny-black2 "#232526"
  "Other color - black2."
  :type 'string
  :group 'ronny)

(defcustom ronny-black3 "#343434"
  "Other color - black3."
  :type 'string
  :group 'ronny)

(defcustom ronny-heading1-color "#FD971F" ;; ,ronny-yellow
  "Color for level 1 headings."
  :type 'string
  :group 'ronny)

(defcustom ronny-heading2-color "#66D9EF" ;; ,ronny-bright-cyan
  "Color for level 2 headings."
  :type 'string
  :group 'ronny)

(defcustom ronny-heading3-color "#A6E22E" ;; ,ronny-bright-green
  "Color for level 3 headings."
  :type 'string
  :group 'ronny)

(defcustom ronny-heading4-color "#F92672" ;; ,ronny-bright-red
  "Color for level 4 headings."
  :type 'string
  :group 'ronny)

(defcustom ronny-heading5-color "#CEB3FF" ;; ,ronny-bright-violet
  "Color for level 5 headings."
  :type 'string
  :group 'ronny)

(defcustom ronny-heading6-color "#7070F0" ;; ,ronny-bright-blue
  "Color for level 6 headings."
  :type 'string
  :group 'ronny)

(custom-theme-set-faces
 'ronny
 ;; Basic faces
 `(default ((t (:foreground ,ronny-white :background ,ronny-black))))
 `(cursor ((t (:foreground ,ronny-white :background ,ronny-bright-red))))
 `(fringe ((t (:inherit default)))) ;; if user don't want
 ;; `(fringe ((t (:background ,ronny-black2))))
 `(region ((t (:background "#403D3D"))))
 `(highlight ((t (:foreground ,ronny-black :background ,ronny-yellow))))
 ;; `(secondary-selection ((t (:background unspecified))))
 `(shadow ((t (:inherit font-lock-comment-face :slant normal))))
 `(tooltip ((t (:foreground "#D6D6D4" :background ,ronny-black2))))

 ;; Mode line
 `(mode-line ((t (:foreground ,ronny-bright-white :background ,ronny-black3 :box unspecified))))
 `(mode-line-inactive ((t (:foreground ,ronny-bright-black :background "#171819" :box unspecified))))
 `(mode-line-buffer-id ((t (:weight bold))))

 ;; Menu (this line: File Edit Options Buffers Help)
 `(menu ((t (:foreground ,ronny-white :background ,ronny-black3))))
 `(tty-menu-disabled-face ((t (:foreground "#555556" :background ,ronny-black2))))
 `(tty-menu-enabled-face ((t (:inherit tooltip))))
 `(tty-menu-selected-face ((t (:background ,ronny-black3))))

 ;; Line numbers
 `(line-number ((t (:foreground "#555556" :background ,ronny-black2))))
 `(line-number-current-line ((t (:foreground ,ronny-white :background ,ronny-black2))))

 ;; Search
 `(isearch ((t (:foreground ,ronny-black :background ,ronny-yellow))))
 ;; `(isearch-fail ((t (:background unspecified :foreground unspecified))))
 `(lazy-highlight ((t (:foreground ,ronny-black :background ,ronny-bright-yellow))))

 ;; Error, warning, success
 `(error ((t (:foreground ,ronny-bright-red :weight bold))))
 `(warning ((t (:foreground ,ronny-bright-yellow :weight bold))))
 `(success ((t (:foreground ,ronny-green :weight bold))))

 ;; Minibuffer
 `(minibuffer-prompt ((t (:foreground ,ronny-yellow))))

 ;; Link
 `(link ((t (:foreground ,ronny-yellow :underline t))))
 `(link-visited ((t (:foreground ,ronny-violet :underline t))))

 ;; Header line
 `(header-line ((t (:inherit mode-line))))

 ;; Vertical border
 `(vertical-border ((t (:foreground ,ronny-gray2))))

 ;; Escape glyph
 `(escape-glyph ((t (:foreground ,ronny-gray2))))

 ;; Match parentheses
 `(show-paren-match ((t (:foreground ,ronny-black :background ,ronny-yellow))))
 `(show-paren-mismatch ((t (:foreground ,ronny-black :background ,ronny-red))))

 ;; Font lock (syntax highlighting)
 `(font-lock-builtin-face ((t (:foreground ,ronny-bright-cyan, :slant italic))))
 ;; TODO: this original monokai color, make var
 ;;`(font-lock-comment-face ((t (:foreground "#75715E" :slant italic))))
 `(font-lock-comment-face ((t (:foreground "#7E8E91" :slant italic))))
 `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :slant normal))))
 `(font-lock-punctuation-face ((t (:inherit font-lock-delimiter-face))))
 `(font-lock-constant-face ((t (:foreground ,ronny-violet))))
 `(font-lock-number-face ((t (:foreground ,ronny-violet))))
 `(font-lock-regexp-face ((t (:inherit font-lock-string-face))))
 `(font-lock-delimiter-face ((t (:inherit font-lock-comment-face :slant normal))))
 `(font-lock-bracket-face ((t (:inherit font-lock-delimiter-face))))
 `(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 `(font-lock-doc-markup-face ((t (:foreground ,ronny-bright-cyan :slant italic))))
 `(font-lock-escape-face ((t (:foreground ,ronny-bright-red))))
 `(font-lock-keyword-face ((t (:foreground ,ronny-bright-red))))
 `(font-lock-operator-face ((t (:foreground ,ronny-bright-red))))
 `(font-lock-negation-char-face ((t (:foreground ,ronny-bright-red))))
 `(font-lock-preprocessor-face ((t (:foreground ,ronny-bright-red))))
 `(font-lock-regexp-grouping-construct ((t (:foreground ,ronny-bright-red))))
 `(font-lock-regexp-grouping-backslash ((t (:foreground ,ronny-bright-red))))
 `(font-lock-string-face ((t (:foreground ,ronny-bright-yellow))))
 `(font-lock-type-face ((t (:foreground ,ronny-bright-cyan))))
 `(font-lock-variable-name-face ((t (:foreground ,ronny-yellow))))
 `(font-lock-warning-face ((t (:foreground ,ronny-bright-red))))
 `(font-lock-function-call-face ((t (:foreground ,ronny-bright-green))))
 `(font-lock-function-name-face ((t (:foreground ,ronny-bright-green))))
 `(font-lock-variable-use-face ((t (:foreground ,ronny-yellow))))
 `(font-lock-variable-name-face ((t (:foreground ,ronny-yellow))))
 `(font-lock-property-use-face ((t (:foreground ,ronny-yellow))))
 `(font-lock-property-name-face ((t (:foreground ,ronny-yellow))))

 ;; outline-*
 `(outline-1 ((t (:foreground ,ronny-heading1-color))))
 `(outline-2 ((t (:foreground ,ronny-heading2-color))))
 `(outline-3 ((t (:foreground ,ronny-heading3-color))))
 `(outline-4 ((t (:foreground ,ronny-heading4-color))))
 `(outline-5 ((t (:foreground ,ronny-heading5-color))))
 `(outline-6 ((t (:foreground ,ronny-heading6-color))))

 ;; Tab line
 `(tab-line-tab ((t (:foreground ,ronny-bright-white :background ,ronny-black))))
 `(tab-line-tab-current ((t (:inherit tab-line-tab))))
 `(tab-line-tab-inactive ((t (:foreground ,ronny-white :background ,ronny-black3))))
 ;; Tab bar
 `(tab-bar-tab ((t (:foreground ,ronny-bright-white :background ,ronny-black))))
 `(tab-bar-tab-inactive ((t (:foreground ,ronny-white :background ,ronny-black3))))
 `(tab-bar-tab-highlight ((t (:foreground ,ronny-white :background ,ronny-black2 :box (:line-width 1 :style released-button)))))

 ;; TODO: org-mode
 `(org-document-title ((t (:weight bold :foreground ,ronny-heading1-color))))
 `(org-drawer ((t (:inherit font-lock-comment-face))))
 `(org-document-info ((t (:foreground ,ronny-yellow))))
 ;; `(org-date ((t ())))
 ;; `(org-todo ((t ())))
 ;; `(org-done ((t ())))
 `(org-block ((t (:background ,ronny-black2))))
 ;; `(org-block-begin-line ((t ())))
 ;; `(org-table ((t ())))
 ;; `(org-code ((t ())))
 ;; `(org-verbatim ((t ())))

 ;; dired-mode
 `(dired-symlink ((t (:foreground ,ronny-violet :slant italic))))
 `(dired-marked ((t (:foreground ,ronny-bright-green :weight bold))))
 `(dired-flagged ((t (:foreground ,ronny-bright-red :weight bold))))
 `(dired-directory ((t (:foreground ,ronny-bright-cyan :weight bold))))
 ;; diredfl-mode
 `(diredfl-dir-heading ((t (:inherit dired-directory))))
 `(diredfl-dir-name ((t (:inherit dired-directory))))
 `(diredfl-file-name ((t (:foreground ,ronny-bright-white))))
 `(diredfl-symlink ((t (:foreground ,ronny-violet :slant italic))))
 `(diredfl-flag-mark ((t (:foreground "#e2c770"  :background "#433f2f"))))
 `(diredfl-flag-mark-line ((t (:background ,ronny-black3))))
 `(diredfl-deletion ((t (:foreground "#e74c3c" :background "#442724"))))
 `(diredfl-deletion-file-name ((t (:foreground "#e74c3c" :background "#442724"))))
 `(diredfl-file-suffix ((t (:inherit shadow))))
 ;; dired-subtree
 `(dired-subtree-depth-1-face ((t (:background ,ronny-black2))))
 `(dired-subtree-depth-2-face ((t (:background ,ronny-black3))))

 ;; Which-key
 `(which-key-key-face ((t (:foreground ,ronny-bright-green :weight bold))))
 `(which-key-command-description-face ((t (:foreground ,ronny-yellow))))

 ;; Eglot/Flymake (LSP)
 `(flymake-error ((t (:underline (:style wave :color ,ronny-bright-red)))))
 `(flymake-warning ((t (:underline (:style wave :color ,ronny-bright-yellow)))))
 `(flymake-note ((t (:underline (:style wave :color ,ronny-bright-black)))))
 `(eglot-mode-line ((t  (:foreground ,ronny-yellow))))

 ;; TODO: Dape (debugging)

 ;; ansi-color <- term <- vterm
 `(ansi-color-black ((t (:foreground ,ronny-black :background ,ronny-black))))
 `(ansi-color-bright-black ((t (:foreground ,ronny-bright-black :background ,ronny-bright-black))))
 `(ansi-color-red ((t (:foreground ,ronny-red :background ,ronny-red))))
 `(ansi-color-bright-red ((t (:foreground ,ronny-bright-red :background ,ronny-bright-red))))
 `(ansi-color-green ((t (:foreground ,ronny-green :background ,ronny-green))))
 `(ansi-color-bright-green ((t (:foreground ,ronny-bright-green :background ,ronny-bright-green))))
 `(ansi-color-yellow ((t (:foreground ,ronny-yellow :background ,ronny-yellow))))
 `(ansi-color-bright-yellow ((t (:foreground ,ronny-bright-yellow :background ,ronny-bright-yellow))))
 `(ansi-color-blue ((t (:foreground ,ronny-blue :background ,ronny-blue))))
 `(ansi-color-bright-blue ((t (:foreground ,ronny-bright-blue :background ,ronny-bright-blue))))
 `(ansi-color-magenta ((t (:foreground ,ronny-purple :background ,ronny-purple))))
 `(ansi-color-bright-magenta ((t (:foreground ,ronny-bright-purple :background ,ronny-bright-purple))))
 `(ansi-color-cyan ((t (:foreground ,ronny-cyan :background ,ronny-cyan))))
 `(ansi-color-bright-cyan ((t (:foreground ,ronny-bright-cyan :background ,ronny-bright-cyan))))
 `(ansi-color-white ((t (:foreground ,ronny-white :background ,ronny-white))))
 `(ansi-color-bright-white ((t (:foreground ,ronny-bright-white :background ,ronny-bright-white))))

 ;; Vertico/Consult/Corfu
 `(vertico-current ((t (:background ,ronny-black3))))
 `(consult-line-number ((t (:foreground ,ronny-gray))))
 `(corfu-default ((t (:inherit tooltip))))
 `(corfu-current ((t (:foreground "#d6d6d4" :background ,ronny-black3))))
 `(corfu-bar ((t (:background "#a8a8a8"))))
 `(corfu-border ((t (:background ,ronny-black3))))

 ;; TODO: Avy/Ace-window (navigation)

 ;; TODO:
 ;; - diff-added, diff-removed, ...
 ;; magit
 `(magit-branch-remote ((t (:foreground ,ronny-green))))
 `(magit-branch-local ((t (:foreground ,ronny-bright-cyan))))
 `(magit-section-heading ((t (:foreground ,ronny-yellow :weight bold))))
 `(magit-diff-removed ((t (:foreground ,ronny-red :background "#2D1418"))))
 `(magit-diff-added ((t (:foreground ,ronny-green :background "#1C2610"))))
 `(magit-diff-removed-highlight ((t (:inherit magit-diff-removed))))
 `(magit-diff-added-highlight ((t (:inherit magit-diff-added))))
 `(magit-section-highlight ((t (:background ,ronny-black3))))
 `(magit-diff-context-highlight ((t (:background ,ronny-black3))))
 `(magit-diffstat-added ((t (:foreground ,ronny-green))))
 `(magit-diffstat-removed ((t (:foreground ,ronny-red))))
 ;; diff-hl
 `(diff-hl-delete ((t (:foreground ,ronny-bright-red :background ,ronny-bright-red))))
 `(diff-hl-change ((t (:foreground ,ronny-bright-black :background ,ronny-bright-black))))
 `(diff-hl-insert ((t (:foreground ,ronny-green :background ,ronny-green))))

 ;; Jinx (spell checking)
 `(jinx-misspelled ((t (:underline (:style wave :color ,ronny-bright-yellow))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ronny)

;;; ronny-theme.el ends here
