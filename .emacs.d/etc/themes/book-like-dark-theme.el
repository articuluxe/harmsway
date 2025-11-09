;;; book-like-dark-theme.el --- Book like grayscale dark theme -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Evgeny Simonenko

;; Author: Evgeny Simonenko <easimonenko@gmail.com>
;; Keywords: themes
;; Version: 0.1.2
;; Package-Requires: ((emacs "26.1"))
;; Created: August 2025
;; URL: https://github.com/easimonenko/book-like-themes
;; Repository: https://github.com/easimonenko/book-like-themes

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Book like grayscale dark theme.

;;; Code:

(deftheme book-like-dark
  "Book like grayscale dark theme.")

(custom-theme-set-faces
 'book-like-dark
 '(default ((t (:width normal :weight regular :slant normal :underline nil :overline nil :extend nil :strike-through nil :box nil :inverse-video nil :foreground "white" :background "black" :stipple nil :inherit nil))))

 '(agda2-highlight-datatype-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-error-face ((t (:underline t))))
 '(agda2-highlight-function-face ((t (:inherit font-lock-function-name-face))))
 '(agda2-highlight-inductive-constructor-face ((t (:inherit font-lock-function-name-face :slant oblique))))
 '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(agda2-highlight-module-face ((t (:foreground "white smoke" :weight bold))))
 '(agda2-highlight-number-face ((t (:inherit font-lock-number-face))))
 '(agda2-highlight-primitive-type-face ((t (:inherit font-lock-type-face :weight normal))))

 '(company-tooltip ((t (:background  "dim gray" :foreground "white"))))
 '(company-tooltip-annotation ((t (:foreground "gainsboro"))))
 '(company-tooltip-common ((t (:weight bold))))
 '(company-tooltip-scrollbar-thumb ((t (:background "white"))))
 '(company-tooltip-scrollbar-track ((t (:background "gray"))))
 '(company-tooltip-selection ((t (:background "gray"))))

 '(completions-annotations ((t (:inherit shadow))))
 '(completions-common-part ((t (:weight bold))))

 '(cursor ((((background light)) (:background "white smoke")) (((background dark)) (:background "dim gray"))))

 '(custom-group-tag ((t (:inherit variable-pitch :weight bold))))
 '(custom-invalid ((t (:inherit error :background "dim gray" :underline (:color foreground-color :style wave :position nil)))))
 '(custom-state ((t (:foreground "white smoke"))))
 '(custom-variable-tag ((t (:foreground "white smoke" :weight bold))))

 '(elisp-shorthand-font-lock-face ((t (:inherit font-lock-keyword-face))))

 '(error ((t (:weight bold))))

 '(flymake-error ((t (:underline (:color "white" :style wave :position nil)))))
 '(flymake-note ((t (:underline (:color "dark gray" :style wave :position nil)))))
 '(flymake-warning ((t (:underline (:color "gainsboro" :style wave :position nil)))))

 '(font-lock-bracket-face ((t (:foreground "gainsboro"))))
 '(font-lock-builtin-face ((t (:foreground "gainsboro" :weight bold))))
 '(font-lock-comment-face ((t (:extend t :background "dim gray" :foreground "white smoke"))))
 '(font-lock-constant-face ((t (:foreground "white smoke"))))
 '(font-lock-function-name-face ((t (:foreground "white smoke"))))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(font-lock-string-face ((t (:foreground "light gray"))))
 '(font-lock-type-face ((t (:foreground "gainsboro" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "white smoke"))))
 '(font-lock-warning-face ((t (:underline (:color "gainsboro" :style wave :position nil)))))

 '(highlight ((t (:weight bold :inverse-video t))))
 '(isearch ((t (:inverse-video t))))
 '(isearch-fail ((t (:inverse-video t))))
 '(isearch-group-1 ((t (:inherit isearch))))
 '(isearch-group-2 ((t (:inherit isearch))))
 '(lazy-highlight ((t (:inherit highlight))))

 '(link ((t (:foreground "white smoke" :underline t))))
 '(link-visited ((t (:inherit link :foreground "gainsboro"))))

 '(magit-branch-local ((t (:foreground "light gray"))))
 '(magit-branch-remote ((t (:foreground "white smoke"))))
 '(magit-cherry-equivalent ((t (:foreground "gainsboro"))))
 '(magit-cherry-unmatched ((t (:foreground "white smoke"))))
 '(magit-diff-added ((t (:extend t :background "black" :foreground "white smoke"))))
 '(magit-diff-added-highlight ((t (:extend t :background "dim gray" :foreground "white"))))
 '(magit-diff-base ((t (:extend t :background "black" :foreground "gainsboro"))))
 '(magit-diff-base-highlight ((t (:extend t :background "white smoke" :foreground "dark gray"))))
 '(magit-diff-file-heading-selection ((t (:inherit magit-diff-file-heading-highlight :extend t))))
 '(magit-diff-removed ((t (:extend t))))
 '(magit-diff-removed-highlight ((t (:extend t))))
 '(magit-diffstat-added ((t nil)))
 '(magit-diffstat-removed ((t nil)))
 '(magit-log-author ((t nil)))
 '(magit-process-ng ((t (:inherit magit-section-heading))))
 '(magit-process-ok ((t (:inherit magit-section-heading))))
 '(magit-reflog-amend ((t nil)))
 '(magit-reflog-checkout ((t nil)))
 '(magit-reflog-cherry-pick ((t nil)))
 '(magit-reflog-commit ((t nil)))
 '(magit-reflog-merge ((t nil)))
 '(magit-reflog-other ((t nil)))
 '(magit-reflog-rebase ((t nil)))
 '(magit-reflog-remote ((t nil)))
 '(magit-reflog-reset ((t nil)))
 '(magit-section-heading ((t (:inherit outline-1))))
 '(magit-section-highlight ((t (:extend t :inverse-video t))))
 '(magit-sequence-drop ((t (:inherit magit-hash))))
 '(magit-sequence-head ((t (:inherit magit-hash))))
 '(magit-sequence-part ((t (:inherit magit-hash))))
 '(magit-sequence-stop ((t (:inherit magit-hash))))
 '(magit-signature-bad ((t (:foreground "black" :weight bold))))
 '(magit-signature-error ((t (:foreground "black"))))
 '(magit-signature-expired ((t (:foreground "dim gray"))))
 '(magit-signature-good ((t (:foreground "gray"))))
 '(magit-signature-revoked ((t (:background "gainsboro" :foreground "black"))))
 '(magit-signature-untrusted ((t (:background "white smoke" :foreground "dim gray"))))
 '(magit-tag ((t (:foreground "dim gray"))))

 '(markdown-highlighting-face ((t (:background "dim gray" :foreground "white"))))

 '(match ((t (:inverse-video t))))
 '(minibuffer-prompt ((t (:foreground "white smoke" :weight bold))))
 '(mode-line ((t (:background "light gray" :foreground "black" :box (:line-width (1 . -1) :style released-button)))))

 '(org-agenda-structure ((t (:background "gray" :foreground "gainsboro"))))
 '(org-document-info ((t (:background "gray" :foreground "white smoke" :extend t))))
 '(org-document-title ((t (:foreground "white smoke" :weight bold))))
 '(org-done ((t (:foreground "gainsboro" :weight bold))))
 '(org-drawer ((t (:foreground "light gray"))))
 '(org-ellipsis ((t (:foreground "light gray" :underline t))))
 '(org-footnote ((t (:foreground "gainsboro" :underline t))))
 '(org-formula ((t (:foreground "white smoke"))))
 '(org-headline-done ((t (:foreground "gainsboro"))))
 '(org-headline-todo ((t (:foreground "gainsboro"))))
 '(org-latex-and-related ((t (:foreground "white smoke"))))
 '(org-mode-line-clock-overrun ((t (:inherit mode-line :background "light gray" :inverse-video t))))
 '(org-roam-header-line ((t (:extend t :foreground "dim gray" :weight bold :extend t))))
 '(org-roam-preview-heading-selection ((t (:inherit org-roam-preview-heading-highlight :extend t :foreground "white"))))
 '(org-scheduled ((t (:foreground "gray"))))
 '(org-scheduled-previously ((t (:foreground "gray" :underline (:color foreground-color :style dashes :position nil)))))
 '(org-scheduled-today ((t (:foreground "dim gray"))))
 '(org-sexp-date ((t (:foreground "dim gray"))))
 '(org-table ((t (:foreground "black"))))
 '(org-time-grid ((t (:foreground "gray"))))
 '(org-todo ((t (:foreground "black" :underline (:color foreground-color :style dashes :position nil) :weight bold))))
 '(org-upcoming-deadline ((t (:foreground "dim gray" :underline (:color foreground-color :style dashes :position nil)))))

 '(outline-1 ((t (:inherit font-lock-function-name-face :weight bold))))
 '(outline-2 ((t (:inherit font-lock-variable-name-face :weight bold))))
 '(outline-3 ((t (:inherit font-lock-keyword-face :weight bold))))

 '(sh-heredoc ((t (:foreground "gainsboro"))))
 '(sh-quoted-exec ((t (:foreground "white smoke"))))

 '(region ((t (:extend t :inverse-video t))))
 '(shadow ((t (:background "dim gray" :foreground "white smoke"))))
 '(show-paren-match ((t (:inverse-video t))))
 '(show-paren-mismatch ((t (:inverse-video t))))
 '(success ((t (:foreground "gainsboro" :weight bold))))
 '(trailing-whitespace ((t (:inverse-video t))))
 '(warning ((t (:foreground "white smoke" :weight bold)))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'book-like-dark)
;;; book-like-dark-theme.el ends here
