;;; wilson-ng-theme.el --- A modernized version of the Wilson theme -*- lexical-binding: t -*-

;; Author: Levin Düsterhus <levin@duesterhus.net>
;; URL: https://github.com/levindue/emacs-wilson-theme
;; License: GPL-3.0
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))

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
;; This is a modified version of the "Wilson" theme
;; originally from the sublime-themes package by Owain Lewis.
;; Modifications include an expanded color palette, face remapping
;; and modern plugin support.

;;; Code:

(deftheme wilson-ng
  "A modernized version of the Wilson theme based upon a dirty spitfire.")

(let ((wilson-oilstained-eggshell   "#6C6B59")
      (wilson-flying-boots          "#44443C")
      (wilson-darker-flying-boots   "#222222")
      (wilson-spring-grass          "#9BA657")
      (wilson-stained-white         "#BEBFB7")
      (wilson-darker-stained-white  "#A9AAA3")
      (wilson-gray                  "#84857E")
      (wilson-darker-gray           "gray30")
      (wilson-dark-gray             "gray25")
      (wilson-light-gray            "gray20")
      (wilson-lighter-gray          "gray12")
      (wilson-stained-orange        "#B97E56")
      (wilson-darker-stained-orange "#A56F4B")
      (wilson-stained-yellow        "#CFB980")
      (wilson-darker-stained-yellow "#B9A572")
      (wilson-stained-blue           "#5F7285")
      (wilson-darker-stained-blue    "#495767")
      (wilson-lighter-stained-blue   "#7A8B9B"))

  (custom-theme-set-faces
   'wilson-ng

   ;; ----------------- Frame stuff --------------------
   `(default ((t (:background ,wilson-darker-flying-boots :foreground ,wilson-stained-white))))
   `(cursor  ((t (:background ,wilson-stained-white))))
   `(hl-line ((t (:background ,wilson-flying-boots))))
   `(modeline ((t (:background ,wilson-spring-grass :foreground ,wilson-flying-boots))))
   `(mode-line-inactive ((t (:box nil :background ,wilson-light-gray :foreground ,wilson-stained-yellow))))
   `(mode-line ((t (:box nil :foreground ,wilson-stained-white :background ,wilson-flying-boots))))
   `(fringe ((t (:background ,wilson-darker-flying-boots))))
   `(minibuffer-prompt ((default (:foreground ,wilson-stained-orange))))
   `(linum ((t (:background ,wilson-darker-flying-boots :foreground, wilson-stained-white))))
   `(region ((t (:foreground ,wilson-darker-stained-white :background ,wilson-flying-boots))))

   ;; ---------------- Code Highlighting ---------------
   `(font-lock-builtin-face ((t (:foreground ,wilson-darker-stained-orange))))
   `(font-lock-constant-face ((t (:foreground ,wilson-stained-orange))))
   `(font-lock-comment-face ((t (:foreground ,wilson-oilstained-eggshell))))
   `(font-lock-function-name-face ((t (:foreground ,wilson-lighter-stained-blue))))
   `(font-lock-function-call-name-face ((t (:foreground ,wilson-lighter-stained-blue))))
   `(font-lock-keyword-face ((t (:foreground ,wilson-darker-stained-orange))))
   `(font-lock-string-face ((t (:foreground ,wilson-spring-grass))))
   `(font-lock-variable-name-face ((t (:foreground ,wilson-stained-white))))
   `(font-lock-type-face ((t (:foreground ,wilson-darker-stained-yellow))))
   `(font-lock-warning-face ((t (:foreground ,wilson-darker-stained-orange :bold t))))

   ;; ---------------- Magit --------------------
   `(magit-section-heading ((t (:foreground ,wilson-stained-orange :weight bold))))
   `(magit-branch-local ((t (:foreground ,wilson-stained-yellow))))
   `(magit-branch-remote ((t (:foreground ,wilson-spring-grass))))
   `(magit-diff-context ((t (:foreground ,wilson-gray))))
   `(magit-diff-context-highlight ((t (:background ,wilson-light-gray :foreground ,wilson-gray))))
   `(magit-diff-added ((t (:foreground ,wilson-spring-grass))))
   `(magit-diff-added-highlight ((t (:background ,wilson-light-gray :foreground ,wilson-spring-grass :weight bold))))
   `(magit-diff-removed ((t (:foreground ,wilson-stained-orange))))
   `(magit-diff-removed-highlight ((t (:background ,wilson-light-gray :foreground ,wilson-stained-orange :weight bold))))
   `(magit-diff-hunk-heading ((t (:background ,wilson-darker-gray :foreground ,wilson-stained-white))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,wilson-dark-gray :foreground ,wilson-stained-white :weight bold))))
   `(magit-log-author ((t (:foreground ,wilson-stained-yellow))))
   `(magit-log-date ((t (:foreground ,wilson-darker-gray))))
   `(magit-hash ((t (:foreground ,wilson-darker-stained-orange))))
   `(magit-process-ok ((t (:foreground ,wilson-spring-grass :weight bold))))
   `(magit-process-ng ((t (:foreground ,wilson-stained-orange :weight bold))))
   `(magit-blame-highlight ((t (:background ,wilson-light-gray))))
   `(magit-blame-heading ((t (:background ,wilson-light-gray))))

   ;; ---------------- Vertico --------------------
   `(vertico-current ((t (:foreground ,wilson-darker-stained-white :background ,wilson-flying-boots))))

   ;; ---------------- Markdown-mode --------------------
   `(markdown-link-face ((t (:foreground ,wilson-lighter-stained-blue))))))
   

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'wilson-ng)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; wilson-ng-theme.el ends here
