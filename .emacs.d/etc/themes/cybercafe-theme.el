;;; cybercafe-theme.el --- Cybercafe color theme  -*- lexical-binding:t -*-

;; Copyright (C) 2023  Gabriel de Brito

;; Author: Gabriel de Brito gabrielgbrito@icloud.com
;; URL: http://github.com/gboncoffee/cybercafe-emacs-theme
;; Keywords: faces
;; Package-Requires: ((emacs "24.1"))
;; Version: 0.3.3

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:
;;
;; Cybercafe color theme for Emacs.  A white-on-black, almost-colorless theme.

;;; Code:

(deftheme cybercafe
  "Cybercafe: almost colorless, white-on-black theme.")

(defgroup cybercafe nil
  "A white-on-black, almost-colorless theme."
  :prefix "cybercafe-"
  :link '(url-link :tag "Source code"
                   "https://github.com/gboncoffee/cybercafe-emacs-theme")
  :group 'faces)

(defcustom cybercafe-cyberpunk nil
  "If t, uses more green in UI."
  :type 'boolean
  :group 'cybercafe)

(defcustom cybercafe-soft nil
  "If t, Uses a slightly lighter background."
  :type 'boolean
  :group 'cybercafe)

(let ((cybercafe-fg (if cybercafe-soft "#f8f8f8" "#f0f0f0"))
      (cybercafe-alt-fg "#bbbbbb")
      (cybercafe-bg (if cybercafe-soft "#202020" "#000000"))
      (cybercafe-alt-bg "#444444")
      (cybercafe-red "#ee0000")
      (cybercafe-green "#00ee00")
      (cybercafe-blue "#5c5cff")
      (cybercafe-yellow "#eeee00")
      (has-doom-modeline (bound-and-true-p doom-modeline-mode)))
  (custom-theme-set-variables
   'cybercafe
   '(frame-background-mode (quote dark)))
  (custom-theme-set-faces
   'cybercafe

   ;; general
   `(cursor ((t (:background ,cybercafe-fg :foreground ,cybercafe-bg))))
   `(default ((t (:background ,cybercafe-bg :foreground ,cybercafe-fg))))
   `(error ((t :background ,cybercafe-red :foreground ,cybercafe-bg)))
   `(warning ((t :background ,cybercafe-yellow :foreground ,cybercafe-bg)))
   `(match
     ((t :background ,(if cybercafe-cyberpunk cybercafe-green cybercafe-yellow)
	 :foreground ,cybercafe-bg)))
   `(region ((t :background ,cybercafe-alt-bg)))
   '(bold ((t :bold t)))
   '(italic ((t (:slant italic))))
   '(bold-italic ((t (:slant italic :weight bold))))
   `(fringe ((t :background ,cybercafe-bg)))
   `(line-number
     ((t :background ,cybercafe-bg :foreground ,cybercafe-alt-fg)))
   `(line-number-current-line
     ((t :background ,cybercafe-bg
	 :foreground ,cybercafe-fg
	 :bold t
	 :italic t)))
   `(minibuffer-prompt ((t :foreground ,cybercafe-fg)))
   `(highlight
     ((t :background ,(if cybercafe-cyberpunk cybercafe-green cybercafe-yellow)
	 :foreground ,cybercafe-bg)))
   `(isearch ((t (:background ,cybercafe-yellow :foreground ,cybercafe-bg))))
   `(lazy-highlight
     ((t (:background ,cybercafe-green :foreground ,cybercafe-bg))))
   `(trailing-whitespace ((t (:background ,cybercafe-red))))
   `(link ((t (:foreground ,cybercafe-blue :underline t))))
   `(link-visited ((t (:foreground ,cybercafe-blue :underline t :bold t))))

   ;; font-lock
   `(font-lock-comment-face ((t (:foreground ,cybercafe-alt-fg :italic t))))
   `(font-lock-constant-face ((t (:foreground ,cybercafe-fg))))
   `(font-lock-function-name-face ((t (:foreground ,cybercafe-fg))))
   `(font-lock-keyword-face ((t (:foreground ,cybercafe-fg))))
   `(font-lock-negation-char-face ((t (:foreground ,cybercafe-fg))))
   `(font-lock-string-face ((t (:foreground ,cybercafe-alt-fg))))
   `(font-lock-type-face ((t (:foreground ,cybercafe-fg))))
   `(font-lock-variable-name-face ((t (:foreground ,cybercafe-fg))))
   `(font-lock-warning-face ((t (:foreground ,cybercafe-yellow))))
   `(font-lock-preprocessor-face ((t (:foreground ,cybercafe-fg))))
   `(font-lock-builtin-face ((t (:foreground ,cybercafe-fg))))

   ;; mode line
   `(mode-line
     ((t :background
	 ,(if has-doom-modeline
	      cybercafe-alt-bg
	    (if cybercafe-cyberpunk cybercafe-green cybercafe-alt-fg))
	 :foreground
	 ,(if has-doom-modeline
	      cybercafe-fg
	    cybercafe-bg))))
   `(mode-line-highlight
     ((t :background
	 ,(if has-doom-modeline
	      cybercafe-alt-bg
	    (if cybercafe-cyberpunk cybercafe-green cybercafe-alt-fg))
	 :foreground
	 ,(if has-doom-modeline
	      cybercafe-fg
	    cybercafe-bg))))
   `(mode-line-inactive
     ((t :background
	 ,(if
	      (or cybercafe-cyberpunk
		  has-doom-modeline)
	      cybercafe-bg
	    cybercafe-alt-bg)
	 :foreground ,cybercafe-alt-fg)))
   `(doom-modeline-bar
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-fg)))
   `(doom-modeline-buffer-file
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-fg :bold t)))
   `(doom-modeline-buffer-major-mode
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-fg :bold t)))
   `(doom-modeline-buffer-minor-mode
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-alt-fg)))
   `(doom-modeline-buffer-modified
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-fg :bold t)))
   `(doom-modeline-buffer-path
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-alt-fg)))
   `(doom-modeline-buffer-project-root
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-alt-fg)))
   `(doom-modeline-debug
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-yellow)))
   `(doom-modeline-evil-insert-state
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-yellow)))
   `(doom-modeline-evil-visual-state
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-green)))
   `(doom-modeline-info
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-green)))
   `(doom-modeline-lsp-success
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-green)))
   `(doom-modeline-persp-name
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-alt-fg)))
   `(doom-modeline-project-dir
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-alt-fg)))
   `(doom-modeline-project-parent-dir
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-alt-fg)))
   `(doom-modeline-urgent
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-red)))
   `(doom-modeline-warning
     ((t :background ,cybercafe-alt-bg :foreground ,cybercafe-yellow)))

   ;; show paren
   `(show-paren-mismatch
     ((t :background ,(if cybercafe-cyberpunk cybercafe-green cybercafe-alt-fg)
	 :foreground ,cybercafe-bg)))
   `(show-paren-match
     ((t :background ,(if cybercafe-cyberpunk cybercafe-green cybercafe-alt-fg)
	 :foreground ,cybercafe-bg)))

   ;; ido
   `(ido-first-match
     ((t :foreground ,(if cybercafe-cyberpunk cybercafe-green cybercafe-yellow)
	 :background ,cybercafe-bg :bold t)))
   `(ido-only-match
     ((t :foreground ,(if cybercafe-cyberpunk cybercafe-green cybercafe-yellow)
	 :background ,cybercafe-bg :bold t)))
   `(ido-subdir
     ((t (:foreground ,cybercafe-blue :background ,cybercafe-bg :bold t))))

   ;; dired
   `(dired-directory ((t (:foreground ,cybercafe-blue :bold t))))
   `(dired-symlink ((t :foreground ,cybercafe-blue :bold nil :underline t)))
   `(dired-ignored ((t :foreground ,cybercafe-alt-fg)))
   `(dired-marked
     ((t (:background ,cybercafe-yellow :foreground ,cybercafe-bg :bold t))))
   `(dired-broken-symlink
     ((t (:background ,cybercafe-red :foreground ,cybercafe-yellow))))

   ;; diff
   `(diff-added
     ((t (:background ,cybercafe-green :foreground ,cybercafe-bg))))
   `(diff-changed
     ((t (:background ,cybercafe-yellow :foreground ,cybercafe-bg))))
   `(diff-removed
     ((t (:background ,cybercafe-red :foreground ,cybercafe-bg))))
   `(diff-error
     ((t (:background ,cybercafe-red :foreground ,cybercafe-bg))))
   `(diff-file-header
     ((t (:background ,cybercafe-bg :foreground ,cybercafe-fg))))
   `(diff-header
     ((t (:background ,cybercafe-bg :foreground ,cybercafe-fg))))
   `(diff-hunk-header
     ((t (:background ,cybercafe-alt-bg :foreground ,cybercafe-fg))))
   `(diff-index
     ((t (:background ,cybercafe-alt-bg :foreground ,cybercafe-fg))))

   ;; magit
   `(magit-diff-added-highlight
     ((t (:background ,cybercafe-green :foreground ,cybercafe-bg))))
   `(magit-diff-changed-highlight
     ((t (:background ,cybercafe-yellow :foreground ,cybercafe-bg))))
   `(magit-diff-removed-highlight
     ((t (:background ,cybercafe-red :foreground ,cybercafe-bg))))
   `(magit-diff-error-highlight
     ((t (:background ,cybercafe-red :foreground ,cybercafe-bg))))
   `(magit-diff-file-header-highlight
     ((t (:background ,cybercafe-bg :foreground ,cybercafe-fg))))
   `(magit-diff-header-highlight
     ((t (:background ,cybercafe-bg :foreground ,cybercafe-fg))))
   `(magit-diff-hunk-header-highlight
     ((t (:background ,cybercafe-alt-bg :foreground ,cybercafe-fg))))
   `(magit-diff-index-highlight
     ((t (:background ,cybercafe-alt-bg :foreground ,cybercafe-fg))))

   `(magit-diff-added
     ((t (:background ,cybercafe-green :foreground ,cybercafe-bg))))
   `(magit-diff-changed
     ((t (:background ,cybercafe-yellow :foreground ,cybercafe-bg))))
   `(magit-diff-removed
     ((t (:background ,cybercafe-red :foreground ,cybercafe-bg))))
   `(magit-diff-error
     ((t (:background ,cybercafe-red :foreground ,cybercafe-bg))))
   `(magit-diff-file-header
     ((t (:background ,cybercafe-bg :foreground ,cybercafe-fg))))
   `(magit-diff-header
     ((t (:background ,cybercafe-bg :foreground ,cybercafe-fg))))
   `(magit-diff-hunk-header
     ((t (:background ,cybercafe-alt-bg :foreground ,cybercafe-fg))))
   `(magit-diff-index
     ((t (:background ,cybercafe-alt-bg :foreground ,cybercafe-fg))))

   `(magit-section-highlight ((t (:background ,cybercafe-alt-bg))))
   `(magit-branch-current ((t (:foreground ,cybercafe-alt-fg :bold t))))
   `(magit-branch-local ((t (:foreground ,cybercafe-alt-fg))))
   `(magit-branch-remote ((t (:foreground ,cybercafe-alt-fg))))
   `(magit-branch-remote-head ((t (:foreground ,cybercafe-alt-fg))))
   `(magit-branch-upstream ((t (:foreground ,cybercafe-alt-fg))))
   `(magit-branch-warning
     ((t :foreground
	 ,(if cybercafe-cyberpunk cybercafe-green cybercafe-yellow))))
   `(magit-section-heading
     ((t :foreground
	 ,(if cybercafe-cyberpunk cybercafe-green cybercafe-yellow))))

   ;; org
   `(org-level-1 ((t (:inherit outline-1 :extend nil :height 1.3))))
   `(org-level-2 ((t (:inherit outline-1 :extend nil :height 1.2))))
   `(org-level-3 ((t (:inherit outline-1 :extend nil :height 1.1))))
   `(org-done ((t (:foreground ,cybercafe-green))))
   `(org-todo ((t (:foreground ,cybercafe-yellow))))
   `(org-date ((t (:foreground ,cybercafe-blue))))

   ;; markdown
   `(markdown-code-face ((t (:foreground ,cybercafe-alt-fg))))

   ;; tuareg
   `(tuareg-font-double-semicolon-face ((t (:foreground ,cybercafe-fg))))
   `(tuareg-font-lock-error-face
     ((t (:background ,cybercafe-red :foreground ,cybercafe-bg))))
   `(tuareg-font-lock-extension-node-face ((t (:foreground ,cybercafe-fg))))
   `(tuareg-font-lock-governing-face ((t (:foreground ,cybercafe-fg :bold t))))
   `(tuareg-font-lock-multistage-face
     ((t (:background ,cybercafe-alt-bg :foreground ,cybercafe-fg))))
   `(tuareg-font-lock-operator-face ((t (:foreground ,cybercafe-fg :bold t))))

   ;; latex
   `(font-latex-bold-face ((t (:foreground ,cybercafe-fg :bold t))))
   `(font-latex-italic-face ((t (:foreground ,cybercafe-fg :italic t))))
   `(font-latex-math-face ((t (:foreground ,cybercafe-green))))
   `(font-latex-script-char-face ((t (:foreground ,cybercafe-alt-fg))))
   `(font-latex-sectioning-0-face ((t (:foreground ,cybercafe-fg :bold t))))
   `(font-latex-sectioning-1-face ((t (:foreground ,cybercafe-fg :bold t))))
   `(font-latex-sectioning-2-face ((t (:foreground ,cybercafe-fg :bold t))))
   `(font-latex-sectioning-3-face ((t (:foreground ,cybercafe-fg :bold t))))
   `(font-latex-sectioning-4-face ((t (:foreground ,cybercafe-fg :bold t))))
   `(font-latex-sectioning-5-face ((t (:foreground ,cybercafe-fg :bold t))))
   `(font-latex-string-face ((t (:foreground ,cybercafe-fg :italic t))))
   `(font-latex-underline-face ((t (:foreground ,cybercafe-fg :underline t))))
   `(font-latex-verbatim-face ((t (:foreground ,cybercafe-fg :italic t))))
   `(font-latex-warning-face ((t (:foreground ,cybercafe-yellow))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'cybercafe)

;;; cybercafe-theme.el ends here.
