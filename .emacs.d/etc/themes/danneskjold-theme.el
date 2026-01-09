;;; danneskjold-theme.el --- beautiful high-contrast theme  -*- lexical-binding: t; -*-

;; Copyright (c) 2016-2026 Dmitry Akatov
;; Author: Dmitry Akatov <akatovda@google.com>
;; URL: https://github.com/rails-to-cosmos/danneskjold-theme
;; Package-Version: 1.0.1

;;; Commentary:
;; High-contrast minimalistic theme with dark and light variants.

;;; Code:

(deftheme danneskjold "High-contrast minimalistic theme. Dark variant.")
(deftheme danneskjold-light "High-contrast minimalistic theme. Light variant.")

;;;###autoload
(defun danneskjold-toggle-theme ()
  "Toggle between light and dark danneskjold themes."
  (interactive)
  (if (custom-theme-enabled-p 'danneskjold-light)
      (progn
        (disable-theme 'danneskjold-light)
        (enable-theme 'danneskjold))
    (disable-theme 'danneskjold)
    (enable-theme 'danneskjold-light)))

(defun danneskjold-invert-color (color)
  "Invert a hex COLOR string using Emacs' built-in color manipulation."
  (let ((rgb (color-name-to-rgb color)))
    (if rgb
        (apply #'color-rgb-to-hex
               (mapcar (lambda (c) (- 1.0 c)) rgb))
      (error "Invalid color: %s" color))))

;; Define shared variables for the dark theme
(let* ((class '((class color) (min-colors 89)))
       ;; Core Palette
       (background "#000000")
       (foreground "#FFFFFF")
       (black      "#000000")
       (white      "#FFFFFF")
       (grey       "#C0C5CF")
       (grey-1     "#525254")
       (grey-2     "#39393D")

       ;; Accent Palette
       (yellow     "#ffcc00")
       (yellow-c   "#281580")
       (orange     "#ffa500")
       (red        "#E74C3C")
       (magenta    "#F92672")
       (violet     "#7b68ee")
       (blue       "#4CB5F5")
       (cyan       "#66D9EF")
       (green      "#B6E63E")
       (green-3    "#86B20E")
       (dark-cyan  "#8FA1B3")

       ;; Specialized Colors
       (frost      "#D0E1F9")
       (invisible  "#2b4b6e")
       (comment    "#A4C2EB")
       (waddles    "#FF87BA")
       (diredcl    "#749AF7")
       (ada-midnight "#21252b")
       (sbt-midnight "#282c34")

       ;; Component Mappings
       (green-bright "#2ecc71")
       (green-dark   "#27ae60")
       (blue-bright  "#3498db")
       (blue-dark    "#2980b9")
       (red-bright   "#e74c3c")
       (red-dark     "#c0392b")
       (orange-bright "#e67e22")
       (yellow-bright "#f1c40f")
       (yellow-dark   "#f39c12")
       (magenta-bright "#9b59b6")
       (magenta-dark   "#8e44ad")
       (grey-bright1 "#bdc3c7")
       (grey-bright2 "#ecf0f1")
       (grey-dark1   "#95a5a6")
       (grey-dark2   "#7f8c8d"))

  ;; --- Dark Theme Face Definitions ---
  (custom-theme-set-faces
   'danneskjold
   `(default ((,class (:foreground ,foreground :background ,black))))
   `(button ((,class (:foreground ,frost :underline t :weight normal))))
   `(comint-highlight-prompt ((,class (:foreground ,cyan))))
   `(company-tooltip ((,class (:foreground ,white :background ,ada-midnight))))
   `(company-tooltip-selection ((,class (:foreground ,white :background ,sbt-midnight))))
   `(company-tooltip-common ((,class (:foreground ,yellow))))
   `(compilation-error ((,class (:foreground ,red))))
   `(compilation-info ((,class (:foreground ,diredcl))))
   `(compilation-line-number ((,class (:foreground ,green))))
   `(diff-added ((,class (:foreground ,green-dark))))
   `(diff-removed ((,class (:foreground ,yellow-dark))))
   `(dired-directory ((,class (:foreground ,blue))))
   `(dired-flagged ((,class (:foreground ,red :underline t))))
   `(dired-ignored ((,class (:foreground ,invisible))))
   `(diredp-deletion ((,class (:foreground ,black :background ,red))))
   `(diredp-dir-name ((,class (:foreground "DeepSkyBlue1"))))
   `(diredp-file-name ((,class (:foreground ,foreground))))
   `(font-lock-builtin-face ((,class (:foreground ,blue))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,yellow))))
   `(font-lock-function-name-face ((,class (:foreground ,yellow))))
   `(font-lock-keyword-face ((,class (:foreground ,frost))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,orange))))
   `(font-lock-variable-name-face ((,class (:foreground ,foreground))))
   `(fringe ((,class (:foreground ,invisible))))
   `(header-line ((,class (:background ,black :foreground ,comment :underline ,comment))))
   `(highlight ((,class (:background ,invisible :foreground ,black))))
   `(hl-line ((,class (:background "#1F1F1F"))))
   `(isearch ((,class (:foreground ,black :background ,invisible))))
   `(lazy-highlight ((,class (:foreground ,black :background ,yellow))))
   `(line-number ((,class (:foreground ,invisible))))
   `(line-number-current-line ((,class (:foreground ,foreground))))
   `(link ((,class (:foreground ,frost :underline t))))
   `(magit-branch-local ((,class (:foreground "#1abc9c"))))
   `(minibuffer-prompt ((,class (:foreground ,diredcl))))
   `(mode-line ((,class (:background ,black :foreground ,comment))))
   `(mode-line-inactive ((,class (:background ,black :foreground ,invisible))))
   `(org-done ((,class (:foreground ,green))))
   `(org-todo ((,class (:foreground ,yellow))))
   `(org-table ((,class (:foreground ,yellow))))
   `(org-level-1 ((,class (:foreground ,foreground :bold t))))
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,red))))
   `(region ((,class (:background "#373d4f"))))
   `(show-paren-match ((,class (:background ,blue :foreground ,foreground))))
   `(vertical-border ((,class (:foreground "#223959"))))
   `(wgrep-file-face ((,class (:background ,(danneskjold-invert-color "#281580") :foreground ,yellow))))
   `(whitespace-space ((,class (:foreground ,ada-midnight))))
   `(whitespace-newline ((,class (:foreground ,ada-midnight))))

   `(org-priority ((,class (:foreground ,foreground)))) ;; Default
   `(org-priority-highest ((,class (:foreground ,red :bold t))))    ; [#A]
   `(org-priority-medium ((,class (:foreground ,yellow))))          ; [#B]
   `(org-priority-lowest ((,class (:foreground ,green-dark))))      ; [#C]
   `(org-ellipsis ((,class (:inherit hs-face :underline unspecified))))

   `(vertico-current ((,class (:foreground "#FFCA00")))))

  ;; --- Light Theme Face Definitions ---
  (custom-theme-set-faces
   'danneskjold-light
   `(default ((t (:foreground "black" :background "white"))))
   `(org-block ((t (:extend t :background "ghost white"))))
   `(org-upcoming-deadline ((t (:foreground "purple3"))))
   `(company-tooltip-selection ((t (:background "#ffd600"))))
   `(diredp-date-time ((t (:foreground "goldenrod1"))))
   `(diredp-deletion ((t (:foreground ,red :weight bold :slant italic))))
   `(diredp-file-name ((t (:foreground "black"))))
   `(font-lock-comment-face ((t (:foreground "#7f8c8d"))))
   `(highlight ((t (:background "honeydew" :extend t))))
   `(hl-line ((t (:background "gray98"))))
   `(ivy-current-match ((t (:weight bold :background "#ffd600"))))
   `(line-number ((t (:foreground "grey"))))
   `(line-number-current-line ((t (:foreground ,diredcl))))
   `(mode-line-buffer-read-only-face ((t (:foreground "brown4"))))
   `(org-agenda-current-time ((t (:foreground "black" :weight bold))))
   `(org-block-begin-line ((t (:foreground "black" :weight bold :background "white"))))
   `(org-ellipsis ((t (:underline unspecified))))
   `(tooltip ((t (:foreground "black" :background "white"))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'danneskjold)
;;; danneskjold-theme.el ends here
