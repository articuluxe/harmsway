;;; klere-theme.el --- A dark theme with lambent color highlights and incremental grays

;; Copyright (C) 2018,2020, Jean Libète

;; Author: Jean Libète <tomenzgg@mail.mayfirst.org>
;; Homepage: https://github.com/tomenzgg/emacs-klere-theme
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Started with emacs-theme-generator, https://github.com/mswift42/theme-creator.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; I haven't seen all themes out there, I'm sure, but it seems most users
;; prefer – what look like, to me – washed out colors. In some cases, some
;; themes have stated that was their point, that contrasting colors are
;; harder on the eyes. But call me a slave to the asthetic but I wanted a
;; dark theme with radiant colors that seem to shine against the dark
;; backgrounds; I dunno if I quite achieved that but it's the color scheme
;; I've found myself most liking so I'm pleased with it. Options are
;; great so everyone gets what they like so here's it for you to try.

;;; Code:

 (deftheme klere)
 (let ((class   '((class color) (min-colors 89)))
       (fg1                            "#FFFFFF")
       (fg2                            "#767676")
       (fg3                            "#e8e8e8")
       (fg4                            "#c2c2c2")
       (fg5                            "#d1d1d1")
       (fg6                            "#bbbbbb")
       (bg1                            "#000000")
       (bg2                            "#111217") ;; "#101010"
       (bg3                            "#181818")
       (bg4                            "#2B2B2B")
       (bg5                            "#393939")
       (key2                           "#3e89bb")
       (key3                           "#0f6d9e")
       (builtin                        "#F52749")
       (keyword                        "#007bb3")
       (const                          "#00d1e0")
       (comment                        "#484848") ;; #e01d1d
       (func                           "#622f7d") ;; #7B28E1 (brighter and pops more but too much?)
       (str                            "#1D8B15") ;; #34be34 (but darker and more green)
       (neon                           "#7BE128")
       (type                           "#eab700")
       (var                            "#e87400")
       (warning                        "#ff0000")
       (org-bg1                        "#1C1F24")
       (org-bg2                        "#23272E")
       (org-block-begin-fg             "#535D6E")
       (org-date-fg                    "#B4BBC8")
       (org-date-outline               "#5B6268")
       (org-green                      "#339443")
       (org-purple                     "#653394")
       (org-teal                       "#33948A")
       (silky-gray                     "#343A44")
       (magit-add                      "#22aa22") ;; #00cd00
       (magit-remove                   "#aa2222"))
   (custom-theme-set-faces
   'klere
        `(default                                          ((,class (:background ,bg2 :foreground ,fg1))))
        `(font-lock-builtin-face                           ((,class (:foreground ,builtin))))
        `(font-lock-comment-face                           ((,class (:foreground ,comment :slant italic))))
	`(font-lock-negation-char-face                     ((,class (:foreground ,const))))
	`(font-lock-reference-face                         ((,class (:foreground ,const))))
	`(font-lock-constant-face                          ((,class (:foreground ,const))))
        `(font-lock-doc-face                               ((,class (:foreground ,comment))))
        `(font-lock-function-name-face                     ((,class (:foreground ,func :weight bold))))
        `(font-lock-keyword-face                           ((,class (:bold ,class :foreground ,keyword))))
        `(font-lock-string-face                            ((,class (:foreground ,str))))
        `(font-lock-type-face                              ((,class (:foreground ,type ))))
        `(font-lock-variable-name-face                     ((,class (:foreground ,var))))
        `(font-lock-warning-face                           ((,class (:foreground ,warning :background ,bg3))))
        `(region                                           ((,class (:background ,fg1 :foreground ,bg1))))
        `(highlight                                        ((,class (:foreground ,fg5 :background ,bg4))))
	`(hl-line                                          ((,class (:background ,bg4))))
	`(fringe                                           ((,class (:background ,bg3 :foreground ,fg6))))
	`(cursor                                           ((,class (:background ,bg4))))
        `(show-paren-match                                 ((,class (:foreground ,type    :slant italic :weight bold))))
        `(show-paren-mismatch                              ((,class (:foreground ,warning :slant italic :weight bold))))
        `(isearch                                          ((,class (:bold t :foreground ,warning :background ,bg4))))
        `(mode-line                                        ((,class (:bold t :underline unspecified :foreground ,fg1 :background "#1a2127"))))
        `(mode-line-inactive                               ((,class (:foreground ,fg6 :background "#171724" :weight normal :slant italic))))
        `(mode-line-buffer-id                              ((,class (:bold t :foreground ,keyword :background unspecified))))
	`(mode-line-highlight                              ((,class (:foreground ,func :box nil :weight bold))))
        `(mode-line-emphasis                               ((,class (:foreground ,fg1))))
	`(vertical-border                                  ((,class (:foreground ,fg5))))
        `(minibuffer-prompt                                ((,class (:bold t :foreground ,keyword))))
        `(default-italic                                   ((,class (:italic t))))
	`(link                                             ((,class (:foreground ,const :underline t))))
        `(line-number                                      ((,class (:foreground ,fg2))))
        `(line-number-current-line                         ((,class (:foreground ,fg4))))
	`(org-code                                         ((,class (:foreground ,const :inherit fixed-pitch))))
	`(org-hide                                         ((,class (:foreground ,fg6))))
        `(org-level-1                                      ((,class (:foreground ,fg1        :height 1.5  :bold t))))
        `(org-level-2                                      ((,class (:foreground ,org-green  :height 1.2  :bold t))))
        `(org-level-3                                      ((,class (:foreground ,org-purple :height 1.05 :bold t))))
        `(org-level-4                                      ((,class (:foreground ,key2       :height 1.0  :bold t))))
        `(org-level-5                                      ((,class (:foreground ,org-teal   :height 0.9))))
        `(org-level-5                                      ((,class (:foreground ,org-teal))))
        `(org-level-6                                      ((,class (:foreground ,func))))
        `(org-level-7                                      ((,class (:foreground ,str))))
        `(org-level-8                                      ((,class (:foreground ,var))))
        `(org-ellipsis                                     ((,class ())))
        `(org-date                                         ((,class (:height 0.9 :foreground ,org-date-fg :background ,org-bg1
                                                                     :box (:line-width 1 :color ,org-date-outline) :inherit fixed-pitch))))
        `(org-footnote                                     ((,class (:underline t :foreground ,fg6))))
        `(org-link                                         ((,class (:underline t :foreground ,type ))))
        `(org-special-keyword                              ((,class (:foreground ,func :overline t :height 0.95 :width condensed))))
        `(org-block                                        ((,class (:foreground ,fg5 :background ,org-bg1))))
        `(org-quote                                        ((,class (:inherit org-block :slant italic))))
        `(org-verse                                        ((,class (:inherit org-block :slant italic))))
        `(org-block-begin-line                             ((,class (:foreground ,org-block-begin-fg :background ,org-bg2
                                                                     :box (:line-width (3 . 2) :color ,org-bg2) :inherit org-meta-line))))
        `(org-todo                                         ((,class (:width condensed :height 0.9 :foreground ,org-bg1 :background ,keyword
                                                                     :bold t   :box (:line-width 2 :color ,keyword)))))
        `(org-done                                         ((,class (:width condensed :height 0.9 :foreground ,fg1     :background ,bg5
                                                                     :bold nil :box (:line-width 2 :color ,bg5)))))
        `(org-headline-done                                ((,class (:foreground ,comment :strike-through t :slant italic))))
        `(org-warning                                      ((,class (:underline t :foreground ,warning))))
        `(org-agenda-structure                             ((,class (:weight bold :foreground ,fg5 :box (:color ,fg6) :background ,bg4))))
        `(org-agenda-date                                  ((,class (:foreground ,var :height 1.1 ))))
        `(org-agenda-date-weekend                          ((,class (:weight normal :foreground ,fg6))))
        `(org-agenda-date-today                            ((,class (:weight bold :foreground ,keyword :height 1.4))))
        `(org-agenda-done                                  ((,class (:foreground ,bg5))))
	`(org-scheduled                                    ((,class (:foreground ,type))))
        `(org-scheduled-today                              ((,class (:foreground ,func :weight bold :height 1.2))))
	`(org-ellipsis                                     ((,class (:foreground ,builtin))))
	`(org-verbatim                                     ((,class (:foreground ,fg6))))
        `(org-document-info-keyword                        ((,class (:foreground ,func))))
	`(font-latex-bold-face                             ((,class (:foreground ,type))))
	`(font-latex-italic-face                           ((,class (:foreground ,key3 :italic t))))
	`(font-latex-string-face                           ((,class (:foreground ,str))))
	`(font-latex-match-reference-keywords              ((,class (:foreground ,const))))
	`(font-latex-match-variable-keywords               ((,class (:foreground ,var))))
	`(ido-only-match                                   ((,class (:foreground ,warning))))
	`(org-sexp-date                                    ((,class (:foreground ,fg6))))
	`(ido-first-match                                  ((,class (:foreground ,keyword :bold t))))
	`(gnus-header-content                              ((,class (:foreground ,keyword))))
	`(gnus-header-from                                 ((,class (:foreground ,var))))
	`(gnus-header-name                                 ((,class (:foreground ,type))))
	`(gnus-header-subject                              ((,class (:foreground ,func :bold t))))
	`(mu4e-view-url-number-face                        ((,class (:foreground ,type))))
	`(mu4e-cited-1-face                                ((,class (:foreground ,fg3))))
	`(mu4e-cited-7-face                                ((,class (:foreground ,fg5))))
	`(mu4e-header-marks-face                           ((,class (:foreground ,type))))
	`(ffap                                             ((,class (:foreground ,fg6))))
	`(js2-private-function-call                        ((,class (:foreground ,const))))
	`(js2-jsdoc-html-tag-delimiter                     ((,class (:foreground ,str))))
	`(js2-jsdoc-html-tag-name                          ((,class (:foreground ,key2))))
	`(js2-external-variable                            ((,class (:foreground ,type  ))))
        `(js2-function-param                               ((,class (:foreground ,const))))
        `(js2-jsdoc-value                                  ((,class (:foreground ,str))))
        `(js2-private-member                               ((,class (:foreground ,fg5))))
        `(js3-warning-face                                 ((,class (:underline ,keyword))))
        `(js3-error-face                                   ((,class (:underline ,warning))))
        `(js3-external-variable-face                       ((,class (:foreground ,var))))
        `(js3-function-param-face                          ((,class (:foreground ,key3))))
        `(js3-jsdoc-tag-face                               ((,class (:foreground ,keyword))))
        `(js3-instance-member-face                         ((,class (:foreground ,const))))
        `(warning                                          ((,class (:foreground ,warning))))
	`(ac-completion-face                               ((,class (:underline t :foreground ,keyword))))
	`(info-quoted-name                                 ((,class (:foreground ,builtin))))
	`(info-string                                      ((,class (:foreground ,str))))
	`(icompletep-determined                            ((,class :foreground ,builtin)))
        `(undo-tree-visualizer-current-face                ((,class :foreground ,builtin)))
        `(undo-tree-visualizer-default-face                ((,class :foreground ,fg3)))
        `(undo-tree-visualizer-unmodified-face             ((,class :foreground ,var)))
        `(undo-tree-visualizer-register-face               ((,class :foreground ,type)))
	`(slime-repl-inputed-output-face                   ((,class (:foreground ,type))))
        `(trailing-whitespace                              ((,class :foreground unspecified :background ,warning)))
        `(rainbow-delimiters-depth-1-face                  ((,class :foreground ,fg1)))
        `(rainbow-delimiters-depth-2-face                  ((,class :foreground ,type)))
        `(rainbow-delimiters-depth-3-face                  ((,class :foreground ,var)))
        `(rainbow-delimiters-depth-4-face                  ((,class :foreground ,const)))
        `(rainbow-delimiters-depth-5-face                  ((,class :foreground ,keyword)))
        `(rainbow-delimiters-depth-6-face                  ((,class :foreground ,fg1)))
        `(rainbow-delimiters-depth-7-face                  ((,class :foreground ,type)))
        `(rainbow-delimiters-depth-8-face                  ((,class :foreground ,var)))
        `(magit-item-highlight                             ((,class :background ,bg4)))
        `(magit-section-heading                            ((,class (:foreground ,keyword :weight bold))))
        `(magit-hunk-heading                               ((,class (:background ,bg4))))
        `(magit-section-highlight                          ((,class (:background ,bg4))))
        `(magit-hunk-heading-highlight                     ((,class (:background ,bg4))))
        `(magit-diff-added                                 ((,class (:foreground "#DDFFDD" :background ,org-green))))
        `(magit-diff-added-highlight                       ((,class (:foreground "#CCEECC" :background ,magit-add))))
        `(magit-diff-removed                               ((,class (:foreground "#FFDDDD" :background ,magit-remove))))
        `(magit-diff-removed-highlight                     ((,class (:foreground "#EECCCC" :background ,magit-remove))))
        `(magit-diff-context-highlight                     ((,class (:foreground "gray50"  :background ,bg4))))
        `(magit-diffstat-added                             ((,class (:foreground ,type))))
        `(magit-diffstat-removed                           ((,class (:foreground ,var))))
        `(magit-process-ok                                 ((,class (:foreground ,func    :weight bold))))
        `(magit-process-ng                                 ((,class (:foreground ,warning :weight bold))))
        `(magit-branch                                     ((,class (:foreground ,const   :weight bold))))
        `(magit-log-author                                 ((,class (:foreground ,fg5))))
        `(magit-hash                                       ((,class (:foreground ,org-purple :bold t))))
        `(magit-diff-file-header                           ((,class (:foreground ,fg3 :background ,bg4))))
        `(lazy-highlight                                   ((,class (:foreground ,fg3 :background ,bg4))))
        `(term                                             ((,class (:foreground ,fg1 :background ,bg1))))
        `(term-color-black                                 ((,class (:foreground ,bg4 :background ,bg4))))
        `(term-color-blue                                  ((,class (:foreground ,func :background ,func))))
        `(term-color-red                                   ((,class (:foreground ,keyword :background ,bg4))))
        `(term-color-green                                 ((,class (:foreground ,type :background ,bg4))))
        `(term-color-yellow                                ((,class (:foreground ,var :background ,var))))
        `(term-color-magenta                               ((,class (:foreground ,builtin :background ,builtin))))
        `(term-color-cyan                                  ((,class (:foreground ,str :background ,str))))
        `(term-color-white                                 ((,class (:foreground ,fg3 :background ,fg3))))
        `(rainbow-delimiters-unmatched-face                ((,class :foreground ,warning)))
        `(helm-header                                      ((,class (:foreground ,fg3     :background ,bg1 :underline nil :box    nil))))
        `(helm-source-header                               ((,class (:foreground ,keyword :background ,bg1 :underline nil :weight bold))))
        `(helm-selection                                   ((,class (:background ,bg3 :underline nil))))
        `(helm-selection-line                              ((,class (:background ,bg3))))
        `(helm-visible-mark                                ((,class (:foreground ,bg1     :background ,bg4))))
        `(helm-candidate-number                            ((,class (:foreground ,bg1     :background ,fg1))))
        `(helm-separator                                   ((,class (:foreground ,type    :background ,bg1))))
        `(helm-time-zone-current                           ((,class (:foreground ,builtin :background ,bg1))))
        `(helm-time-zone-home                              ((,class (:foreground ,type    :background ,bg1))))
        `(helm-buffer-not-saved                            ((,class (:foreground ,type    :background ,bg1))))
        `(helm-buffer-process                              ((,class (:foreground ,builtin :background ,bg1))))
        `(helm-buffer-saved-out                            ((,class (:foreground ,fg1     :background ,bg1))))
        `(helm-buffer-size                                 ((,class (:foreground ,fg1     :background ,bg1))))
        `(helm-ff-directory                                ((,class (:foreground ,func    :background ,bg1     :weight bold))))
        `(helm-ff-file                                     ((,class (:foreground ,fg1     :background ,bg1     :weight normal))))
        `(helm-ff-executable                               ((,class (:foreground ,key2    :background ,bg1     :weight normal))))
        `(helm-ff-invalid-symlink                          ((,class (:foreground ,key3    :background ,bg1     :weight bold))))
        `(helm-ff-symlink                                  ((,class (:foreground ,keyword :background ,bg1     :weight bold))))
        `(helm-ff-prefix                                   ((,class (:foreground ,bg1     :background ,keyword :weight normal))))
        `(helm-grep-cmd-line                               ((,class (:foreground ,fg1     :background ,bg1))))
        `(helm-grep-file                                   ((,class (:foreground ,fg1     :background ,bg1))))
        `(helm-grep-finish                                 ((,class (:foreground ,fg3     :background ,bg1))))
        `(helm-grep-lineno                                 ((,class (:foreground ,fg1     :background ,bg1))))
        `(helm-grep-match                                  ((,class (:foreground nil      :background nil      :inherit helm-match))))
        `(helm-grep-running                                ((,class (:foreground ,func    :background ,bg1))))
        `(helm-moccur-buffer                               ((,class (:foreground ,func    :background ,bg1))))
        `(helm-source-go-package-godoc-description         ((,class (:foreground ,str))))
        `(helm-bookmark-w3m                                ((,class (:foreground ,type))))
        `(company-echo-common                              ((,class (:foreground ,bg1 :background ,fg1))))
        `(company-preview                                  ((,class (:background ,bg1 :foreground ,key2))))
        `(company-preview-common                           ((,class (:foreground ,bg3 :foreground ,fg5))))
        `(company-preview-search                           ((,class (:foreground ,type :background ,bg1))))
        `(company-scrollbar-bg                             ((,class (:background ,bg4))))
        `(company-scrollbar-fg                             ((,class (:foreground ,keyword))))
        `(company-tooltip                                  ((,class (:foreground ,fg1 :background ,bg1))))
        `(company-tooltop-annotation                       ((,class (:foreground ,const))))
        `(company-tooltip-common                           ((,class (:foreground ,fg5))))
        `(company-tooltip-common-selection                 ((,class (:foreground ,builtin :italic t :bold nil))))
        `(company-tooltip-mouse                            ((,class (:inherit highlight))))
        `(company-tooltip-selection                        ((,class (:background ,silky-gray :foreground ,fg1 :bold t))))
        `(company-template-field                           ((,class (:inherit region))))
        `(web-mode-builtin-face                            ((,class (:inherit    font-lock-builtin-face))))
        `(web-mode-comment-face                            ((,class (:inherit    font-lock-comment-face))))
        `(web-mode-constant-face                           ((,class (:inherit    font-lock-constant-face))))
        `(web-mode-keyword-face                            ((,class (:foreground ,keyword))))
        `(web-mode-doctype-face                            ((,class (:foreground "#9FE55B"                     :weight bold))))
        `(web-mode-function-name-face                      ((,class (:inherit    font-lock-function-name-face))))
        `(web-mode-string-face                             ((,class (:foreground ,str))))
        `(web-mode-type-face                               ((,class (:inherit    font-lock-type-face))))
        `(web-mode-html-attr-name-face                     ((,class (:foreground ,func))))
        `(web-mode-html-attr-value-face                    ((,class (:foreground ,keyword))))
        `(web-mode-warning-face                            ((,class (:inherit    font-lock-warning-face))))
        `(web-mode-html-tag-face                           ((,class (:foreground ,builtin                      :weight bold))))
        `(web-mode-css-pseudo-class-face                   ((,class (:foreground "cyan3"                       :weight normal))))
        `(doom-modeline-buffer-major-mode                  ((,class (:foreground ,const      :inherit (doom-modeline-emphasis bold)))))
        `(doom-modeline-host                               ((,class (:foreground "#7EF96B"   :inherit (doom-modeline          italic)))))
        `(doom-modeline-urgent                             ((,class (:foreground ,warning    :inherit (doom-modeline          error)))))
        `(doom-modeline-warning                            ((,class (:foreground ,type       :inherit (doom-modeline)))))
        `(doom-modeline-buffer-modified                    ((,class (:foreground ,type       :inherit (doom-modeline          warning bold)
                                                                     :background unspecified))))
        `(jde-java-font-lock-package-face                  ((,class (:foreground ,var))))
        `(jde-java-font-lock-public-face                   ((,class (:foreground ,keyword))))
        `(jde-java-font-lock-private-face                  ((,class (:foreground ,keyword))))
        `(jde-java-font-lock-constant-face                 ((,class (:foreground ,const))))
        `(jde-java-font-lock-modifier-face                 ((,class (:foreground ,key3))))
        `(jde-jave-font-lock-protected-face                ((,class (:foreground ,keyword))))
        `(jde-java-font-lock-number-face                   ((,class (:foreground ,var))))
        `(circe-originator-face                            ((,class (:weight     bold))))
        `(circe-server-face                                ((,class (:inherit    font-lock-comment-face))))
        `(circe-my-message-face                            ((,class (:background ,bg4))))
        `(lui-time-stamp-face                              ((,class (:foreground "SlateBlue"))))
        `(wl-highlight-message-header-contents             ((,class (:foreground "mediumpurple"))))
                                                                     ;; message-header-name
        `(wl-highlight-message-headers                     ((,class (:foreground "green"))))
                                                                     ;; message-header-xheader
        `(wl-highlight-message-important-header-contents   ((,class (:foreground "deepskyblue1" :weight bold))))
                                                                     ;; message-header-other
        `(wl-highlight-message-important-header-contents2  ((,class (:foreground "VioletRed1"   :weight bold))))
        `(wl-highlight-message-unimportant-header-contents ((,class (:foreground ,var))))
        `(wl-highlight-summary-normal-face                 ((,class (:foreground ,const))))
        `(wl-highlight-summary-new-face                    ((,class (:foreground "PaleGreen"    :weight bold))))
                                                                     ;; gnus-summary-normal-read
        `(wl-highlight-summary-unread-face                 ((,class (:foreground "#8AEE60"))))
                                                                     ;; gnus-summary-low-read
        `(wl-highlight-summary-low-unread-face             ((,class (:inherit    wl-highlight-summary-unread-face :slant  italic))))
                                                                     ;; gnus-summary-high-read
        `(wl-highlight-summary-high-unread-face            ((,class (:inherit    wl-highlight-summary-unread-face :weight bold))))
                                                                     ;; gnus-summary-normal-ancient
        `(wl-highlight-summary-thread-top-face             ((,class (:foreground "SkyBlue"))))
                                                                     ;; gnus-summary-high-ancient
        `(wl-highlight-summary-high-read-face              ((,class (:inherit    wl-highlight-summary-thread-top-face :weight bold))))
                                                                     ;; gnus-summary-low-ancient
        `(wl-highlight-summary-low-read-face               ((,class (:inherit    wl-highlight-summary-thread-top-face :slant  italic))))
        `(wl-highlight-summary-normal-face                 ((,class (:foreground ,const))))
        `(wl-highlight-summary-answered-face               ((,class (:foreground "steelblue2"))))
        `(wl-highlight-summary-deleted-face                ((,class (:foreground ,type        :strike-through t        :slant italic))))
        `(wl-highlight-summary-disposed-face               ((,class (:foreground "gray"       :strike-through t))))
        `(wl-highlight-header-separator-face               ((,class (:foreground ,org-date-fg :background     ,org-bg2 :slant italic)))))
   (custom-theme-set-variables
    'klere
    '(org-ellipsis                       " ▼")
    '(org-fontify-quote-and-verse-blocks t))


 ;; Search functionality
   (defvar klere---isearch-hl-line-p                   nil
     "Boolean for the Klere Emacs theme to turn on `hl-line-mode' during `isearch'.")
   (when klere---isearch-hl-line-p
     (defvar klere---isearch-hl-line-major-modes-to-skip '()
       "Major modes to not activate `hl-line-mode' in when using `isearch'.")

     (defun klere-turn-on-hl-line-mode ()
       (interactive)

       (when (not (member major-mode klere---isearch-hl-line-major-modes-to-skip))
         (hl-line-mode +1)))
     (defun klere-turn-off-hl-line-mode ()
       (interactive)

       (when (not (member major-mode klere---isearch-hl-line-major-modes-to-skip))
         (hl-line-mode -1)))

     (add-hook 'isearch-mode-hook     #'klere-turn-on-hl-line-mode)
     (add-hook 'isearch-mode-end-hook #'klere-turn-off-hl-line-mode)))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'klere)

;; Local Variables:
;; no-byte-compile: t
;; End:
(provide 'klere-theme)
;;; klere-theme.el ends here
