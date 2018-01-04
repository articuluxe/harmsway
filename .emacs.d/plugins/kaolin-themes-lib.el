;;; kaolin-themes-lib.el --- Kaolin-themes library, provides common parts for the theme engine

;; TODO: (??) add travis ci

;; TODO: color spec and color functions
;; TODO: add accent color like aquamarine
;; TODO: add pure colors

;; TODO: (??) make mode-line dark in ligth themes.
;; TODO: bright background option
;; TODO: (??) disable color background for terminal
;; TODO: add to all colors light and dark variant
;; TODO: (??) add var to highlight key seq'

;; TODO: Add the following faces to lib:
;; TODO: custom-* and buttons
;; TODO: magit faces

;; TODO: add mode-line option/flat mode-line style

;; TODO: (??) add base(terminal) colors
;; TODO: (??) colorful comments
;; TODO: treemacs support
;; TODO: (??) color cornflower blue
;; TODO: (??) add -pkg.el
;; TODO: (??) outline-* faces

;; TODO: line-num opt: hl or mono(gray)

;; TODO: add colored selection option
;; TODO: distant foregound
;; TODO: move git-gutter faces from a theme file to lib,
;; i.e. avoid duplication

;; TODO: add company-tooltip-common-selection for new themes

;; TODO (??) background: #5a6066 || #5d636a
;; #617c7e

;; Predefined Kaolin palette
;; 22-24 colors
;; NEW - OLD
;; GAP +/- ~7 hue
;; TODO: (??)
;; saturation 100
;; value 85
;; TODO: (??)
;; color0 - pure
;; color1 - color
;; color2 - dark
;; color3 - light/bright
;; color4 - faded


;; TODO: (??) num, link and prep color vars use the same color

;; TERMINAL COLORS: 0-7 is dark, 8-15 is light, so...

(defconst kaolin-palette
  '(
    ;; (black0          "#181818")
    ;; (black1          "#1b1b1b")
    ;; (black2          "#252525")
    ;; (black3          "#2f2f2f")
    ;; (black4          "#353535")

    ;; Black - #020203
    (black0          "#161618")
    (black1          "#18181B")
    (black2          "#222225")
    (black3          "#2B2B2F")
    (black4          "#303035")

    ;; Midnigh colors
    (midnight-green       "#142223")
    (alt-midnight-green   "#0f1e1d")
    (midnight-blue        "#1e2528" black2)
    ;; (alt-midnight-blue "#062732")
    ;; (alt-midnight-blue "#12121a")
    (alt-midnight-blue    "#13131c" black2)
    (midnight-purple      "#1a121a")

    ;; Gray
    ;; TODO: (??) change hue to 240?
    (gray0           "#353b3c")
    (gray1           "#383e3f")
    (gray2           "#414849") ; old gray
    (gray2           "#4b5254")
    (gray3           "#545c5e")
    (gray4           "#60696b") ; old alt-gray
    (gray5           "#697375")
    (gray6           "#737d80")
    (gray7           "#7c878a") ; old bright-gray
    (gray8           "#879193")
    (gray9           "#919a9c") ; old light-gray


    ;; TODO:
    (lavender-gray   "#b6b5c5")
    (grayish-orange  "#a5a19c")

    ;; White - #FDFDFF
    (white0          "#f2f2f2")
    (white1          "#e6e6e8")
    (white2          "#d4d4d6")
    (white3          "#c9c9cd")
    (white4          "#bebec4")

    ;; Yellow #FFFF00
    (dark-yellow     "#555a2f")
    (yellow          "#acb370")

    ;; Amber #FFBF00
    (amber           "#d4b668")
    (faded-wheat     "#d9ca9b")
    (light-yellow    "#c9bb87")
    ;; WHEAT #f5deb3
    ;; TODO: fix second
    ;; (??) desaturared
    (wheat           "#d1bb90" "#ffd7a5")

    ;; Orange #FF7F00
    (alt-orange      "#d9a76f")
    (orange          "#dbac66")
    (alt-yellow      "#be9266")
    (alt-wheat       "#fdd5b1")
    (pure-orange     "#cc6a00")

    ;; TODO:
    (faded-orange    "#cd9575" "#d7af87")

    ;; TODO Vermilion #FF3F00
    (light-orange    "#ddc085")

    ;; Red #FF0000
    (alt-red         "#c93232") ; strong red
    (red             "#cd5c5c") ; moderate red
    (light-red       "#d66e75") ; Slightly desaturated red/soft red
    (faded-red       "#863d42") ; dark moderate red; muted red

    ;; TODO: adjust
    ;; Maroon  - dark-red
    (dark-red        "#832729") ; dark red or maroon?

    ;; Crimson #FF003F
    (moderate-pink   "#a0586c")
    (light-pink      "#ef98aa")

    ;; Rose #FF007F
    (pink            "#d24b83")
    (soft-pink       "#fbaed2")
    ;; Dark rose is eggplant

    ;; Cerise #FF00BF
    ;; (alt-purple      "#915c83")
    (alt-purple      "#a9779c")
    ;; TODO:
    (dark-violet     "#997a8d")

    ;; Magenta/Fuchsia #FF00FF
    (dark-purple     "#563d56")
    (purple          "#835d83")

    ;; TODO:
    (grayish-magenta "#796878")
    (light-purple    "#cea2ca")

    ;; Purple #BF00FF
    (violet          "#ab98b5")

    ;; Violet #7F00FF
    (lavender        "#967bb6")
    (alt-lavender    "#9d81ba")

    (light-violet    "#d1aef4")

    ;; TODO: add to group
    (alt-violet      "#af94f5")

    ;; Ultramarine #3F00FF
    ;; TODO: add color

    ;; Blue #0000FF
    ;; (blue             "#6a6a9a")
    ;; (blue             "#5757ad")
    (faded-blue       "#817f96") ;  Navy

    (magenta         "#5454b6") ; TODO my current magenta is moderate blue or even navy

    ;; Cerulean #003FFF
    (grayish-blue      "#687184")

    ;; Azure/Sky Blue #007FFF
    (alt-grayish-blue  "#8f9ca7")
    (dark-blue         "#2a4661")

    (blue             "#3b6fa3")

    ;; Dodger blue
    ;; (moderate-blue    "#4e7f95")
    (moderate-blue    "#53859d")


    (soft-blue        "#4ca6e8")

    ;; Capri/Deep Sky Blue #00BFFF
    ;; TOOD: adjust
    (alt-blue         "#267fb5")
    (teal-blue          "#91b9c7")

    ;; Cyan #00FFFF
    (cyan             "#54b6b6")
    (dark-cyan        "#098b8b")

    ;; my old teal is dark cyan
    (teal               "#80b6bc")

    ;; Aquamarine #00FFBF
    ; (aquamarine         "#7fffd4")
    (aquamarine         "#68f3c5")

    ;; TODO: new group
    (light-jade         "#709688")


    ;; TODO: Viridian
    (viridian           "#40826d")

    ;; Spring green #00FF7F
    (dark-jade          "#2E4038")
    (jade               "#597A6C")
    (teal-green         "#6fb593")
    (dark-green         "#39855f")
    (light-green        "#54b685")

    ;; TODO: adjust
    (light-jade         "#709688")

    ;; Erin #00FF3F
    ;; TODO: add color

    ;; Green #00FF00
    (alt-lime           "#8fbc8f")
    (grayish-green      "#9ca78f")

    ;; Harlequin #3FFF00
    ;; TODO: add color

    ;; Chartreuse #7FFF00
    (lime               "#85b654")

    ;; Lime #D5FF00
    ;; "#b9c791"

    ;; EXTRA COLORS
    ;; TODO Brown
    ;; Brown orange + black
    (brown           "#7d6360")
    (light-brown     "#ae9895")
    (alt-brown       "#52413f")
    (bazaar          "#98777b")

    ;; TODO: make MAIN color more green :>
    (deep-green         "#39656b")
    ;; Teal; TODO: make a bit more green
    ;; (green              "#4a858c")
    (green              "#4d8d93")


    ;; Named face options
    (bold            kaolin-bold)
    (italic          kaolin-italic)
    (underline       kaolin-underline)
    (underline-style (if kaolin-wave 'wave 'line))

    (fg0  white0)
    (fg1  white1)
    (fg2  white2)
    (fg3  white3)
    (fg4  white4)

    (bg0  black0)
    (bg1  black1)
    (bg2  black2)
    (bg3  black3)
    (bg4  black4)

    (dim-buffer black0)
    ;; TODO: change because almost same green using by strings
    (hl         aquamarine)
    (hl-mono    gray4)
    (hl-line    (if kaolin-hl-line-colored midnight-blue bg2))
    (hl-indent  gray3)
    (selection  bg3)
    (pulse      dark-jade)

    (todo red)
    (done teal-green)

    (button grayish-orange)
    (button-hl light-orange)

    (tooltip-bg bg2)
    (tooltip-fg gray9)
    (tooltip-hl-bg alt-brown)
    (tooltip-hl-fg light-orange)

    (rb1 teal)
    (rb2 violet)
    (rb3 jade)
    (rb4 faded-blue)
    (rb5 green)
    (rb6 light-violet)
    (rb7 grayish-orange)
    (rb8 grayish-magenta)
    (rb9 lavender)

    (diff-add    light-green)
    (diff-change violet)
    (diff-rem    red)

    (keyword     green)
    (second-key  deep-green)
    (builtin     teal)
    (comment     gray3)
    (alt-comment alt-grayish-blue)
    (functions   builtin)
    ;; TODO: (??) change to light-brown like sierra.vim
    (str         teal-green)
    (str-alt     jade)
    (doc         str-alt)
    (type        faded-orange)
    (const       violet)
    (var         faded-blue)
    (num         red)
    (bool        num)
    (prep        lavender)
    (warning     orange)
    (err         red)

    ;; Mode-line
    (line-fg           fg4)
    (line-bg1          bg2)
    (line-bg2          bg4)
    (line-border       bg4)
    (line-color1       keyword)
    (line-color2       builtin)
    (segment-active    gray3)
    (segment-inactive  gray3)
    (evil-normal       green)
    (evil-insert       light-green)
    (evil-visual       orange)
    (evil-replace      red)
    (evil-motion       yellow)
    (evil-operator     evil-normal)
    (evil-emacs        light-yellow)

    (win-border    black3)
    (line-num-bg   bg1)
    (line-num-fg   gray3)
    (line-num-hl   gray9)
    (cursor        white0)

    (swiper-bg   bg2)
    (ivy-bg      nil)
    (ivy1        fg1)
    (ivy2        soft-blue)
    (ivy3        light-orange)
    (ivy4        light-violet)))

;; Predefined Kaolin face specifications
(defconst kaolin-faces
  '(
    ;; Font-lock
    (font-lock-builtin-face           (:foreground builtin))
    (font-lock-comment-delimiter-face (:foreground comment))
    (font-lock-comment-face           (:foreground comment))
    (font-lock-constant-face          (:foreground const))
    (font-lock-doc-face               (:foreground doc))
    (font-lock-function-name-face     (:foreground functions :bold bold))
    (font-lock-keyword-face           (:foreground keyword :bold bold))
    (font-lock-negation-char-face     (:foreground red))
    (font-lock-preprocessor-face      (:foreground prep :bold nil))
    (font-lock-reference-face         (:foreground const))
    (font-lock-string-face            (:foreground str))
    (font-lock-type-face              (:foreground type))
    (font-lock-variable-name-face     (:foreground var))
    (font-lock-warning-face           (:background nil :foreground warning))

    ;; Kaolin faces
    (kaolin-boolean (:foreground bool))

    ;; General
    (default             (:background bg1 :foreground fg1))
    (warning             (:foreground warning))
    (error               (:foreground err))
    (shadow              (:foreground gray4))
    (file-name-shadow    (:inherit 'shadow))
    (region              (:background selection))
    (secondary-selection (:background dark-jade))
    (fringe              (:background bg1 :foreground fg1))
    (cursor              (:background cursor))
    (vertical-border     (:foreground win-border))
    (window-divider      (:foreground win-border))
    (minibuffer-prompt   (:foreground keyword :bold bold))
    (bold                (:bold bold))
    (italic              (:italic italic))
    (default-italic      (:italic italic))
    (bold-italic         (:bold bold :italic italic))
    ;; TODO: add link var
    (link                (:foreground prep :underline underline))
    (link-visited        (:inherit 'link :underline nil))
    (success             (:background nil :foreground light-green))
    (escape-glyph        (:background nil :foreground cyan))

    (menu        (:background bg2 :foreground fg2))
    (header-line (:background midnight-blue :foreground var))
    (tooltip     (:foreground tooltip-bg :foreground tooltip-fg))

    (match        (:background nil :foreground hl))
    (isearch      (:background nil :foreground hl :bold bold :underline underline))
    (isearch-fail (:background nil :foreground red))


    ;; Interface
    (package-name (:inherit 'link :underline nil))
    (button                (:inherit 'link))
    (custom-button         (:background bg3 :foreground button :box (:line-width 2 :color bg2 :style 'released-button)))
    (custom-button-mouse   (:background bg4 :foreground button-hl :box (:line-width 2 :color bg2 :style 'released-button)))
    (custom-button-pressed (:background bg4 :foreground button-hl :box (:line-width 2 :color bg2 :style 'pressed-button)))
    (custom-button-unraised (:inherit 'custom-button))
    (custom-button-pressed-unraised (:inherit 'custom-button-pressed))
    (custom-state          (:background nil :foreground green))
    (custom-changed        (:background nil :foreground orange))
    (custom-visibility     (:background nil :foreground cyan :height 0.9 :underline underline))
    (custom-invalid        (:background nil :foreground red))
    (custom-set            (:background nil :foreground light-jade))
    (widget-documentation  (:background nil :foreground var))
    (widget-button         (:background nil :foreground keyword))

    ;; Highlighting
    (highlight                (:background bg2 :foreground light-orange))
    (lazy-highlight           (:background bg3 :foreground fg2))
    (hl-line                  (:background hl-line))
    (highlight-numbers-number (:foreground num))
    (highlight-quoted-quote   (:inherit 'font-lock-builtin-face))
    (highlight-quoted-symbol  (:inherit 'font-lock-keyword-face))

    ;; Highlight indent guides
    (highlight-indent-guides-odd-face        (:background hl-indent))
    (highlight-indent-guides-even-face       (:background hl-indent))
    (highlight-indent-guides-character-face  (:foreground hl-indent))

    ;; Indent-guide
    (indent-guide-face (:foreground hl-indent))

    ;; Highlighting indentation
    (highlight-indentation-face                 (:background bg2))
    (highlight-indentation-current-column-face  (:background bg3))

    ;; Eldoc
    (eldoc-highlight-function-argument  (:inherit 'font-lock-constant-face))

    ;; Pulse
    (pulse-highlight-start-face (:background pulse))

    ;; Auto-dim-other-buffers
    (auto-dim-other-buffers-face  (:background dim-buffer))

    ;; Linum & nlinum
    (linum                        (:background line-num-bg :foreground line-num-fg :bold nil
                                               :italic nil :underline nil :strike-through nil))
    (linum-highlight-face         (:inherit 'linum))
    (nlinum-current-line          (:background line-num-bg :foreground line-num-hl :bold bold
                                               :italic nil :underline nil :strike-through nil))
    (linum-relative-current-line  (:inherit 'nlinum-current-line))
    (nlinum-relative-current-face (:inherit 'nlinum-current-line))

    ;; Native line numbers
    (line-number                  (:background line-num-bg :foreground line-num-fg :bold nil
                                               :italic nil :underline nil :strike-through nil))
    (line-number-current-line     (:background line-num-bg :foreground line-num-hl :bold bold
                                               :italic nil :underline nil :strike-through nil))

    ;; Which-function-mode
    (which-func (:foreground orange))

    ;; Which-key
    (which-key-key-face                   (:foreground light-green :bold bold))
    (which-key-group-description-face     (:foreground alt-lavender))
    (which-key-local-map-description-face (:foreground soft-blue))
    (which-key-command-description-face   (:foreground teal))

    ;; Ruler-mode
    (ruler-mode-default        (:background bg2 :foreground gray3))
    (ruler-mode-column-number  (:foreground var))
    (ruler-mode-current-column (:foreground orange))
    (ruler-mode-fill-column    (:foreground pink))
    (ruler-mode-comment-column (:foreground teal-blue))
    (ruler-mode-fringes        (:foreground green))
    (ruler-mode-pad            (:foreground var))
    (ruler-mode-tab-stop       (:foreground violet))
    (ruler-mode-goal-column    (:foreground alt-red))

    ;; TODO: Message faces
    (message-header-name    (:foreground deep-green))
    (message-header-subject (:foreground teal-green))
    (message-header-to      (:foreground teal-green))
    (message-header-other   (:foreground teal))

    ;; Elfeed
    (elfeed-search-tag-face          (:foreground light-yellow))
    (elfeed-search-feed-face         (:foreground green))
    (elfeed-search-date-face         (:foreground var))
    (elfeed-search-unread-title-face (:foreground fg1))
    (elfeed-search-unread-count-face (:foreground orange))
    (elfeed-search-title-face        (:foreground comment))

    ;; Modeline
    (mode-line           (:box (:line-width 2 :color line-border) :background line-bg1 :foreground var :bold bold))
    (mode-line-inactive  (:box (:line-width 2 :color line-border) :background line-bg1 :foreground gray9 :bold bold))
    (mode-line-buffer-id (:background nil :foreground line-color2 :bold bold))
    (mode-line-highlight (:foreground line-color2 :box nil :bold bold))
    (mode-line-emphasis  (:foreground fg1))

    ;; Telephone-line
    (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground line-fg))
    (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground gray9))
    (telephone-line-evil            (:inherit 'mode-line))
    (telephone-line-evil-normal     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-normal))
    (telephone-line-evil-insert     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-insert))
    (telephone-line-evil-visual     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-visual))
    (telephone-line-evil-replace    (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-replace))
    (telephone-line-evil-motion     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-motion))
    (telephone-line-evil-operator   (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-operator))
    (telephone-line-evil-emacs      (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-emacs))

    ;; Powerline
    ;; TODO: check it
    (powerline-active1   (:inherit 'mode-line))
    (powerline-active2   (:inherit 'mode-line))
    (powerline-inactive1 (:inherit 'mode-line-inactive))
    (powerline-inactive2 (:inherit 'mode-line-inactive))

    ;; Spaceline
    (spaceline-highlight-face (:foreground teal))

    ;; Smart-mode-line
    (sml/line-number      (:foreground lime))
    (sml/modes            (:foreground purple))
    (sml/global           (:foreground teal))
    (sml/filename         (:foreground green))
    (sml/charging         (:foreground green))
    (sml/discharging      (:foreground red))
    (sml/modified         (:foreground light-green :bold bold))
    (sml/outside-modified (:background alt-red :foreground fg1))
    (sml/prefix           (:foreground line-fg))
    (sml/read-only        (:foreground orange))

    ;; TODO: maybe delete or change to other package
    ;; Fic-mode
    (fic-face         (:background nil :foreground todo :bold bold))
    (fic-author-face  (:background nil :foreground todo :bold bold))

    ;; Additional completion
    (ac-completion-face    (:foreground keyword :underline underline))
    (info-quoted-name      (:foreground builtin))
    (info-string           (:foreground str))
    (icompletep-determined (:foreground builtin))

    ;; Company
    (company-tooltip                  (:background tooltip-bg :foreground fg3 :bold bold))
    (company-tooltip-common           (:foreground hl))
    ;; TODO:
    (company-tooltip-common-selection (:bold bold))
    (company-tooltip-selection        (:background tooltip-hl-bg :foreground tooltip-hl-fg))
    (company-tooltip-annotation       (:foreground var))
    (company-scrollbar-bg             (:background bg1))
    (company-scrollbar-fg             (:foreground keyword))
    ;; TODO: read about template
    (company-template-field           (:background bg3))
    (company-echo-common              (:background bg1 :foreground light-yellow))
    (company-preview                  (:background nil :foreground keyword))
    (company-preview-common           (:background bg2 :foreground light-orange))
    (company-preview-search           (:background bg1 :foreground blue))
    (company-tooltip-mouse            (:background bg3 :foreground fg3))


    ;; Flycheck
    (flycheck-info           (:foreground teal-blue))
    (flycheck-warning        (:underline (:style underline-style :color warning)))
    (flycheck-error          (:underline (:style underline-style :color err)))
    (flycheck-fringe-error   (:foreground err))
    (flycheck-fringe-warning (:foreground warning))
    (flycheck-fringe-info    (:foreground teal-blue))

    ;; Flyspell
    (flyspell-duplicate (:underline (:style underline-style :color warning)))
    (flyspell-incorrect (:underline (:style underline-style :color err)))

    ;; Hydra
    (hydra-face-red      (:foreground red))
    (hydra-face-teal     (:foreground teal))
    (hydra-face-blue     (:foreground soft-blue))
    (hydra-face-pink     (:foreground pink))
    (hydra-face-amaranth (:foreground purple))

    ;; Ido
    (ido-indicator   (:foreground num))
    (ido-first-match (:foreground hl :bold bold))
    (ido-only-match  (:foreground cyan))
    (ido-subdir      (:foreground lavender))

    ;; Gnus
    (gnus-header-content (:foreground keyword))
    (gnus-header-from    (:foreground var))
    (gnus-header-name    (:foreground type))
    (gnus-header-subject (:foreground functions :bold bold))

    ;; Mu4e
    (mu4e-header-marks-face    (:foreground type))
    (mu4e-view-url-number-face (:foreground type))
    (mu4e-cited-1-face         (:foreground fg2))
    (mu4e-cited-7-face         (:foreground fg3))

    ;; ffap
    (ffap (:foreground fg4))

    ;; Slime
    (slime-repl-inputed-output-face (:foreground type))

    ;; Js-mode
    (js2-private-function-call    (:foreground const))
    (js2-jsdoc-html-tag-delimiter (:foreground str))
    (js2-jsdoc-html-tag-name      (:foreground keyword))
    (js2-external-variable        (:foreground type))
    (js2-function-param           (:foreground const))
    (js2-error                    (:underline (:color alt-red :style underline-style)))
    (js2-function-call            (:foreground functions))
    (js2-object-property          (:foreground light-brown))
    (js2-jsdoc-value              (:foreground str))
    (js2-private-member           (:foreground fg3))
    (js3-function-param-face      (:foreground keyword))
    (js3-instance-member-face     (:foreground const))
    (js3-external-variable-face   (:foreground var))
    (js3-jsdoc-tag-face           (:foreground keyword))
    (js3-warning-face             (:underline keyword))
    (js3-error-face               (:underline err))

    ;; Latex
    (font-latex-bold-face                (:foreground type))
    (font-latex-italic-face              (:foreground keyword :italic italic))
    (font-latex-string-face              (:foreground str))
    (font-latex-match-reference-keywords (:foreground const))
    (font-latex-match-variable-keywords  (:foreground var))

    ;; Latex/Auctex
    (font-latex-warning-face      (:inherit 'warning))
    (font-latex-string-face       (:inherit 'font-lock-string-face))
    (font-latex-math-face         (:foreground violet))
    (font-latex-sedate-face       (:foreground teal-blue))
    (font-latex-script-char-face  (:foreground violet))
    (font-latex-sectioning-0-face (:foreground wheat :bold bold))
    (font-latex-sectioning-1-face (:inherit 'font-latex-sectioning-0-face))
    (font-latex-sectioning-2-face (:inherit 'font-latex-sectioning-0-face))
    (font-latex-sectioning-3-face (:inherit 'font-latex-sectioning-0-face))
    (font-latex-sectioning-4-face (:inherit 'font-latex-sectioning-0-face))
    (font-latex-sectioning-5-face (:inherit 'font-latex-sectioning-0-face))

    ;; Undo-tree
    (undo-tree-visualizer-active-branch-face (:foreground fg1 :bold bold))
    (undo-tree-visualizer-current-face       (:foreground cyan))
    (undo-tree-visualizer-default-face       (:foreground fg2))
    (undo-tree-visualizer-unmodified-face    (:foreground var))
    (undo-tree-visualizer-register-face      (:foreground type))

    ;; Rainbow delimeters
    ;; TODO: change color & rewrite
    ;; TODO: probably I need to define custom vars or smth...
    (show-paren-match (:background nil :foreground orange :bold bold))
    ;; (if (eq show-paren-style 'expression)
    ;;  (show-paren-match-face (:background bg3 :foreground nil))))
    ;;  (show-paren-match-face (:background nil :foreground orange :bold bold)))))

    ;; TODO: make red more contrast
    (show-paren-mismatch (:background faded-red :foreground bg2))
    (rainbow-delimiters-unmatched-face (:foreground warning))
    (rainbow-delimiters-base-face    (:foreground rb1))
    (rainbow-delimiters-depth-1-face (:foreground rb1))
    (rainbow-delimiters-depth-2-face (:foreground rb2))
    (rainbow-delimiters-depth-3-face (:foreground rb3))
    (rainbow-delimiters-depth-4-face (:foreground rb4))
    (rainbow-delimiters-depth-5-face (:foreground rb5))
    (rainbow-delimiters-depth-6-face (:foreground rb6))
    (rainbow-delimiters-depth-7-face (:foreground rb7))
    (rainbow-delimiters-depth-8-face (:foreground rb8))
    (rainbow-delimiters-depth-9-face (:foreground rb9))

    ;; Diff
    (diff-header      (:background bg2))
    (diff-file-header (:background bg2 :foreground green))
    (diff-added       (:background dark-green :foreground fg1))
    (diff-changed     (:background diff-change :foreground fg1))
    (diff-removed     (:background dark-red :foreground fg1))

    ;; Imenu list
    ;; TODO:
    (imenu-list-entry-subalist-face-0 (:inherit 'font-lock-keyword-face))
    (imenu-list-entry-face-1 (:foreground tooltip-fg))
    (imenu-list-entry-face-0 (:inherit 'font-lock-type-face))

    ;; Git gutter
    (git-gutter:unchanged (:background bg1 :foreground nil))
    (git-gutter:added     (:background bg1 :foreground diff-add :bold bold))
    (git-gutter:modified  (:background bg1 :foreground diff-change :bold bold))
    (git-gutter:deleted   (:background bg1 :foreground diff-rem :bold bold))

    ;; Diff-hl
    (diff-hl-insert (:background diff-add))
    (diff-hl-change (:background diff-change))
    (diff-hl-delete (:background diff-rem))

    ;; Popup
    (popup-face                (:background tooltip-bg :foreground tooltip-fg :bold bold))
    (popup-menu-selection-face (:background tooltip-hl-bg :foreground tooltip-hl-fg :bold bold))
    (popup-tip-face            (:background tooltip-hl-bg :foreground builtin :bold bold))

    ;; Terminal
    (term               (:background bg1 :foreground fg1))
    (term-color-black   (:foreground black1))
    (term-color-blue    (:foreground blue))
    (term-color-red     (:foreground red))
    (term-color-green   (:foreground green))
    (term-color-yellow  (:foreground yellow))
    (term-color-magenta (:foreground purple))
    (term-color-cyan    (:foreground cyan))
    (term-color-white   (:foreground white2))

    ;; EShell
    (eshell-prompt        (:foreground green :bold bold))
    (eshell-ls-directory  (:foreground magenta :bold bold))
    (eshell-ls-symlink    (:foreground blue :bold bold))
    (eshell-ls-executable (:foreground lime :bold bold))
    (eshell-ls-archive    (:foreground red))
    (eshell-ls-backup     (:foreground purple))
    (eshell-ls-clutter    (:foreground pink))
    (eshell-ls-missing    (:background bg3 :foreground red))
    (eshell-ls-product    (:foreground yellow))
    (eshell-ls-readonly   (:foreground fg2))
    (eshell-ls-special    (:foreground light-green))
    (eshell-ls-unreadable (:foreground var))

    ;; Whitespace mode
    ;; TODO: Add variant for light themes
    (whitespace-empty            (:background dark-jade :foreground gray9))
    (whitespace-line             (:background bg3 :foreground warning))
    (whitespace-newline          (:foreground teal))
    (whitespace-indentation      (:background hl-indent))
    (whitespace-tab              (:background light-jade))
    (whitespace-space            (:background gray4 :foreground dark-jade))
    (whitespace-hspace           (:foreground cyan))
    (whitespace-space-before-tab (:background alt-yellow :foreground bg2))
    (whitespace-space-after-tab  (:background alt-yellow :foreground bg2))
    (whitespace-trailing         (:foreground alt-red))
    (whitespace-big-indent       (:background dark-red :foreground alt-red))

    ;; Org-mode
    ;; TODO: org agenda faces
    (org-todo                      (:foreground todo :bold bold))
    (org-done                      (:foreground done  :bold bold))
    (org-headline-done             (:foreground gray4  :bold nil))
    (org-ellipsis                  (:foreground builtin))
    (org-date                      (:foreground light-yellow :underline underline))
    (org-link                      (:inherit 'link))
    (org-code                      (:foreground light-yellow))
    (org-verbatim                  (:foreground soft-blue))
    (org-hide                      (:foreground bg1))
    (org-special-keyword           (:foreground functions))
    (org-table                     (:foreground var :bold bold))
    (org-formula                   (:foreground type))
    (org-warning                   (:foreground warning :underline underline))

    (org-document-info-keyword     (:foreground second-key))
    (org-meta-line                 (:inherit 'org-document-info-keyword))
    (org-block-begin-line          (:foreground second-key))
    (org-block-end-line            (:inherit 'org-block-begin-line))
    (org-list-dt                   (:inherit 'org-checkbox))
    (org-document-title            (:foreground builtin :bold bold))
    (org-document-info             (:foreground builtin))
    (org-footnote                  (:foreground fg4 :underline underline))

    (org-agenda-date-weekend       (:weight 'normal :foreground fg4))
    (org-block                     (:foreground fg3))
    (org-quote                     (:inherit 'org-block :slant 'italic))
    (org-verse                     (:inherit 'org-block :slant 'italic))
    (org-agenda-done               (:foreground bg4))
    (org-scheduled                 (:foreground type))
    (org-scheduled-today           (:foreground functions :height 1.2 :bold bold))
    (org-sexp-date                 (:foreground fg4))

    ;; Emmet
    (emmet-preview-input   (:foreground nil :background nil))
    (emmet-preview-output  (:foreground nil :background nil))

    ;; Flx
    (flx-highlight-face (:foreground hl :underline underline))

    ;; Smartparens
    (sp-pair-overlay-face (:foreground nil))

    ;; Web-mode
    (css-selector                   (:inherit 'font-lock-builtin-face))
    (web-mode-css-selector-face     (:inherit 'font-lock-builtin-face))
    (web-mode-type-face             (:inherit 'font-lock-type-face))
    (web-mode-html-tag-face         (:inherit 'font-lock-keyword-face))
    (web-mode-html-tag-bracket-face (:inherit 'web-mode-html-tag-face))
    (web-mode-html-attr-name-face   (:inherit 'font-lock-function-name-face))
    (web-mode-html-attr-value-face  (:inherit 'font-lock-string-face))
    (web-mode-builtin-face          (:inherit 'font-lock-builtin-face))
    (web-mode-keyword-face          (:inherit 'font-lock-builtin-face))
    (web-mode-constant-face         (:inherit 'font-lock-constant-face))
    (web-mode-comment-face          (:inherit 'font-lock-comment-face))
    (web-mode-doctype-face          (:inherit 'font-lock-preprocessor-face))
    (web-mode-function-name-face    (:inherit 'font-lock-function-name-face))
    (web-mode-string-face           (:inherit 'font-lock-string-face))
    (web-mode-warning-face          (:inherit 'font-lock-warning-face))

    ;; Nim
    (nim-font-lock-export-face (:inherit 'font-lock-function-name-face :italic nil))

    ;; Evil ex
    (evil-ex-info                   (:foreground orange))
    (evil-ex-substitute-matches     (:background nil :foreground red :underline underline))
    (evil-ex-substitute-replacement (:background nil :foreground light-green))
    (evil-ex-lazy-highlight         (:inherit 'lazy-highlight))

    ;; Evil-goggles
    ;; TODO: add rest of evil goggles faces
    (evil-goggles-default-face  (:background pulse))

    ;; Beacon-mode
    (beacon-fallback-background (:background pulse))

    ;; Helm
    ;; TODO: (!!) find helm status line color that based on var face
    ;; TODO: update faces
    ;; TODO: light theme: helm-find-files header
    ;; TODO: add helm-locate-finish and helm-prefarg
    ;; TODO: customize '[?]' write something like fsfasfsa in helm
    (helm-header                              (:foreground fg2 :underline nil :box nil))
    (helm-source-header                       (:foreground keyword :underline nil :bold bold))
    (helm-match                               (:foreground type :bold bold))
    (helm-header-line-left-margin             (:background blue :foreground bg1))
    (helm-selection                           (:background bg2 :foreground type :bold bold))
    (helm-selection-line                      (:background bg2 :foreground type :bold bold))
    (helm-visible-mark                        (:foreground blue))
    (helm-candidate-number                    (:foreground light-jade))
    (helm-separator                           (:foreground type))
    (helm-time-zone-current                   (:foreground builtin))
    (helm-time-zone-home                      (:foreground type))
    (helm-buffer-not-saved                    (:foreground type))
    (helm-buffer-process                      (:foreground builtin))
    (helm-buffer-saved-out                    (:foreground fg1))
    (helm-buffer-size                         (:foreground fg1))
    (helm-ff-directory                        (:foreground functions :bold bold))
    (helm-buffer-directory                    (:foreground purple))
    (helm-ff-dotted-directory                 (:foreground functions :bold bold))
    (helm-ff-dotted-symlink-directory         (:foreground blue :bold bold))
    (helm-ff-file                             (:foreground fg1 :weight 'normal))
    (helm-ff-executable                       (:foreground keyword :weight 'normal))
    (helm-ff-invalid-symlink                  (:foreground warning :bold bold))
    (helm-resume-need-update                  (:background alt-red :foreground nil))
    (helm-ff-symlink                          (:foreground keyword :bold bold))
    (helm-ff-prefix                           (:background keyword :foreground bg1 :weight 'normal))
    (helm-grep-cmd-line                       (:foreground fg1))
    (helm-grep-file                           (:foreground fg1))
    (helm-grep-finish                         (:foreground fg2))
    (helm-grep-lineno                         (:foreground fg1))
    (helm-grep-match                          (:inherit 'helm-match :background nil :foreground nil))
    (helm-grep-running                        (:foreground functions))
    (helm-moccur-buffer                       (:foreground functions))
    (helm-source-go-package-godoc-description (:foreground str))
    (helm-bookmark-w3m                        (:foreground type))

    ;; Ivy & swiper basic
    (ivy-current-match           (:background hl-line :foreground hl :bold t))
    (ivy-minibuffer-match-face-1 (:background nil :foreground ivy1))
    (ivy-minibuffer-match-face-2 (:background nil :foreground ivy2 :bold bold))
    (ivy-minibuffer-match-face-3 (:background nil :foreground ivy3 :bold bold))
    (ivy-minibuffer-match-face-4 (:background nil :foreground ivy4 :bold bold))
    (ivy-current-match           (:background hl-line :foreground hl :bold t))

    (swiper-match-face-1 (:background bg2 :foreground ivy1))
    (swiper-match-face-2 (:background bg2 :foreground ivy2 :bold bold))
    (swiper-match-face-3 (:background bg2 :foreground ivy3 :bold bold))
    (swiper-match-face-4 (:background bg2 :foreground ivy4 :bold bold))
    (swiper-line-face    (:inherit 'hl-line))))

;; Predefined Kaolin variables
;; (defconst kaolin-vars
;;   '())

(provide 'kaolin-themes-lib)

;;; kaolin-themes-lib.el ends here
