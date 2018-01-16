;;; kaolin-ocean-theme.el --- Dark blue Kaolin theme
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme ocean "Dark blue Kaolin theme variant."
  ;; Palette modification
  ((bg1 blue5 black1)
   (bg2 "#1d1d2b" black2)
   (bg3 "#28283a" black3)
   (bg4 "#32324a" black4)

   (azure2 "#325074")

   (keyword     azure4)
   ;; TODO: a bit more bright
   (second-key  bg4 cerise4)
   (builtin     capri4)
   (functions   builtin)
   (var         violet4)
   (const       cerise4)
   (type        cyan1)
   (num         pink1)
   (bool        num)
   (prep        blue1)

   (comment     gray0)
   (alt-comment "#34344c")
   (str         amber3 "#ffd787")
   (str-alt     vermilion4)
   (doc         str-alt)
   (warning     orange1)
   (err         red1)

   (dim-buffer "#0F0F17")
   (hl         cyan2)
   (hl-line    (if kaolin-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      bg4)

   (todo pink1)

   (tooltip-hl-bg bg4)
   (tooltip-hl-fg chartreuse1)

   (ivy2 pink1)
   (ivy3 amber3)
   (ivy4 spring-green1)

   (rb1 cyan3)
   (rb2 violet4)
   (rb3 cyan3)
   (rb4 blue4)
   (rb5 spring-green4)
   (rb6 spring-green3)
   (rb7 amber3)
   (rb8 azure2)
   (rb9 azure3)

   (diff-add    aquamarine4)
   (diff-change magenta4)
   (diff-rem    red4)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       bg3)

   (segment-active    gray3)
   (segment-inactive  gray3)
   (evil-normal       teal1)
   (evil-insert       spring-green1)
   (evil-visual       orange1)
   (evil-replace      red1)
   (evil-motion       yellow1)
   (evil-operator     evil-normal)
   (evil-emacs        amber3)

   (win-border    bg3)
   (line-num-bg   bg1)
   (line-num-fg   bg4)
   (line-num-hl   blue4 gray9)
   (cursor        "#c3c8e0"))

  ;; Custom theme set faces
  (
   (link                (:foreground cyan1 :underline underline))
   (show-paren-mismatch (:background bg2 :foreground red0))

   (org-code            (:foreground teal1))
   (org-verbatim        (:foreground spring-green1))
   (org-quote           (:foreground blue4))

   (git-gutter:added    (:background diff-add :foreground diff-add))
   (git-gutter:modified (:background diff-change :foreground diff-change))
   (git-gutter:deleted  (:background diff-rem :foreground diff-rem)))

  ;; Set custom vars
  (custom-theme-set-variables
   'kaolin-ocean
   '(kaolin-hl-line-colored t))

  (when kaolin-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-ocean
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-change :foreground ,diff-change))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))



;;; kaolin-ocean-theme.el ends here
