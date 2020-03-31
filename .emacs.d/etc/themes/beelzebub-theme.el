(require 'color)
  (require 'company)

  ;; Colors and attributes
  (setq-default b-font-height 80
		b-fg "#d0d0d0"
		b-bg "#121212"
		b-hg "#5f5f87")

  ;; Clear theme
  (dolist (f (face-list))
    (progn
      (set-face-foreground f b-fg)
      (set-face-background f b-bg)
      (set-face-attribute f nil
			  :height b-font-height
			  :width 'normal
			  :weight 'normal
			  :box nil)))
  ;; Cursor
  (set-cursor-color b-hg)

  ;; Syntax highlighting
  (set-face-attribute 'font-lock-comment-face nil
		      :foreground b-hg :background nil)
  (set-face-attribute 'font-lock-comment-delimiter-face nil
		      :foreground b-hg :background nil)
  (set-face-attribute 'font-lock-string-face nil
		      :foreground (color-darken-name b-fg 10))

  ;; Highlight
  (set-face-attribute 'highlight nil
		      :foreground nil :background b-hg)
  (set-face-attribute 'hl-line nil
		      :foreground nil :background (color-darken-name b-hg 30))
  (set-face-attribute 'region nil
		      :foreground nil :background (color-darken-name b-hg 30))
  (set-face-attribute 'isearch nil
		      :foreground nil :background b-fg)
  (set-face-attribute 'lazy-highlight nil
		      :foreground nil :background b-fg)

  ;; Modeline
  (set-face-attribute 'mode-line nil
		      :foreground b-hg :background nil)
  (set-face-attribute 'mode-line-inactive nil
		      :foreground (color-darken-name b-hg 15) :background nil)
  (set-face-attribute 'mode-line-highlight nil
		      :foreground b-hg :background nil)
  (set-face-attribute 'mode-line-buffer-id nil
		      :foreground nil :background nil)

  ;; Company
  (set-face-attribute 'company-tooltip nil
		      :foreground b-bg :background b-fg)
  (set-face-attribute 'company-tooltip-annotation nil
		      :foreground nil :background nil)
  (set-face-attribute 'company-tooltip-annotation-selection nil
		      :foreground nil :background nil)
  (set-face-attribute 'company-tooltip-common nil
		      :foreground b-hg :background b-fg)
  (set-face-attribute 'company-tooltip-selection nil
		      :foreground b-fg :background b-hg)

  ;; Helm
  (set-face-attribute 'helm-match nil
		      :foreground nil :background (color-darken-name b-hg 30))
  (set-face-attribute 'helm-match-item nil
		      :foreground nil :background (color-darken-name b-hg 30))
  (set-face-attribute 'helm-selection nil
		      :foreground nil :background (color-darken-name b-hg 30))
  (set-face-attribute 'helm-selection-line nil
		      :foreground nil :background (color-darken-name b-hg 30))

  ;; Org
  (set-face-attribute 'org-meta-line nil
		      :foreground (color-lighten-name b-hg 5) :background nil)
  (set-face-attribute 'org-level-1 nil
		      :foreground (color-lighten-name b-hg 10) :background nil)
  (set-face-attribute 'org-level-2 nil
		      :foreground (color-lighten-name b-hg 15) :background nil)
  (set-face-attribute 'org-level-3 nil
		      :foreground (color-lighten-name b-hg 20) :background nil)
  (set-face-attribute 'org-level-4 nil
		      :foreground (color-lighten-name b-hg 25) :background nil)
  (set-face-attribute 'org-level-5 nil
		      :foreground (color-lighten-name b-hg 30) :background nil)
  (set-face-attribute 'org-level-6 nil
		      :foreground (color-lighten-name b-hg 35) :background nil)
  (set-face-attribute 'org-level-7 nil
		      :foreground (color-lighten-name b-hg 40) :background nil)
  (set-face-attribute 'org-level-8 nil
		      :foreground (color-lighten-name b-hg 45) :background nil)
  (set-face-attribute 'org-block-begin-line nil
		      :foreground (color-lighten-name b-hg 5) :background nil)
  (set-face-attribute 'org-block-end-line nil
		      :foreground (color-lighten-name b-hg 5) :background nil)

  ;; Magit
  (set-face-attribute 'magit-diff-removed nil
		      :background (color-darken-name b-hg 35))
  (set-face-attribute 'magit-diff-removed-highlight nil
		      :background (color-darken-name b-hg 30))
  (set-face-attribute 'magit-diff-added nil
		      :background (color-lighten-name b-bg 15))
  (set-face-attribute 'magit-diff-added-highlight nil
		      :background (color-lighten-name b-bg 10))
