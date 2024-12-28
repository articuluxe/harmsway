;;; tokyonight-moon-theme.el --- tokyonight theme inspired by the neovim version -*- lexical-binding:t -*-

;; Commentary at file end

;;; Code:
(deftheme tokyonight-moon
  "Based on folke's tokyonight-moon, through rawley for the emacs flavoring")
(let ((tokyonight-moon-bg "#222436")
      (tokyonight-moon-bg-dark "#1e2030")
      (tokyonight-moon-bg-hl "#2f334d")
      (tokyonight-moon-fg "#c8d3f5")
      (tokyonight-moon-fg-1 "#828bb8")
      (tokyonight-moon-fg-2 "#3b4261")
      (tokyonight-moon-white "#ffffff")
      (tokyonight-moon-black "#444a73")
      (tokyonight-moon-comment "#636da6")
      (tokyonight-moon-dark "#545c7e")
      (tokyonight-moon-dark+1 "#737aa2")
      (tokyonight-moon-blue-1 "#394b70")
      (tokyonight-moon-blue "#3e68d7")
      (tokyonight-moon-blue+1 "#82aaff")
      (tokyonight-moon-blue+2 "#65bcff")
      (tokyonight-moon-blue+3 "#0db9d7")
      (tokyonight-moon-blue+4 "#89ddff")
      (tokyonight-moon-blue+5 "#b4f9f8")
      (tokyonight-moon-cyan "#86e1fc")
      (tokyonight-moon-magenta "#c099ff")
      (tokyonight-moon-magenta+1 "#ff007")
      (tokyonight-moon-purple "#fca7ea")
      (tokyonight-moon-orange "#ff966c")
      (tokyonight-moon-yellow "#ffc777")
      (tokyonight-moon-green "#c3e88d")
      (tokyonight-moon-teal-1 "#41a6b5")
      (tokyonight-moon-teal "#4fd6be")
      (tokyonight-moon-teal+1 "#4fd6be")
      (tokyonight-moon-red-1 "#c53b53")
      (tokyonight-moon-red "#ff757f"))
  (custom-theme-set-variables
   'tokyonight-moon
   '(frame-background-mode (quote dark)))

  (custom-theme-set-faces
   'tokyonight-moon

   ;; Uncategorized Coloring
   `(border ((t ,(list :background tokyonight-moon-bg
		       :foreground tokyonight-moon-bg-hl))))
   `(cursor ((t (:background ,tokyonight-moon-white))))
   `(default ((t ,(list :foreground tokyonight-moon-fg
			:background tokyonight-moon-bg))))
   `(fringe ((t ,(list :background tokyonight-moon-bg-dark
		       :foreground tokyonight-moon-fg-2))))
   `(link ((t (:foreground ,tokyonight-moon-dark+1 :underline t))))
   `(link-visited ((t (:foreground ,tokyonight-moon-dark+1 :underline t))))
   `(match ((t (:background ,tokyonight-moon-dark+1))))
   `(shadow ((t (:foreground ,tokyonight-moon-dark+1))))
   `(minibuffer-prompt ((t :foreground ,tokyonight-moon-magenta)))
   `(region ((t (:background ,tokyonight-moon-bg-hl :foreground nil))))
   `(secondary-selection ((t ,(list :background tokyonight-moon-bg-hl
				    :foreground nil))))
   `(trailing-whitespace ((t ,(list :foreground tokyonight-moon-white
				    :background tokyonight-moon-red))))
   `(tooltip ((t ,(list :background tokyonight-moon-bg-hl
			:foreground tokyonight-moon-fg))))
   `(success ((t (:foreground ,tokyonight-moon-green :weight bold))))
   `(warning ((t (:foreground ,tokyonight-moon-orange :weight bold))))
   `(tooltip ((t (:foreground ,tokyonight-moon-fg :background ,tokyonight-moon-fg))))
     
   ;; Calendar
   `(holiday-face ((t (:foreground ,tokyonight-moon-red-1))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground tokyonight-moon-green
				 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground tokyonight-moon-yellow
				    :bold t
				    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,tokyonight-moon-red))))
   `(compilation-mode-line-failt ((t ,(list :foreground tokyonight-moon-red
					    :weight 'bold
					    :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground tokyonight-moon-green
					   :weight 'bold
					   :inherit 'unspecified))))

   ;; Custom
   `(custom-state ((t (:foreground ,tokyonight-moon-green))))

   ;; Diff
   `(diff-removed ((t ,(list :foreground tokyonight-moon-red
			     :background nil))))
   `(diff-added ((t ,(list :foreground tokyonight-moon-green
			   :background nil))))

   ;; Dired
   `(dired-directory ((t (:foreground ,tokyonight-moon-magenta))))
   `(dired-ignored ((t ,(list :foreground tokyonight-moon-blue+5
			      :inherit 'unspecified))))

   ;; Add ido support
   `(ido-first-match ((t (:foreground ,tokyonight-moon-yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,tokyonight-moon-orange))))
   `(ido-subdir ((t (:foreground ,tokyonight-moon-green))))

   ;; eshell
   `(eshell-ls-backup ((t (:foreground ,tokyonight-moon-orange))))
   `(eshell-ls-directory ((t (:foreground ,tokyonight-moon-blue))))
   `(eshell-ls-executable ((t (:foreground ,tokyonight-moon-green))))
   `(eshell-ls-symlink ((t (:foreground ,tokyonight-moon-yellow))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide 'tokyonight-moon-theme)
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;; Notes
;; Folke's current version (as of 2024-11-25) includes git colours, there aren't
;; options for that in Rawley's so I've not added it.

;; TODO: add git diff colors

;; Sources
;; Author: Rawley Fowler <rawleyfowler@gmail.com>
;; URL: https://github.com/rawleyfowler/tokyonight-emacs

;; You can find the neovim version here: https://github.com/folke/tokyonight.nvim

;;; tokyo-theme.el ends here
