;;; tokyonight-storm-theme.el --- tokyonight theme inspired by the neovim version -*- lexical-binding:t -*-

;; Commentary at file end

;;; Code:
(deftheme tokyonight-storm
  "Based on folke's tokyonight-storm-storm, through rawley for the emacs flavoring")

(let ((tokyonight-storm-bg "#24283b")
      (tokyonight-storm-bg-dark "#1f2335")
      (tokyonight-storm-bg-hl "#292e42")
      (tokyonight-storm-fg "#c0caf5")
      (tokyonight-storm-fg-1 "#a9b1d6")
      (tokyonight-storm-fg-2 "#3b4261")
      (tokyonight-storm-white "#ffffff")
      (tokyonight-storm-black "#414868")
      (tokyonight-storm-comment "#565f89")
      (tokyonight-storm-dark "#545c7e")
      (tokyonight-storm-dark+1 "#737aa2")
      (tokyonight-storm-blue-1 "#394b70")
      (tokyonight-storm-blue "#3d59a1")
      (tokyonight-storm-blue+1 "#7aa2f7")
      (tokyonight-storm-blue+2 "#2ac3d3")
      (tokyonight-storm-blue+3 "#0db9d7")
      (tokyonight-storm-blue+4 "#89ddff")
      (tokyonight-storm-blue+5 "#b4f9f8")
      (tokyonight-storm-cyan "#7dcfff")
      (tokyonight-storm-magenta "#bb9af7")
      (tokyonight-storm-magenta+1 "#ff007c")
      (tokyonight-storm-purple "#9d7cd8")
      (tokyonight-storm-orange "#ff9e64")
      (tokyonight-storm-yellow "#e0af68")
      (tokyonight-storm-green "#9ece6a")
      (tokyonight-storm-teal-1 "#41a6b5")
      (tokyonight-storm-teal "#1abc9c")
      (tokyonight-storm-teal+1 "#73daca")
      (tokyonight-storm-red-1 "#db4b4b")
      (tokyonight-storm-red "#f7768e"))
  (custom-theme-set-variables
   'tokyonight-storm
   '(frame-background-mode (quote dark)))

  (custom-theme-set-faces
   'tokyonight-storm

   ;; Uncategorized Coloring
   `(border ((t ,(list :background tokyonight-storm-bg
		       :foreground tokyonight-storm-bg-hl))))
   `(cursor ((t (:background ,tokyonight-storm-white))))
   `(default ((t ,(list :foreground tokyonight-storm-fg
			:background tokyonight-storm-bg))))
   `(fringe ((t ,(list :background tokyonight-storm-bg-dark
		       :foreground tokyonight-storm-fg-2))))
   `(link ((t (:foreground ,tokyonight-storm-dark+1 :underline t))))
   `(link-visited ((t (:foreground ,tokyonight-storm-dark+1 :underline t))))
   `(match ((t (:background ,tokyonight-storm-dark+1))))
   `(shadow ((t (:foreground ,tokyonight-storm-dark+1))))
   `(minibuffer-prompt ((t :foreground ,tokyonight-storm-magenta)))
   `(region ((t (:background ,tokyonight-storm-bg-hl :foreground nil))))
   `(secondary-selection ((t ,(list :background tokyonight-storm-bg-hl
				    :foreground nil))))
   `(trailing-whitespace ((t ,(list :foreground tokyonight-storm-white
				    :background tokyonight-storm-red))))
   `(tooltip ((t ,(list :background tokyonight-storm-bg-hl
			:foreground tokyonight-storm-fg))))
   `(success ((t (:foreground ,tokyonight-storm-green :weight bold))))
   `(warning ((t (:foreground ,tokyonight-storm-orange :weight bold))))
   `(tooltip ((t (:foreground ,tokyonight-storm-fg :background ,tokyonight-storm-fg))))
     
   ;; Calendar
   `(holiday-face ((t (:foreground ,tokyonight-storm-red-1))))

   ;; Compilation
   `(compilation-info ((t ,(list :foreground tokyonight-storm-green
				 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground tokyonight-storm-yellow
				    :bold t
				    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,tokyonight-storm-red))))
   `(compilation-mode-line-failt ((t ,(list :foreground tokyonight-storm-red
					    :weight 'bold
					    :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground tokyonight-storm-green
					   :weight 'bold
					   :inherit 'unspecified))))

   ;; Custom
   `(custom-state ((t (:foreground ,tokyonight-storm-green))))

   ;; Diff
   `(diff-removed ((t ,(list :foreground tokyonight-storm-red
			     :background nil))))
   `(diff-added ((t ,(list :foreground tokyonight-storm-green
			   :background nil))))

   ;; Dired
   `(dired-directory ((t (:foreground ,tokyonight-storm-magenta))))
   `(dired-ignored ((t ,(list :foreground tokyonight-storm-blue+5
			      :inherit 'unspecified))))

   ;; Add ido support
   `(ido-first-match ((t (:foreground ,tokyonight-storm-yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,tokyonight-storm-orange))))
   `(ido-subdir ((t (:foreground ,tokyonight-storm-green))))

   ;; eshell
   `(eshell-ls-backup ((t (:foreground ,tokyonight-storm-orange))))
   `(eshell-ls-directory ((t (:foreground ,tokyonight-storm-blue))))
   `(eshell-ls-executable ((t (:foreground ,tokyonight-storm-green))))
   `(eshell-ls-symlink ((t (:foreground ,tokyonight-storm-yellow))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide 'tokyonight-storm-theme)
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
