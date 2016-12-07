;;; desert-theme.el --- A port of a well-known VIM theme.

;; Copyright (C) Sergei Lebedev
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;; Author: Sergei Lebedev <superbobry@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet


(deftheme desert
  "A port of a well-known VIM theme.")

;; Please, install rainbow-mode
;; Colors with +x are lighter. Colors with -x are darker.
(let ((desert-fg            "ghost white")
      (desert-bg-1          "gray15")
      (desert-bg            "gray20")
      (desert-bg+1          "gray40")
      (desert-bg+2          "gray60")
      (desert-yellow-1      "yellow2")
      (desert-yellow        "yellow")
      (desert-khaki         "khaki")
      (desert-olive         "OliveDrab")
      (desert-green         "PaleGreen3")
      (desert-blue          "LightSkyBlue3")
      (desert-pink          "#ffa0a0")
      (desert-red           "IndianRed3")
      (desert-warning-fg    "goldenrod"))
  (custom-theme-set-variables
   'desert
   '(frame-background-mode (quote dark)))

  (custom-theme-set-faces
   'desert
   `(default ((t (:foreground ,desert-fg :background ,desert-bg))))
   `(cursor ((t (:background ,desert-khaki))))
   `(fringe ((t (:background ,desert-bg))))
   `(font-lock-builtin-face ((t (:foreground ,desert-red))))
   `(font-lock-comment-face ((t (:foreground ,desert-blue))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,desert-blue))))
   `(font-lock-doc-face ((t (:foreground ,desert-red))))
   `(font-lock-keyword-face ((t (:foreground ,desert-khaki :bold t))))
   `(font-lock-string-face ((t (:foreground ,desert-pink))))
   `(font-lock-type-face ((t (:foreground ,desert-green))))
   `(font-lock-variable-name-face ((t (:foreground ,desert-fg))))
   `(font-lock-warning-face ((t (:foreground ,desert-warning-fg :bold t :inherit nil))))
   `(font-lock-function-name-face ((t (:foreground ,desert-green :bold t))))
   `(font-lock-constant-face ((t (:foreground ,desert-pink))))

   `(minibuffer-prompt ((t (:foreground ,desert-khaki :bold t))))
   `(Buffer-menu-buffer ((t (:foreground ,desert-khaki))))
   `(header-line ((t (:background ,desert-bg-1 :box (:color ,desert-bg :line-width 2)))))
   `(mode-line ((t (:inherit header-line :foreground ,desert-bg+2 :background ,desert-bg-1))))
   `(mode-line-inactive ((t (:inherit mode-line))))
   `(mode-line-buffer-id ((t (:foreground ,desert-warning-fg :bold t))))

   `(linum ((t (:foreground ,desert-yellow :background ,desert-bg))))
   `(highlight ((t (:foreground ,desert-khaki :background ,desert-olive))))
   `(region ((t (:foreground ,desert-khaki :background ,desert-olive))))
   `(show-paren-mismatch ((t (:foreground ,desert-red :background ,desert-bg :bold t))))
   `(show-paren-match ((t (:foreground ,desert-fg :background "darkcyan" :bold t))))
   `(trailing-whitespace ((t (:background nil :inherit font-lock-warning-face))))
   `(match ((t (:weight bold))))

   ;; dired
   `(dired-directory ((t (:foreground ,desert-khaki))))

   ;; link
   `(link ((t (:background ,desert-bg :foreground ,desert-red :bold t :underline nil))))
   `(link-visited ((t (:inherit link :bold nil))))

   ;; isearch
   `(isearch ((t (:foreground ,desert-khaki :background ,desert-olive))))
   `(isearch-lazy-light ((t (:background ,desert-bg :foreground ,desert-fg :bold t))))

   ;; compilation
   `(compilation-info ((t (:foreground ,desert-green :bold t :inherit nil))))
   `(compilation-warning ((t (:foreground ,desert-khaki :bold t :inherit nil))))

   ;; jabber.el
   `(jabber-roster-user-chatty ((t (:inherit font-lock-type-face :bold t))))
   `(jabber-roster-user-online ((t (:inherit font-lock-keyword-face :bold t))))
   `(jabber-roster-user-offline ((t (:foreground ,desert-bg+1 :background ,desert-bg))))
   `(jabber-roster-user-away ((t (:inherit font-lock-doc-face))))
   `(jabber-roster-user-xa ((t (:inherit font-lock-doc-face))))
   `(jabber-roster-user-dnd ((t (:inherit font-lock-comment-face))))
   `(jabber-roster-user-error ((t (:inherit font-lock-warning-face))))

   `(jabber-title-small ((t (:height 1.2 :weight bold))))
   `(jabber-title-medium ((t (:inherit jabber-title-small :height 1.2))))
   `(jabber-title-large ((t (:inherit jabber-title-medium :height 1.2))))

   `(jabber-chat-prompt-local ((t (:inherit font-lock-string-face :bold t))))
   `(jabber-chat-prompt-foreign ((t (:inherit font-lock-function-name-face :bold nil))))
   `(jabber-chat-prompt-system ((t (:inherit font-lock-comment-face :bold t))))
   `(jabber-rare-time-face ((t (:inherit font-lock-function-name-face :bold nil))))

   `(jabber-activity-face ((t (:inherit jabber-chat-prompt-foreign))))
   `(jabber-activity-personal-face ((t (:inherit jabber-chat-prompt-local :bold t))))

   ;; ido
   `(ido-first-match ((t (:foreground ,desert-green :bold t))))
   `(ido-only-match ((t (:foreground ,desert-green :bold t))))
   `(ido-subdir ((t (:foreground ,desert-khaki :bold t))))

   ;; auto-complete
   `(ac-candidate-face ((t (:background ,desert-bg-1 :foreground ,desert-fg))))
   `(ac-selection-face ((t (:inherit highlight))))
   `(ac-completion-face ((t (:inherit ac-selection-face))))

   ;; elscreen
   `(elscreen-tab-background-face ((t (:background ,desert-bg-1))))
   `(elscreen-tab-other-screen-face
     ((t (:background ,desert-bg-1 :foreground ,desert-bg+2))))
   `(elscreen-tab-current-screen-face
     ((t (:background ,desert-bg-1 :foreground ,desert-warning-fg :bold t))))
   `(elscreen-tab-control-face
     ((t (:inherit elscreen-tab-current-screen-face :underline nil))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'desert)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; desert-theme.el ends here
