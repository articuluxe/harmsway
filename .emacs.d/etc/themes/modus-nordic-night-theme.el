;;; nordic-night-theme.el --- A darker, more colorful version of the lovely Nord theme built on top of Modus -*- lexical-binding: t -*-

;; Copyright (c) 2026 Ashton Wiersdorf

;; Title: Modus Nordic-Night Theme
;; Author: Ashton Wiersdorf <mail@wiersdorf.dev>
;; Created: 2026
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (modus-themes "5.1.0"))
;; SPDX-License-Identifier: MIT
;; Homepage: https://codeberg.org/ashton314/modus-nordic-night

;;; Commentary:
;; Modus Nordic-Night is a modification of the lovely Nord theme. It features a much
;; darker background and more liberal use of the color palliate to make your
;; buffer both more colorful as well as easier to read, whilst still retaining
;; the calm, harmonious tones of the Nord theme.
;;
;; This is a rework of my earlier Nordic-Night theme; this version is
;; built on top of the excellent Modus framework by Protesilaos Stavrou.

;;; Code:

(require 'modus-themes)

(defgroup nordic-night-themes ()
  "Dark, high-contrast themes built around the Modus-themes and with the Nord color palette."
  :group 'faces
  :group 'modus-themes
  :prefix "nordic-night-themes-")

(defconst nordic-night-dark-themes '(nordic-night nordic-midnight))

(modus-themes-register 'nordic-night)
(modus-themes-register 'nordic-midnight)

(defvar modus-nordic-night-palette
  (modus-themes-generate-palette
   `(
     ;; Background colors
     (bg-07 "#3f4f4f")
     (bg-08 "#3d5056")
     (bg-09 "#3b4551")
     (bg-10 "#233949")                  ; blue
     (bg-11 "#512e31")                  ; red
     (bg-12 "#573d35")                  ; orange
     (bg-13 "#61553d")                  ; yellow
     (bg-14 "#46503e")                  ; green
     (bg-15 "#4c3e4a")                  ; purple

     ;; Extended nord colors
     (nn-dark1 "#122a19d4233d"); (color-darken-name "#5e81ac" 80)
     (nn-dark2 "#1b4026be34dc"); (color-darken-name "#5e81ac" 70)
     (nn-dark3 "#245533a8467b"); (color-darken-name "#5e81ac" 60)
     (nn-light1 "#6b7386")
     (nn-light2 "#8892a4")
     (nn-light3 "#b5bdcc")

     ;; Main theme colors
     (bg-main "#121212")		; nn-dark1
     (bg-dim  "#18181a")		; nn-dark2
     (fg-main "#d8dee9")
     (fg-dim  "#8892a4")
     (fg-alt  "#b5bdcc")

     ;; Base theme colors
     (red "#bf616a")
     (green "#a3be8c")
     (yellow "#ebcb8b")
     (yellow-warmer "#d08770")
     (blue "#5e81ac")
     (blue-warmer "#81a1c1")
     (blue-faint "#8fbcbb")
     (cyan "#88c0d0")
     (magenta "#b48ead"))

   ;; Prefer cool colors
   'cool)
  "Color definitions for modus-nordic-night.")

(defconst modus-nordic-night-palette-partial
  `((bg-mode-line-active "#434c5e")
    (fg-mode-line-active fg-main)
    (bg-mode-line-inactive ,(color-darken-name "#5e81ac" 80))
    (fringe ,(color-darken-name "#434c5e" 80))
    (fg-mode-line-inactive blue)
    (border-mode-line-active unspecified)
    (border-mode-line-inactive unspecified)

    (bg-completion bg-10)
    (bg-paren-match bg-10)
    (bg-paren-expression bg-10)

    ;; (bg-red-nuanced     "#3a0c14")
    ;; (bg-yellow-nuanced  "#381d0f")
    ;; (bg-blue-nuanced    "#12154a")
    ;; (bg-magenta-nuanced "#2f0c3f")
    (bg-cyan-nuanced    "#042837")

    (bg-red-nuanced ,(color-saturate-name (color-darken-name "#512e31" 10) 10))
    (bg-orange-nuanced ,(color-saturate-name (color-darken-name "#573d35" 10) 10))
    (bg-yellow-nuanced ,(color-saturate-name (color-darken-name "#61553d" 10) 10))
    (bg-green-nuanced ,(color-saturate-name (color-darken-name "#46503e" 10) 10))
    (bg-magenta-nuanced ,(color-saturate-name (color-darken-name "#4c3e4a" 10) 10))

    (bg-added bg-green-nuanced)
    (bg-added-refine    "#022405")
    (bg-added-fringe    green)
    (fg-added           unspecified)
    (fg-added-intense   green-intense)

    (bg-completion bg-cyan-nuanced)
    (bg-hover bg-cyan-nuanced)
    (bg-hover-secondary bg-blue-nuanced)

    ;; (bg-active-value bg-10)
    (bg-active-value nn-dark3)
    (fg-active-value cyan)
    (bg-active-argument bg-main)
    (fg-active-argument yellow)

    (bg-mark-select bg-cyan-subtle)
    (fg-mark-select blue)

    (bg-changed         "#ffdfa9")
    ;; (bg-changed-faint   "#ffefbf")
    ;; (bg-changed-refine  "#fac090")
    ;; (bg-changed-fringe  "#d7c20a")
    (fg-changed         "#553d00")
    ;; (fg-changed-intense "#655000")

    ;; (bg-removed         "#ffd8d5")
    ;; (bg-removed-faint   "#ffe9e9")
    (bg-removed-refine  "#240205")
    ;; (bg-removed-fringe  "#d84a4f")
    ;; (fg-removed         "#8f1313")
    ;; (fg-removed-intense "#aa2222")

    ;; Misc. mappings
    (info green)

    ;; Code mapping
    (builtin blue-warmer) ; whatever
    (comment fg-dim)
    (constant blue-cooler)
    (fnname blue-warmer)
    (fnname-call blue-cooler)
    (keyword magenta)
    (preprocessor blue)
    (docstring green)
    (string green)
    (type green)
    (variable blue-warmer)
    (variable-use blue-cooler)
    (rx-escape yellow)
    (rx-construct yellow-warmer)
    (identifier blue)

    ;; Accent stuffs
    (accent-0 blue-warmer)
    (accent-1 green)
    (accent-2 blue-warmer)
    (accent-3 yellow)

    ;; Searching
    (bg-search-current bg-cyan-intense)
    (bg-search-static bg-blue-intense)
    (bg-search-lazy bg-blue-intense)
    (fg-search-current bg-main)
    (fg-search-static bg-main)          ; inverted text
    (fg-search-lazy cyan)

    ;; Tab bar
    (bg-tab-bar bg-mode-line-inactive)
    (bg-tab-current bg-mode-line-active)
    (bg-tab-other bg-tab-bar)

    ;; Region, current line
    (bg-region bg-08)
    (bg-hl-line ,(color-darken-name "#5e81ac" 75))
    (bg-line-number-active ,(color-darken-name "#5e81ac" 75))
    (bg-line-number-inactive ,(color-darken-name "#434c5e" 80))

    ;; Headings
    (fg-heading-0 blue)
    (fg-heading-1 blue-warmer)
    (fg-heading-2 blue-faint)
    (fg-heading-3 cyan)
    (fg-heading-3 magenta-cooler)
    (fg-heading-4 blue)
    (fg-heading-5 blue-warmer)
    (fg-heading-6 blue-faint)
    (fg-heading-7 cyan)
    (fg-heading-8 magenta-cooler)

    ;; Org-mode tweaks
    (fg-prose-verbatim green-cooler)
    )
  )

(defcustom modus-nordic-night-palette-overrides nil
  "Overrides for `modus-nordic-night-palette'."
  :group 'nordic-night-themes
  :type '(repeat (list symbol (choice symbol string))))

(defconst modus-nordic-night-custom-faces
  '(
    ;; Mode-line
    `(mode-line
      ((default :inherit modus-themes-ui-variable-pitch
                :width condensed
                :background ,bg-mode-line-active
                :foreground ,fg-mode-line-active)
       (((supports :box t))
        ,@(modus-themes--box border-mode-line-active nil nil))
       (t :underline ,border-mode-line-active)))
    `(mode-line-active
      ((default :inherit modus-themes-ui-variable-pitch
                :width condensed
                :background ,bg-mode-line-active
                :foreground ,fg-mode-line-active)
       (((supports :box t))
        ,@(modus-themes--box border-mode-line-active nil nil))
       (t :underline ,border-mode-line-active)))
    `(mode-line-inactive
      ((default :inherit modus-themes-ui-variable-pitch
                :width condensed
                :background ,bg-mode-line-inactive
                :foreground ,fg-mode-line-inactive)
       (((supports :box t))
        ,@(modus-themes--box border-mode-line-inactive nil nil))
       (t :underline ,border-mode-line-inactive)))

    ;; UI tweaks
    `(fringe ((,c :background ,fringe :foreground ,fg-dim)))
    `(tab-bar ((,c (:background ,bg-tab-bar :box (:line-width (1 . 2) :color "#1e2430")))))
    `(tab-bar-tab-inactive ((,c :foreground ,blue)))

    ;; Mu4e
    `(mu4e-header-face ((,c :foreground ,fg-alt)))

    ;; Misc. mode faces
    `(eglot-highlight-symbol-face ((,c (:background ,nn-dark2))))
    `(marginalia-documentation ((,c :inherit modus-themes-slant :foreground ,preprocessor)))

    ;; Avy
    `(avy-goto-char-timer-face ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-search-lazy :foreground ,fg-search-lazy)))
    `(avy-lead-face ((,c :inherit (bold modus-themes-reset-soft) :background ,red :foreground ,fg-search-current)))
    `(avy-lead-face-0 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-search-lazy :foreground ,fg-search-lazy)))
    `(avy-lead-face-1 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-search-lazy :foreground ,fg-search-lazy)))
    `(avy-lead-face-2 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-search-lazy :foreground ,fg-search-lazy)))

    ;; Transient
    `(transient-key-exit ((,c :foreground ,magenta-intense)))
    `(transient-heading ((,c :foreground ,blue)))

    ;; VC
    `(vc-edited-state ((,c :foreground ,yellow)))

    ;; corfu
    `(corfu-default ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim :extend t)))

    ;; custom

    )
  "Deep, direct face customizations that differ substantially from Modus")

(modus-themes-theme
 'modus-nordic-night
 'nordic-themes
 "Dark, high-contrast themes built around the Modus-themes and with the Nord color palette."
 'dark                                  ; this is a dark theme
 'modus-nordic-night-palette            ; base
 'modus-nordic-night-palette-partial    ; theme definitions
 'modus-nordic-night-palette-overrides  ; user-definable overrides
 'modus-nordic-night-custom-faces)      ; extra theme tweaks

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'modus-nordic-night-theme)
