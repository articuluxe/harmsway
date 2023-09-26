;; nordic-midnight-theme.el --- A pitch-black, more colorful version of the lovely Nord theme -*- lexical-binding: t -*-

;; Copyright (c) 2023 Ashton Wiersdorf

;;; Commentary:

;; Nordic-Midnight is an even darker version of the nordic-night theme.

;;; Code:

(require 'nordic-night-theme)

(deftheme nordic-midnight "A pitch-black, more colorful version of the lovely Nord theme.")

(defgroup nordic-midnight nil
  "Nordic-Midnight theme customizations.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defvar nordic-midnight-nord-colors
  '(:nord00 "#1e2430"
    :nord01 "#2e3440"
    :nord02 "#3b4252"
    :nord03 "#434c5e"
    :nord04 "#d8dee9"
    :nord05 "#e5e9f0"
    :nord06 "#eceff4"
    :nord07 "#8fbcbb"
    :nord08 "#88c0d0"
    :nord09 "#81a1c1"
    :nord10 "#5e81ac"
    :nord11 "#bf616a"
    :nord12 "#d08770"
    :nord13 "#ebcb8b"
    :nord14 "#a3be8c"
    :nord15 "#b48ead")
  "Darker Nord colors for full-color displays")

(defvar nordic-midnight-nord-colors-shell
  '(:nord00 "#262626"
    :nord01 "#303030"
    :nord02 "#3a3a3a"
    :nord03 "#444444"
    :nord04 "#d0d0d0"
    :nord05 "#e4e4e4"
    :nord06 "#eeeeee"
    :nord07 "#008787"
    :nord08 "#00afd7"
    :nord09 "#00afff"
    :nord10 "#0087af"
    :nord11 "#df005f"
    :nord12 "#d75f00"
    :nord13 "#d7af00"
    :nord14 "#87af5f"
    :nord15 "#af87af")
  "Darker Nord colors for limited-color displays")

(defvar nordic-midnight-auroa-bg
  '(:nord07b "#3f4f4f"
    :nord08b "#3d5056"
    :nord09b "#3b4551"
    :nord10b "#233949"
    :nord11b "#512e31"
    :nord12b "#573d35"
    :nord13b "#61553d"
    :nord14b "#46503e"
    :nord15b "#4c3e4a")
  "Darker Nord colors tuned for background")

(defvar nordic-midnight-colors
  '(:nn00 "#000000"
    :nn01 "#121212"
    :nn02 "#181818"
    :nn03 "#6B7386"
    :nn04 "#8892A4"
    :nn05 "#B5BDCC")
  "Nordic midnight colors for full-color displays")

(defvar nordic-midnight-colors-shell
  '(:nn00 "#000000"
    :nn01 "#121212"
    :nn02 "#1c1c1c"
    :nn03 "#6c6c6c"
    :nn04 "#8a8a8a"
    :nn05 "#c6c6c6")
  "Nordic midnight colors for limited-color displays")

(defvar nordic-midnight-auroa-bg-shell
  '(:nord07b "#3a4e4e"
    :nord08b "#3a4e58"
    :nord09b "#3a444e"
    :nord10b "#263a4e"
    :nord11b "#582630"
    :nord12b "#583a30"
    :nord13b "#62583a"
    :nord14b "#444e3a"
    :nord15b "#4e3a4e")
  "Darker Nord colors for limited-color displays tuned for background")

(nordic-night--build-theme 'nordic-midnight
                           (if (nordic-night--fullcolorp)
                               (append nordic-midnight-nord-colors nordic-midnight-colors nordic-midnight-auroa-bg)
                             (append nordic-midnight-nord-colors-shell nordic-midnight-colors-shell nordic-midnight-auroa-bg-shell)))

(provide-theme 'nordic-midnight)

(provide 'nordic-midnight-theme)
