;;; themes.el --- Theme switcher and themes settings                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Henry MATHEISEN

;; Author: Henry MATHEISEN <haineriz@posteo.de>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; I use this code to switch from one theme to another at desired
;; hours.  This is useful if, like me, you like a light theme during
;; the day and a dark theme at night.
;;
;; This can be used a standalone package.  If desired, you only need to
;; set ther variables yourself.  See "My own config page".
;;
;; A minor mode could be provided to enable/disable the switch.  Also
;; the time could be more precise than just the hour

;;; Code:

(defvar day-theme nil
  "The light theme to switch to during the day.
example : (setq light-theme 'spacemacs-light)")

(defvar night-theme nil
  "The dark theme to switch to during the day.
example : (setq dark-theme 'spacemacs-dark)")

(defvar day-hour 08
  "The hour when the theme goes from dark to light in the morning.
Default is 8am.
example : (setq morning-hour 07) for 8am")

(defvar night-hour 17
  "The hour when the theme goes from light to dark in the evening.
Default is 5pm.
example : (setq evening-hour 18) for 17pm")

(defun list-available-themes ()
  "Theme list to display for interactive selection."
  (list (intern (completing-read "Theme: " (custom-available-themes)))))

(defun switch-to-theme (theme)
  "Disable all themes and load the theme THEME if it exists.
Otherwise apply the default theme."
  (unless (member theme custom-enabled-themes)
    (while custom-enabled-themes
      (disable-theme (car custom-enabled-themes)))
    (if theme
        (load-theme theme t))))

;; This the main function, it is called periodically to automatically
;; switch in the morning/evening.
(defun theme-switcher ()
  "Switch themes depending on the hour of the day."
  (let ((now (string-to-number (format-time-string "%H"))))
    (if (and (>= now day-hour) (< now night-hour))
        ;; Between day and night hours: day theme
        (switch-to-theme day-theme)
      ;; Night time
      (switch-to-theme night-theme))
    nil))


;; Interactive functions to make theme manipulation easier.

(defun change-day-theme (theme)
  "Change the day theme interactively to THEME."
  (interactive (list-available-themes))
  (setq day-theme theme)
  (theme-switcher))

(defun change-night-theme (theme)
  "Change the night theme interactively to THEME."
  (interactive (list-available-themes))
  (setq night-theme theme)
  (theme-switcher))

(defun toggle-theme ()
  "Toggle between the day and night theme."
  (interactive)
  ;; swap `day-theme' and `night-theme' variables
  (setq day-theme (prog1 night-theme (setq night-theme day-theme)))
  (theme-switcher))

(setq switcher-timer (run-with-timer 0 (* 1 60) 'theme-switcher))

(defun cancel-theme-switcher ()
  "Cancel timer for theme switcher."
  (interactive)
  (cancel-timer switcher-timer))


;; My own config

(use-package modus-themes
  :ensure
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-subtle-line-numbers nil
        modus-themes-intense-mouseovers t
        modus-themes-deuteranopia nil
        modus-themes-tabs-accented t
        modus-themes-variable-pitch-ui nil
        modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

        modus-themes-fringes nil ; {nil,'subtle,'intense}

        ;; Options for `modus-themes-lang-checkers' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `straight-underline', `text-also', `background',
        ;; `intense' OR `faint'.
        modus-themes-lang-checkers nil

        ;; Options for `modus-themes-mode-line' are either nil, or a list
        ;; that can combine any of `3d' OR `moody', `borderless',
        ;; `accented', a natural number for extra padding (or a cons cell
        ;; of padding and NATNUM), and a floating point for the height of
        ;; the text relative to the base font size (or a cons cell of
        ;; height and FLOAT)
        modus-themes-mode-line '(accented)

        ;; Options for `modus-themes-markup' are either nil, or a list
        ;; that can combine any of `bold', `italic', `background',
        ;; `intense'.
        modus-themes-markup nil

        ;; Options for `modus-themes-syntax' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
        modus-themes-syntax nil

        ;; Options for `modus-themes-hl-line' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `accented', `underline', `intense'
        modus-themes-hl-line '(intense)

        ;; Options for `modus-themes-paren-match' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `bold', `intense', `underline'
        modus-themes-paren-match '(bold intense)

        ;; Options for `modus-themes-links' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
        ;; `bold', `italic', `background'
        modus-themes-links '(neutral-underline background)

        ;; Options for `modus-themes-box-buttons' are either nil (the
        ;; default), or a list that can combine any of `flat', `accented',
        ;; `faint', `variable-pitch', `underline', the symbol of any font
        ;; weight as listed in `modus-themes-weights', and a floating
        ;; point number (e.g. 0.9) for the height of the button's text.
        modus-themes-box-buttons '(variable-pitch flat faint 0.9)

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        modus-themes-prompts '(intense bold)

        ;; The `modus-themes-completions' is an alist that reads three
        ;; keys: `matches', `selection', `popup'.  Each accepts a nil
        ;; value (or empty list) or a list of properties that can include
        ;; any of the following (for WEIGHT read further below):
        ;;
        ;; `matches' - `background', `intense', `underline', `italic', WEIGHT
        ;; `selection' - `accented', `intense', `underline', `italic', `text-also' WEIGHT
        ;; `popup' - same as `selected'
        ;; `t' - applies to any key not explicitly referenced (check docs)
        ;;
        ;; WEIGHT is a symbol such as `semibold', `light', or anything
        ;; covered in `modus-themes-weights'.  Bold is used in the absence
        ;; of an explicit WEIGHT.
        modus-themes-completions '((matches . (extrabold))
                                   (selection . (semibold accented))
                                   (popup . (accented intense)))

        modus-themes-mail-citations nil ; {nil,'intense,'faint,'monochrome}

        ;; Options for `modus-themes-region' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `no-extend', `bg-only', `accented'
        modus-themes-region '(bg-only no-extend)

        ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
        modus-themes-diffs 'desaturated

        modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

        modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        '((header-block . (variable-pitch 1.3))
          (header-date . (grayscale workaholic bold-today 1.1))
          (event . (accented varied))
          (scheduled . uniform)
          (habit . traffic-light))

        modus-themes-headings ; this is an alist: read the manual or its doc string
        '((1 . (variable-pitch 1.3))
          (2 . (rainbow 1.1))
          (t . (semibold))))
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes))

;; Set when to change
(setq day-hour 11)
(setq night-hour 16)

;; Set themes
(setq day-theme 'modus-operandi)
(setq night-theme 'modus-vivendi)

(provide 'themes)

;;; themes.el ends here
