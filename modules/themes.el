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

;; Set when to change
(setq day-hour 9)
(setq night-hour 16)

;; Set themes
(setq day-theme 'modus-operandi)
(setq night-theme 'modus-vivendi)

(provide 'themes)

;;; themes.el ends here
