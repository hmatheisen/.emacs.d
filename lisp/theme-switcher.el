;;; theme-switcher.el --- Theme config                    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Henry MATHEISEN

;; Author: Henry MATHEISEN <haineriz@posteo.de>
;; Keywords:

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

;;; Code:

(defgroup theme-switcher nil
  "Automatically switch theme between day and night.")

(define-minor-mode theme-switcher-mode
  "Automatically switch theme between day and night."
  :global t
  :group 'theme-switcher
  (if theme-switcher-mode
      (progn
        (theme-switcher--create-timer)
        (theme-switcher--switch-theme))
    (theme-switcher--delete-timer)))

(defcustom theme-switcher-day-theme nil
  "The light theme to switch to during the day."
  :type 'symbol
  :group 'theme-switcher)

(defcustom theme-switcher-night-theme nil
  "The dark theme to switch to during the day."
  :type 'symbol
  :group 'theme-switcher)

(defcustom theme-switcher-day-start 08
  "The hour when the theme goes from dark to light in the morning.
Default is 8am."
  :type 'integer
  :group 'theme-switcher)

(defcustom theme-switcher-night-start 16
  "The hour when the theme goes from light to dark in the evening.
Default is 5pm."
  :type 'integer
  :group 'theme-switcher)

(defvar theme-switcher--timer nil
  "Timer that runs the switcher function.")

(defun theme-switcher--load-theme (theme)
  "Disable all enabled themes except THEME then load THEME if not already loaded."
  (if-let ((themes-to-disable (remove theme custom-enabled-themes)))
      (dolist (to-disable themes-to-disable)
        (disable-theme to-disable)))
  (unless (or (member theme custom-enabled-themes) (not theme))
    (load-theme theme)))

(defun theme-switcher--switch-theme ()
  "Compute the current time and load the appropriate theme."
  (let ((now (string-to-number (format-time-string "%H"))))
    (if (and (>= now theme-switcher-day-start) (< now theme-switcher-night-start))
        (theme-switcher--load-theme theme-switcher-day-theme)
      (theme-switcher--load-theme theme-switcher-night-theme))))

(defun theme-switcher--create-timer ()
  "Set a timer to check every minute if the theme needs to be changed."
  (setq theme-switcher--timer (run-with-timer 0 (* 1 60) 'theme-switcher--switch-theme)))

(defun theme-switcher--delete-timer ()
  "Cancel the timer and reset the timer variable."
  (cancel-timer theme-switcher--timer)
  (setq theme-switcher--timer nil))


;; Interactive functions to make theme manipulation easier.
(defun theme-switcher-toggle ()
  "Toggle between the day and night theme."
  (interactive)
  ;; swap `day-theme' and `night-theme' variables
  (setq theme-switcher-day-theme
        (prog1 theme-switcher-night-theme
          (setq theme-switcher-night-theme
                theme-switcher-day-theme)))
  ;; Do the switch
  (theme-switcher--switch-theme))

(provide 'theme-switcher)

;;; theme-switcher.el ends here
