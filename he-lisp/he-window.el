;;; he-window.el --- Window utilities for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Henry MATHEISEN

;; Author: Henry MATHEISEN <henry.mthsn@gmail.com>
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

;; Some utilities regarding window manipulation in Emacs.  Default
;; behavior sometimes seem to come from another a planet where the
;; people have 27 fingers by hands.

;;; Code:

(defun he-split-window-right ()
  "Splits window on the right then focus on that window"
  (interactive)
  (split-window-right)
  (other-window 1))

(defun he-split-window-below ()
  "Splits windmow below then focus on that window"
  (interactive)
  (split-window-below)
  (other-window 1))

;; Use by own split functions
(global-set-key (kbd "C-x 2") 'he-split-window-below)
(global-set-key (kbd "C-x 3") 'he-split-window-right)

;; Move between windows
(defun other-window-backward ()
  "Move to previous window"
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-backward)

(provide 'he-window)
;;; he-window.el ends here
