;;; window.el --- Window utilities for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Henry MATHEISEN

;; Author: Henry MATHEISEN <henry.mthsn@gmail.com>

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

(defun split-window-right-focus ()
  "Splits window on the right then focus on that window"
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-window-below-focus ()
  "Splits windmow below then focus on that window"
  (interactive)
  (split-window-below)
  (other-window 1))

;; Use by own split functions
(global-set-key (kbd "C-x 2") 'split-window-below-focus)
(global-set-key (kbd "C-x 3") 'split-window-right-focus)

;; Move between windows
(defun other-window-backward ()
  "Move to previous window"
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-backward)

;; Still enable Windmove for easy navigation
(windmove-default-keybindings)

;; Resizing
(global-set-key (kbd "M--") 'shrink-window)
(global-set-key (kbd "M-+") 'enlarge-window)
(global-set-key (kbd "C--") 'shrink-window-horizontally)
(global-set-key (kbd "C-+") 'enlarge-window-horizontally)

;; Move between buffers
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)

;; Winner mode to revert window conf
(winner-mode t)


(use-package ace-window
  :defer t
  :config (global-set-key (kbd "C-x o") 'ace-window))

(provide 'window)
;;; window.el ends here
