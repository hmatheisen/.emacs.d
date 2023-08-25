;;; nry-misc.el --- Miscellaneous settings           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Henry MATHEISEN

;; Author: Henry MATHEISEN <henry@macbook>
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

;;

;;; Code:

(setq select-enable-clipboard t)

(setq trash-directory "~/.Trash"
      delete-by-moving-to-trash t)

;; Indent using spaces
(setq-default indent-tabs-mode nil)
;; Set tabs to 2
(setq-default tab-width 2)

;; Make that damn bell shut up
(setq ring-bell-function 'ignore)

;; Default truncate lines
(setq-default truncate-lines t)

;; Unbind suspend keys
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Answer by y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; Backups live in a specific place
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/.backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/.auto-saves" t)))

;; Confirm before leaving
(setq confirm-kill-emacs #'yes-or-no-p)

;; Split Ediff horizontally
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Set mail address
(setq user-mail-address "haineriz@posteo.de")

;; Enable recentf mode
(recentf-mode)

(provide 'nry-misc)
;;; nry-misc.el ends here
