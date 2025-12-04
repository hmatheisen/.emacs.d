;;; keymaps.el --- Keymap settings                   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Henry MATHEISEN

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

;; Global keymaps
(keymap-global-set "M-/" 'hippie-expand)
(keymap-global-set "M-j" 'join-line)

(keymap-global-set "s-p" project-prefix-map)
(keymap-global-set "s-b" 'switch-to-buffer)
(keymap-global-set "s-r" 'recentf)
(keymap-global-set "s-t" 'treemacs)

(keymap-global-set "C-x C-b" 'ibuffer)

(keymap-global-set "s-Z" 'undo-redo)

;; In GUI mode, `C-z' hides the window and I hate it/keep accidentally hitting
;; it.
(keymap-global-set "C-z" nil)

(keymap-global-set "C-<wheel-up>" nil)
(keymap-global-set "C-<wheel-down>" nil)

(keymap-set dired-mode-map "C-c C-q" 'wdired-change-to-wdired-mode)

(put 'help-fns-edit-variable 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'scroll-left 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'keymaps)
;;; keymaps.el ends here
