;;; text.el --- Text manipulation                      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Henry MATHEISEN

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

;; Make use of the align function to target specific
;; regions/characters

;;; Code:

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; Enable downcase/upcase region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Replace active region when typing
(delete-selection-mode +1)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode t)

;; Auto pair on prog mode
(add-hook 'prog-mode-hook 'electric-pair-mode)


;; Text Functions

(defun align-equals (beg end)
  "Align `=' signs in a given region, from BEG to END."
  (interactive "r")
  (align-regexp beg
                end
                "\\(\\s-*\\)="))


(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(provide 'text)

;;; text.el ends here
