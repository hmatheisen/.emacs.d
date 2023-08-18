;;; nry-text.el --- Text editing configs             -*- lexical-binding: t; -*-

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

;; Contrary to what many Emacs users have in their configs, you don't need
;; more than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

;; Enable downcase/upcase region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Enable set goal column
(put 'set-goal-column 'disabled nil)

;; Replace active region when typing
(delete-selection-mode t)

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


;; IEdit for interactive multi edit
(use-package iedit :ensure t)


;; Whitespace cleanup
(add-hook 'before-save-hook 'whitespace-cleanup)
;; Change whitespace style for markdown files
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local
             whitespace-style
             (delq 'trailing whitespace-style))))

(provide 'nry-text)
;;; nry-text.el ends here
