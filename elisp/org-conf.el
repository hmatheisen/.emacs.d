;;; org-conf.el --- Org mode settings                     -*- lexical-binding: t; -*-

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

(require 'org)
(require 'org-indent)

(defun hma/org-mode-hook ()
  "My org mode hook."
  (org-indent-mode)
  (visual-line-mode))

(add-hook 'org-mode-hook 'hma/org-mode-hook)

(keymap-global-set "C-c l" 'org-store-link)
(keymap-global-set "C-c a" 'org-agenda)
(keymap-global-set "C-c c" 'org-capture)

(setopt org-directory (file-name-as-directory (expand-file-name "~/org"))
        org-agenda-files (list org-directory)
        org-capture-templates
        '(("i" "Inbox" entry (file "inbox.org")
           "* TODO %?\n"
           :prepend t :empty-lines-before 1))
        org-todo-keywords
        '((sequence "TODO(t)" "ON GOING(g)" "HOLD(h)" "|" "DONE(d)")))

(provide 'org-conf)

;;; org-conf.el ends here
