;;; he-path.el --- Add new paths to Emacs PATH       -*- lexical-binding: t; -*-

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

;; Add new paths to the internal PATH variable inside Emacs.  This is
;; often an issue since Emacs does not read the PATH from your system,
;; a number of binaries cannot be accessed.

;;; Code:

(defun he/add-to-path (path)
  "Add a path to `exec-path' and Emacs \"PATH\" variable."
  (add-to-list 'exec-path path)
  (setenv "PATH" (concat (getenv "PATH") ":" path)))

(he/add-to-path "/usr/local/bin")
(he/add-to-path "/Library/TeX/texbin")
(he/add-to-path "~/.rbenv/shims")

(provide 'he-path)
;;; he-path.el ends here
