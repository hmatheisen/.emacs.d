;;; utils.el --- Function utils for Emacs init       -*- lexical-binding: t; -*-

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

;; Some utility functions that I may use.

;;; Code:

(defun add-to-path (path)
  "Add a path to variable `exec-path' and Emacs \"PATH\" variable."
  (add-to-list 'exec-path path)
  (setenv "PATH" (concat (getenv "PATH") ":" path)))

(add-to-path "/usr/local/bin")
(add-to-path "/Library/TeX/texbin")
(add-to-path "/Users/henry/.rbenv/shims")
(add-to-path "/Users/henry/.local/bin")

(defun macroexpand-point (sexp)
  "Expand macro SEXP at point to temp buffer."
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

(provide 'utils)
;;; utils.el ends here
