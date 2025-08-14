;;; emacsocil.el --- Create window configurations and run processes in them  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Henry MATHEISEN

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

;;

;;; Code:

(require 'cl-lib)

;; TODO: Create more layouts
(defvar emacsocil-layouts '(:main-vertical))

(defun emacsocil--main-vertical (procs proc-fn)
  "Run all PROCS using PROC-FN in the main-vertical layout."
  (when (length< procs 3)
    (error "Cannot use :main-vertical layout with more thant 3 procs"))
  (delete-other-windows)
  (dolist (proc procs)
    (let* ((position (cl-position proc procs))
           (window (cond ((= position 0) (selected-window))
                         ((= position 1) (split-window-right nil (car (window-list))))
                         ((= position 2) (split-window-below nil (cadr (window-list)))))))
      (with-selected-window window
        (funcall proc-fn proc)))))

(cl-defmacro define-layout (name &optional documentation &key procs layout proc-fn project-path)
  "Create a new layout called NAME.
Split the current frame using a predefined LAYOUT and run all PROCS with PROC-FN."
  (declare (doc-string 2)
           (indent defun))
  (unless (member layout emacsocil-layouts)
    (error (concat "Unknonwn layout name: " (symbol-name layout))))
  (let ((layout-fn (cl-case layout
                     (:main-vertical #'emacsocil--main-vertical))))
    `(defun ,name ()
       ,documentation
       (interactive)
       (let ((default-directory ,(or project-path `default-directory)))
         (,layout-fn ,procs ,proc-fn)))))

(provide 'emacsocil)
;;; emacsocil.el ends here
