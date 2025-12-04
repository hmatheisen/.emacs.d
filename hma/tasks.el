;;; tasks.el --- easily run common tasks             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Henry MATHEISEN

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

;;; Code:

(require 'cl-lib)
(require 'project)

(defvar task-functions nil
  "List of functions created by `define-task'.")

(cl-defmacro define-task (task-name description
                          &key command path (project nil) (async t))
  "Define a new task named to run a COMMAND.
A command has a DESCRIPTION and can be run from a PATH or within a PROJECT.
A call to `define-task' creates a new interactive function named TASK-NAME.
Having both a PATH and a PROJECT set is incompatible and will raise an error
evaluating this macro.
If ASYNC is set to nil, use `shell-command' otherwise, default to
`async-shell-command'.

Example:

\(define-task list-files
  \"List all files in directory\"
  :commamd \"ls -alsh\"
  :path \"~/.emacs.d/\")

This will create a new interactive function called list-files that runs the
`async-shell-command': \"ls -alsh\" inside the directory \"~/.emacs.d/\""
  (declare (indent defun)
           (doc-string 2))
  (when (and project path)
    (error "`define-task': Incompatible keys `:project' and `:path'"))
  (let ((shell-function (if async #'async-shell-command #'shell-command))
        (description (if path
                         (concat path " - " description)
                       description))
        (directory (cond (project '(project-root (project-current t)))
                         ((stringp path) path)
                         (t 'default-directory))))
    (unless (string-equal task-name "elisp--witness--lisp") ;; :(
      (cl-pushnew task-name task-functions))
    `(defun ,task-name ()
       ,description
       (interactive)
       (let ((default-directory ,directory))
         (funcall #',shell-function ,command)))))

(defun tasks-run ()
  "Run a defined task."
  (interactive)
  (funcall
   (intern
    (completing-read "Tasks: " task-functions))))

(provide 'tasks)
;;; tasks.el ends here
