;;; tasks.el --- easily run common tasks             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Henry MATHEISEN

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

;;

;;; Code:

(require 'cl-lib)

(defvar tasks-functions nil
  "List of functions created by tasks.")

(defmacro with-task-project (task-project-name &rest body)
  "Create tasks within a project TASK-PROJECT-NAME.
Eval `define-task' macros in BODY."
  (declare (indent defun))
  (macroexp-progn
   (mapcar
    (lambda (task)
      (unless (eq (car task) 'define-task)
        (error "`with-task-project' can only be used to call `define-task' macro"))
      `(,@(take 2 task)
        :project-path ,task-project-name
        ,@(nthcdr 2 task)))
    body)))

(cl-defmacro define-task (task-name &key command description project-path (async t))
  "Define a new task TASK-NAME to run COMMAND.
A command has a DESCRIPTION and can be run from a PROJECT-PATH.
If ASYNC is set to nil, run a shell command."
  (declare (indent defun))
  (let ((shell-function (if async 'async-shell-command 'shell-command))
        (description (if project-path
                         (concat project-path " - " description)
                       description))
        (directory (or project-path default-directory)))
    (unless (string-equal task-name "elisp--witness--lisp") ;; :(
      (cl-pushnew task-name tasks-functions))
    `(defun ,task-name ()
       (:documentation ,description)
       (interactive)
       (let ((default-directory ,directory))
         (funcall ',shell-function ,command)))))

(defun run-task ()
  "Run a defined task."
  (interactive)
  (funcall (intern (completing-read "Task: " tasks-functions))))

;; TODO (defun project-run-task ())

;; TODO Interactively define a new task
;;      Where can we save the data and which format?

(provide 'tasks)
;;; tasks.el ends here
