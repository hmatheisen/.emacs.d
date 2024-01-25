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

;;; Code:

(require 'cl-lib)
(require 'project)

(defvar tasks-functions nil
  "Plist of functions created by tasks.")

(defun tasks--tasks-functions-get (key)
  "Get KEY's value from `tasks-functions' plist."
  (plist-get tasks-functions key 'string-equal))

(defun tasks--tasks-functions-put (key value)
  "Put VALUE in KEY on `tasks-functions' plist.
VALUE is not inserted if it's already in the list."
  (let ((init-list (tasks--tasks-functions-get key)))
    (unless (member value init-list)
      (setq tasks-functions
            (plist-put tasks-functions
                       key
                       `(,@init-list ,value)
                       'string-equal)))))

(defun tasks--tasks-functions->list ()
  "Convert the `tasks-functions' plist to a function list."
  (let ((func-list))
    (dolist (el tasks-functions func-list)
      (if (listp el)
          (dolist (func el)
            (setq func-list (cons func func-list)))))))

(defun tasks--run-tasks (task-list)
  "Prompt user to chose a task from TASK-LIST and call it."
  (funcall
   (intern
    (completing-read "Tasks: "
                     task-list))))

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
  "Define a new task named to run a COMMAND.
A command has a DESCRIPTION and can be run from a PROJECT-PATH.
A call to `define-task' creates a new interactive function named TASK-NAME.
If ASYNC is set to nil, use `shell-command' otherwise, default to
`async-shell-command'.

Example:

\(define-task list-files
  :commamd \"ls -alsh\"
  :description \"List all files in directory\"
  :project-path \"~/.emacs.d/\")

This will create a new interactive function called list-files that runs the
`async-shell-command': \"ls -alsh\" inside the directory \"~/.emacs.d/\""
  (declare (indent defun))
  (let ((shell-function (if async 'async-shell-command 'shell-command))
        (description (if project-path
                         (concat project-path " - " description)
                       description))
        (directory (or project-path default-directory)))
    (unless (string-equal task-name "elisp--witness--lisp") ;; :(
      (tasks--tasks-functions-put project-path task-name))
    `(defun ,task-name ()
       ,description
       (interactive)
       (let ((default-directory ,directory))
         (funcall ',shell-function ,command)))))

(defun tasks-run ()
  "Run a defined task."
  (interactive)
  (tasks--run-tasks (tasks--tasks-functions->list)))

(defun project-tasks-run ()
  "Run a defined task in a project."
  (interactive)
  (let* ((project (project-current t))
         (root (project-root project))
         (tasks-list (tasks--tasks-functions-get root)))
    (if tasks-list
        (tasks--run-tasks (tasks--tasks-functions-get root))
      (message "No tasks defined for this project"))))

(define-key project-prefix-map "\C-t" 'project-tasks-run)

(provide 'tasks)
;;; tasks.el ends here
