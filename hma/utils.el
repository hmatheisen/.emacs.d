;;; utils.el --- Elisp utils/Interactive functions  -*- lexical-binding: t; -*-

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

;;; Window handling

;; Scroll by half page by default
(defun hma/window-half-height ()
  "Return half the height of a window."
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun hma/scroll-half-page-up-command (&optional arg)
  "Scroll up half the height of a window by default or by ARG."
  (interactive "^P")
  (scroll-up-command (or arg (hma/window-half-height))))

(defun hma/scroll-half-page-down-command (&optional arg)
  "Scroll down half the height of a window by default or by ARG."
  (interactive "^P")
  (scroll-down-command (or arg (hma/window-half-height))))

(keymap-global-set "C-v" #'hma/scroll-half-page-up-command)
(keymap-global-set "M-v" #'hma/scroll-half-page-down-command)

;;; Projects

(defmacro with-current-project (project &rest body)
  "Execute BODY with ROOT as the current PROJECT root."
  (declare (indent 1))
  `(let ((,project (project-current t)))
     ,@body))

(defun hma/copy-file-relative-path ()
  "Print and kill the absolute file path of the current buffer in a project."
  (unless buffer-file-name
    (error "project-absolute-file-path: Could not get buffer file name"))
  (with-current-project project
    (let* ((root (project-root project))
           (absolute-file-path (file-relative-name buffer-file-name root)))
      (kill-new absolute-file-path)
      (message (concat "Saved \"" absolute-file-path "\" to kill ring")))))

(defun project-copy-relative-path ()
  "Print and kill the absolute file path of the current buffer in a project."
  (interactive)
  (hma/copy-file-relative-path))

(keymap-set project-prefix-map "C-y" #'project-copy-relative-path)

;; open vterm at root of project
(defun project-vterm ()
  "Open a vterm at the root of the current project."
  (interactive)
  (with-current-project project
    (let ((name (project-name project))
          (root (project-root project)))
      (when (vterm-open (concat "*vterm*<" name ">"))
        (vterm-send-string (concat "cd " root))
        (vterm-send-return)
        (vterm-clear)))))

(define-key project-prefix-map "t" 'project-vterm)

(defun project-clean-buffers ()
  "Like `project-kill-buffers' but keep some arbitrary ones."
  (interactive)
  (save-some-buffers)
  (let* ((project (project-current t))
         (project-name (project-name project))
         (project-root (project-root project))
         (project-buffers (project-buffers project))
         (dired-buffer-name (buffer-name (dired-noselect project-root))))
    (mapc (lambda (buffer)
            (let ((buffer-name (buffer-name buffer)))
              (unless (or (string-equal buffer-name dired-buffer-name)
                          (string-equal buffer-name (concat "magit: " dired-buffer-name))
                          (string-match-p "^\*vterm\*" buffer-name)
                          (string-match-p "*EGLOT" buffer-name))
                (kill-buffer buffer))))
          project-buffers)
    (message (concat "Buffers cleaned for project: " project-name))))

(define-key project-prefix-map (kbd "k") 'project-clean-buffers)

(defun macroexpand-point (sexp)
  "Expand macro SEXP at point to temp buffer."
  (interactive (list (sexp-at-point)))
  (let ((buffer-name "*el-macroexpansion**"))
    (with-output-to-temp-buffer buffer-name
      (pp (macroexpand sexp)))
    (with-current-buffer buffer-name
      (emacs-lisp-mode)
      (view-mode))))

(defun new-buffer (new-buffer-name)
  "Create a new buffer named NEW-BUFFER-NAME and switch to it."
  (interactive "sNew buffer name: ")
  (switch-to-buffer
   (concat "*" new-buffer-name "*")))

(global-set-key (kbd "C-x B") 'new-buffer)

(provide 'utils)
;;; utils.el ends here
