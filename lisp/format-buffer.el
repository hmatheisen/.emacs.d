;;; format-buffer.el --- Format buffer given an external program  -*- lexical-binding: t; -*-

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

(defun format-region (beg end program args)
  "Format region with PROGRAM and ARGS on BEG to END."
  (let* ((input-file  (make-temp-file (concat program "-input")))  ; run the formatter on this file
         (stdout-file (make-temp-file (concat program "-stdout"))) ; formatter output
         (stderr-file (make-temp-file (concat program "-stderr"))) ; formatter err
         (point-pos   (point)))
    ;; Write region to input-file
    (write-region beg end input-file nil :quiet)
    (let ((error-buffer (get-buffer-create (concat "*" program " Error Buffer*"))) ; create error buffer
          (return-code
           (apply 'call-process
                  program
                  input-file
                  (list (list :file stdout-file) stderr-file)
                  t
                  args))) ; save return code
      ;; Save stderr in error buffer
      (with-current-buffer error-buffer
        (let ((inhibit-read-only t))
          (insert-file-contents stderr-file nil nil nil t))
        (special-mode))
      ;; Update buffer if formatter returns 0
      ;; else, display error buffer
      (if (zerop return-code)
          (progn
            (save-restriction
              (narrow-to-region beg end)
              (insert-file-contents stdout-file nil nil nil t)
              (goto-char point-pos)
              (delete-windows-on error-buffer)))
        (display-buffer error-buffer)))
    (delete-file input-file)
    (delete-file stdout-file)
    (delete-file stderr-file)))

(cl-defmacro format-lang (mode &key command args)
  "Run a shell COMMAND with ARGS for a given MODE on the whole buffer.

This macro creates 2 functions to format a region or the whole buffer.

Errors are shown in a read only buffer if there are any.

Example:
To format a \"ruby-mode\" buffer with the \"stree format --print-width=100\"
command:

\(format ruby
  :command \"stree\"
  :args \\='(\"format\" \"--print-width=100\"\)\)"
  (declare (indent defun))
  (let* ((mode-str          (symbol-name mode))
         (mode-hook         (intern (concat mode-str "-mode-hook")))
         (format-region-fn  (intern (concat mode-str "-format-region")))
         (format-buffer-fn  (intern (concat mode-str "-format-buffer"))))
    `(progn
       (defun ,format-region-fn (beg end)
         (interactive "r")
         (format-region
          beg end ,command ,args))
       (defun ,format-buffer-fn ()
         (interactive)
         (format-region
          (point-min) (point-max) ,command ,args))
       (add-hook ',mode-hook
                 (lambda ()
                   (local-set-key (kbd "C-c f") ',format-buffer-fn))))))

(provide 'format-buffer)
;;; format-buffer.el ends here
