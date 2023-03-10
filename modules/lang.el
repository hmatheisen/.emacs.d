;;; lang.el --- Packages for unsupported languages   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Henry MATHEISEN

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

;; Utils

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

(cl-defmacro format-buffer-on-save (mode &key command args)
  "Run a shell COMMAND with ARGS for a given MODE on the whole buffer.

Errors are shown in a read only buffer if there are any.

Example:
To format a \"ruby-mode\" buffer with the \"stree format
--print-width=100\" command:

\(format-buffer-on-save ruby
  :command \"pg_format\"
  :args '\(\"format\" \"--print-width=100\"\)\)"
  (declare (indent defun))
  (let* ((mode-str            (symbol-name mode))
         (format-region-fn      (intern (concat mode-str "-format-region")))
         (before-save-hook-fn (intern (concat mode-str "-format-buffer")))
         (major-hook-name     (intern (concat mode-str "-mode-hook"))))
    `(progn
       (defun ,format-region-fn (beg end)
         (interactive "r")
         (format-region
          beg end ,command ,args))
       (defun ,before-save-hook-fn ()
         (interactive)
         (format-region
          (point-min) (point-max) ,command ,args))
       (add-hook ',major-hook-name
                 (lambda ()
                   (add-hook 'before-save-hook
                             #',before-save-hook-fn
                             nil
                             t))))))


;; Clojure

(use-package clojure-mode)
(use-package cider)


;; Ruby
(use-package ruby-electric
  :defer t
  :hook (ruby-mode . ruby-electric-mode))

(format-buffer-on-save ruby
  :command "stree"
  :args '("format" "--print-width=100"))


;; Frontend
(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

;; Web mode for js/jsx/tsx
(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.js\\'"  . web-mode))
  :config
  ;; Default to jsx for .js files since it's the one I use the most on
  ;; flow files
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js\\'")))
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; JS indent
(setq js-indent-level 2)

;; Run prettier on save in web mode
(use-package prettier
  :hook ((web-mode . prettier-mode)))


;; XML

(format-buffer-on-save nxml
  :command "xmllint --format -")


;; SQL

(format-buffer-on-save sql
  :command "pg_format")

(use-package yaml-mode
  :defer t)

;; Shell settings
(setq sh-basic-offset 2)

(provide 'lang)
;;; lang.el ends here
