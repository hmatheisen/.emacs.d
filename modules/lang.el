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

(defmacro format-buffer-on-save (mode &rest args)
  "Run a shell command from ARGS for a given MODE on the whole buffer.

Errors are reported in a buffer but not shown.

Example:
To format a \"sql-mode\" buffer with the \"pg_format\" command:

\(format-buffer-on-save
  sql
  :command \"pg_format\"\)"
  (let* ((mode-str            (concat (symbol-name mode)))
         (command             (plist-get args :command))
         (before-save-hook-fn (intern (concat mode-str "-before-save-hook")))
         (major-hook-name     (intern (concat mode-str "-mode-hook")))
         (error-buffer-name   (concat "*" (capitalize mode-str) " Format Error Buffer*")))
    `(progn
       (defun ,before-save-hook-fn ()
         (let ((point-pos (point)))
           (shell-command-on-region
            (point-min)
            (point-max)
            ,command
            (current-buffer)
            t
            ,error-buffer-name
            nil)
           (goto-char point-pos)))
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
  :command "stree format --print-width=100")


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


;; SQL

(format-buffer-on-save sql
  :command "pg_format")

(use-package yaml-mode
  :defer t)

;; Shell settings
(setq sh-basic-offset 2)

(provide 'lang)
;;; lang.el ends here
