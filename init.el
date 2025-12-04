;;; init.el --- Emacs base config file               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Henry MATHEISEN

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
;; along with this program.  If not, see <https://www.gnu.icenses/>.

;;; Commentary:

;;; Code:

(defvar absolute-user-emacs-directory
  (file-truename user-emacs-directory)
  "Emacs directory as absolute path.")

(setq custom-file (concat absolute-user-emacs-directory "elisp/custom.el"))
(load custom-file)

(require 'dired)
(require 'eglot)
(require 'hl-line)
(require 'treesit)
(require 'windmove)
(require 'cc-cmds)

(add-to-list 'load-path (concat absolute-user-emacs-directory "hma"))
(add-to-list 'load-path (concat absolute-user-emacs-directory "elisp"))

(require 'emacsocil)
(require 'format-buffer)
(require 'isearch-transient)
(require 'tasks)
;; (require 'theme-switcher)
(require 'utils)

(setq elisp-flymake-byte-compile-load-path load-path)

(require 'keymaps)
(require 'lang)
(require 'org-conf)
(require 'ui)

;; Packages

(use-package exec-path-from-shell
  :ensure t
  :functions exec-path-from-shell-initialize
  :config
  (exec-path-from-shell-initialize))

(use-package marginalia
  :ensure t
  :functions marginalia-mode
  :init
  (marginalia-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (keymap-global-set "C-S-<down>" 'mc/mark-next-like-this)
  (keymap-global-set "C-S-<up>" 'mc/mark-previous-like-this)
  (keymap-global-set "C-S-<return>" 'mc/mark-all-like-this))

(use-package corfu :ensure t)

(use-package vterm
  :functions (vterm vterm-send-string vterm-open vterm-send-return)
  :config
  (defun vterm-open (buffer-name)
    "Open or switch to a vterm buffer called BUFFER-NAME.  Return `nil' if the
  buffer already existed otherwise, return the new buffer."
    (if-let ((buffer (seq-find (lambda (buffer)
                                 (string= buffer-name (buffer-name buffer)))
                               (buffer-list))))
        (progn (switch-to-buffer buffer) nil)
      (vterm buffer-name)))

  (defun vterm-start-process (proc)
    "Open or switch to a vterm buffer to run a command PROC."
    (let ((vterm-buffer-name (concat "*vterm*<" proc ">")))
      (when (vterm-open vterm-buffer-name)
        (vterm-send-string proc)
        (vterm-send-return)))))

(setenv "DYLD_LIBRARY_PATH" "/opt/homebrew/lib")
(setenv "LIBRARY_PATH" "/opt/homebrew/lib")
(setenv "C_PATH" "/opt/homebrew/include")
(setopt Info-additional-directory-list '("/opt/homebrew/share/info"))

;;; init.el ends here
