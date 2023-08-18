;;; init.el --- Emacs config entrypoint              -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Henry MATHEISEN

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

(defconst *is-a-mac* (eq system-type 'darwin)
  "Check whether system is mac.")

(add-to-list 'load-path
             (expand-file-name "modules" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Load package manager and add sources
(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Load config files

(require 'nry-completion)
(require 'nry-lang)
(when *is-a-mac*
  (require 'nry-macos))
(require 'nry-misc)
(require 'nry-org)
(require 'nry-text)
(require 'nry-theme)
(require 'nry-tools)
(require 'nry-ui)
(require 'nry-utils)
(require 'nry-window)

;; Variables configured via the interactive 'customize' interface
;; (when (file-exists-p custom-file)
;;   (load custom-file))

(provide 'init)
;;; init.el ends here
