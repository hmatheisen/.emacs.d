;;; init.el --- He-Macs init file                    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Henry MATHEISEN

;; Author: Henry MATHEISEN <henry.mthsn@gmail.com>

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

;; Personal Emacs init file.  The actual init is rather small since it
;; just ensures `use-package' and `org' is installed so that we can
;; load the actual config file.

;;; Code:

;; Load package manager and add sources
(require 'package)
(setq package-archives
   '(("org" . "https://orgmode.org/elpa/")
    ("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("gnu" . "http://elpa.gnu.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

;; Install `use-package' if not here
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

;; Load personal function and utilities
(add-to-list 'load-path
             (expand-file-name "he-lisp" user-emacs-directory))

(require 'he-align)
(require 'he-path)
(require 'he-scroll)
(require 'he-window)

;; Install org mode to load config
(use-package org :ensure org-plus-contrib)

;; Tangle configuration
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;;; init.el ends here
