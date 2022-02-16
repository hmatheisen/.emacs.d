;;; packages.el --- Package Settings                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Henry MATHEISEN

;; Author: Henry MATHEISEN <haineriz@posteo.de>

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
(require 'package)

;; Load package manager and add sources
(setq package-archives
   '(("org" . "https://orgmode.org/elpa/")
    ("melpa" . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Ensure `use-package' is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(use-package diminish
  :defer t
  :after use-package)

(provide 'packages)
;;; packages.el ends here
