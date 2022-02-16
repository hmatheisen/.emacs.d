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

(defconst *is-a-mac* (eq system-type 'darwin)
  "Check whether system is mac.")

(defun load-module (file)
  "Load FILE in the modules directory"
  (load (concat
         (expand-file-name "modules/" user-emacs-directory)
         file)))

(load-module "packages") ;; Must be loaded before others

(load-module "completion")
(load-module "keys")
(load-module "lang")
(when *is-a-mac*
  (load-module "macos"))
(load-module "misc")
(load-module "scrolling")
(load-module "text")
(load-module "themes")
(load-module "tools")
(load-module "ui")
(load-module "utils")
(load-module "window")

(provide 'init)

;;; init.el ends here
