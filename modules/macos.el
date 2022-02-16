;;; macos.el --- MacOS Specific Options              -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Henry MATHEISEN

;; Author: Henry MATHEISEN <henry@macbook>

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

;; Keyboard options 
(setq mac-option-modifier 'meta
      mac-command-modifier 'super
      mac-right-option-modifier 'nil)

;; Title bar appearance
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

(provide 'macos)
;;; macos.el ends here
