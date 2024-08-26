;;; early-init.el --- Early settings                 -*- lexical-binding: t; -*-

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

;; Uncomment this to debug.
;; (setq init-file-debug t)
;; (setq messages-buffer-max-lines 100000)

(defconst *is-a-mac* (eq system-type 'darwin))

;; Set high garbage collection threshold during startup.
;; This is reset to a more reasonable value in init.el by the gcmh package.
(setq gc-cons-threshold most-positive-fixnum)

(when *is-a-mac*
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(undecorated-round . t)))

(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 200))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(setq inhibit-startup-screen t)

(provide 'early-init)
;;; early-init.el ends here
