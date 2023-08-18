;;; nry-ui.el --- UI config                              -*- lexical-binding: t; -*-

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

(defconst *mono-font* "Iosevka"
  "Default mono font to be used.")

(defconst *font-size* 16
  "Font size un points.")

;; On prog mode, show line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Full screen on init
(add-hook 'after-init-hook 'toggle-frame-fullscreen)

;; Set fringes
(fringe-mode '(nil . 0)) ;; left only

;; Fonts
(set-face-attribute 'default
                    nil
                    :family *mono-font*
                    :height (* *font-size* 10))

(set-face-attribute 'fixed-pitch
                    nil
                    :family *mono-font*
                    :height (* *font-size* 10))

(set-face-attribute 'variable-pitch
                    nil
                    :family *mono-font*
                    :height (* *font-size* 10))

(provide 'nry-ui)
;;; ui.el ends here
