;;; nry-scroll.el --- Fixing some issues with Emacs default scroll  -*- lexical-binding: t; -*-

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

(defun nry-scroll-half-page (direction)
  "Nry-Scroll half page up if `DIRECTION' is non-nil.
Otherwise will nry-scroll half page down."
  (let ((opos (cdr (nth 6 (posn-at-point)))))
    ;; opos = original position line relative to window
    (move-to-window-line nil)    ;; Move cursor to middle line
    (if direction
        (recenter-top-bottom -1) ;; Current line becomes last
      (recenter-top-bottom 0))   ;; Current line becomes first
    (move-to-window-line opos))) ;; Restore cursor/point position

(defun nry-scroll-half-page-down ()
  "Nry-Scroll exactly half page down keeping cursor/point position."
  (interactive)
  (nry-scroll-half-page nil))

(defun nry-scroll-half-page-up ()
  "Nry-Scroll exactly half page up keeping cursor/point position."
  (interactive)
  (nry-scroll-half-page t))

(define-key global-map [remap scroll-down-command] 'nry-scroll-half-page-up)
(define-key global-map [remap scroll-up-command] 'nry-scroll-half-page-down)

;; Scoll line by line.

(defun scroll-one-line-up ()
  "Scroll one line up."
  (interactive)
  (scroll-up-command 1))

(defun scroll-one-line-down ()
  "Scroll one line down."
  (interactive)
  (scroll-down-command 1))

;; scroll window up/down by one line
(global-set-key (kbd "M-n") 'scroll-one-line-up)
(global-set-key (kbd "M-p") 'scroll-one-line-down)
;; Also use it in Info mode
(define-key Info-mode-map (kbd "M-n") 'scroll-one-line-up)

(setq scroll-step 1
      scroll-conservatively 10000
      next-screen-context-lines 5
      line-move-visual nil)

(pixel-scroll-precision-mode)

(provide 'nry-scroll)
;;; nry-scroll.el ends here
