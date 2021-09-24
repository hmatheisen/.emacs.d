;;; he-scroll.el --- Scroll utilities for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Henry MATHEISEN

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; I am not a fan of Emacs native scrolling so these ar a set of
;; functions to make it smoother.

;;; Code:

;; Half page scroll instead of whole page

(defun he/scroll-half-page (direction)
  "Scrolls half page up if `direction' is non-nil, otherwise
  will scroll half page down."
  (let ((opos (cdr (nth 6 (posn-at-point)))))
    ;; opos = original position line relative to window
    (move-to-window-line nil)  ;; Move cursor to middle line
    (if direction
        (recenter-top-bottom -1)  ;; Current line becomes last
      (recenter-top-bottom 0))  ;; Current line becomes first
    (move-to-window-line opos)))  ;; Restore cursor/point position

(defun he/scroll-half-page-down ()
  "Scrolls exactly half page down keeping cursor/point position."
  (interactive)
  (he/scroll-half-page nil))

(defun he/scroll-half-page-up ()
  "Scrolls exactly half page up keeping cursor/point position."
  (interactive)
  (he/scroll-half-page t))

(global-set-key (kbd "C-v") 'he/scroll-half-page-down)
(global-set-key (kbd "M-v") 'he/scroll-half-page-up)


;; Scoll line by line.

(defun he/scroll-one-line-up ()
  (interactive)
  (scroll-up-command 1))

(defun he/scroll-one-line-down ()
  (interactive)
  (scroll-down-command 1))

;; scroll window up/down by one line
(global-set-key (kbd "M-n") 'he/scroll-one-line-up)
(global-set-key (kbd "M-p") 'he/scroll-one-line-down)

;; Somehow Info mode overrides the globally set key
(define-key Info-mode-map (kbd "M-n") 'he/scroll-one-line-up)

(provide 'he-scroll)
;;; he-scroll.el ends here
