;;; scrolling.el --- My take in scroll in Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Henry MATHEISEN

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

;; I am not a fan of Emacs native scrolling so these are a set of
;; functions to make it smoother.

;;; Code:

;; Half page scroll instead of whole page

(defun scroll-half-page (direction)
  "Scroll half page up if `DIRECTION' is non-nil.
Otherwise will scroll half page down."
  (let ((opos (cdr (nth 6 (posn-at-point)))))
    ;; opos = original position line relative to window
    (move-to-window-line nil)    ;; Move cursor to middle line
    (if direction
        (recenter-top-bottom -1) ;; Current line becomes last
      (recenter-top-bottom 0))   ;; Current line becomes first
    (move-to-window-line opos))) ;; Restore cursor/point position

(defun scroll-half-page-down ()
  "Scroll exactly half page down keeping cursor/point position."
  (interactive)
  (scroll-half-page nil))

(defun scroll-half-page-up ()
  "Scroll exactly half page up keeping cursor/point position."
  (interactive)
  (scroll-half-page t))

(global-set-key (kbd "C-v") 'scroll-half-page-down)
(global-set-key (kbd "M-v") 'scroll-half-page-up)


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


;; Mouse scrolling

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(provide 'scrolling)
;;; scrolling.el ends here
