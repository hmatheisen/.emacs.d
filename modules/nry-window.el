;;; nry-window.el --- Make using windows easier      -*- lexical-binding: t; -*-

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

(require 'cl-lib)

(defun nry-split-window-right-focus ()
  "Split window on the right then focus on that window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun nry-split-window-below-focus ()
  "Split window below then focus on that window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun nry-delete-window-balance ()
  "Delete window and balance."
  (interactive)
  (delete-window)
  (balance-windows))

;; Use by own split functions
(global-set-key (kbd "C-x 0") 'nry-delete-window-balance)
(global-set-key (kbd "C-x 2") 'nry-split-window-below-focus)
(global-set-key (kbd "C-x 3") 'nry-split-window-right-focus)

;; Move between windows
(defun nry-other-window-backward ()
  "Move to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'nry-other-window-backward)

;; Still enable Windmove for easy navigation
(windmove-default-keybindings)

;; Resizing
(global-set-key (kbd "M--") 'shrink-window)
(global-set-key (kbd "M-+") 'enlarge-window)
(global-set-key (kbd "C--") 'shrink-window-horizontally)
(global-set-key (kbd "C-+") 'enlarge-window-horizontally)

;; Winner mode to revert window conf
(winner-mode t)

;; Tab bar
(setq tab-bar-show nil)
(tab-bar-mode t)

;; Vars
(defvar my-workspace
  '(("Elevo" . "~/Code/elevo-rails/")
    ("Emacs" . "~/.emacs.d/")
    ("Org"   . "~/org/"))
  "Alist of tab names and folders to be created by `create-my-workspace'.")

;; Utils
(defun nry-create-named-tab (name)
  "Create a new tab and rename it to NAME."
  (tab-bar-new-tab)
  (tab-bar-rename-tab name))

(defun nry-print-tab-name ()
  "Print the name of the current tab."
  (message
   (concat
    "<< "
    (upcase (alist-get 'name (tab-bar--current-tab)))
    " >>")))

;; Interactive functions
(defun nry-new-tab (name)
  "Create a new tab with `NAME'."
  (interactive "sNew tab name: ")
  (nry-create-named-tab name))

(defun nry-create-my-workspace ()
  "Create tabs based on the `my-workspace' variable."
  (interactive)
  (unless my-workspace
    (error "Cannot create workspaces if var: `my-workspace' is nil"))
  ;; Do not create a new tab for the element of the list
  (tab-bar-rename-tab (caar my-workspace))
  (dired (cdar my-workspace))
  (cl-loop for (tab . dir) in (cdr my-workspace)
           do
           (nry-create-named-tab tab)
           (dired dir))
  (tab-bar-switch-to-tab (caar my-workspace)))

(defun nry-tab-next ()
  "Call `tab-bar-switch-to-next-tab' and print tab name."
  (interactive)
  (tab-bar-switch-to-next-tab)
  (nry-print-tab-name))

(defun nry-tab-prev ()
  "Call `tab-bar-switch-to-prev-tab' and print tab name."
  (interactive)
  (tab-bar-switch-to-prev-tab)
  (nry-print-tab-name))

(define-key global-map (kbd "C-<tab>") 'nry-tab-next)
(define-key global-map (kbd "C-S-<tab>") 'nry-tab-prev)
(define-key global-map (kbd "C-x t v") 'toggle-frame-tab-bar)
(define-key global-map (kbd "C-x t s") 'tab-switcher)
(define-key global-map (kbd "C-x t 2") 'nry-new-tab)

(use-package "ace-window"
  :ensure t
  :config (global-set-key (kbd "C-x o") 'ace-window))

(provide 'nry-window)
;;; nry-window.el ends here
