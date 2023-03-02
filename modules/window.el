;;; window.el --- Window utilities for Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Henry MATHEISEN

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

;; Some utilities regarding window manipulation in Emacs.  Default
;; behavior sometimes seem to come from another a planet where the
;; people have 27 fingers by hands.

;;; Code:

(defun split-window-right-focus ()
  "Split window on the right then focus on that window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun split-window-below-focus ()
  "Split window below then focus on that window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun delete-window-balance ()
  "Delete window and balance."
  (interactive)
  (delete-window)
  (balance-windows))

;; Use by own split functions
(global-set-key (kbd "C-x 0") 'delete-window-balance)
(global-set-key (kbd "C-x 2") 'split-window-below-focus)
(global-set-key (kbd "C-x 3") 'split-window-right-focus)

;; Move between windows
(defun other-window-backward ()
  "Move to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-backward)

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
    ("Org"   . "~/Notes/org-roam/"))
  "Alist of tab names and folders to be created by `create-my-workspace'.")

;; Utils
(defun create-named-tab (name)
  "Create a new tab and rename it to NAME."
  (tab-bar-new-tab)
  (tab-bar-rename-tab name))

(defun print-tab-name ()
  "Print the name of the current tab."
  (message
   (concat
    "<< "
    (upcase (alist-get 'name (tab-bar--current-tab)))
    " >>")))

;; Interactive functions
(defun new-tab (name)
  "Create a new tab with `NAME'."
  (interactive "sNew tab name: ")
  (create-named-tab name))

(defun create-my-workspace ()
  "Create tabs based on the `my-workspace' variable."
  (interactive)
  (unless my-workspace
    (error "Cannot create workspaces if var: `my-workspace' is nil"))
  ;; Do not create a new tab for the element of the list
  (tab-bar-rename-tab (caar my-workspace))
  (dired (cdar my-workspace))
  (cl-loop for (tab . dir) in (cdr my-workspace)
           do
           (create-named-tab tab)
           (dired dir)))

(defun my-tab-next ()
  "Call `tab-bar-switch-to-next-tab' and print tab name."
  (interactive)
  (tab-bar-switch-to-next-tab)
  (print-tab-name))

(defun my-tab-prev ()
  "Call `tab-bar-switch-to-prev-tab' and print tab name."
  (interactive)
  (tab-bar-switch-to-prev-tab)
  (print-tab-name))

(define-key global-map (kbd "C-<tab>") 'my-tab-next)
(define-key global-map (kbd "C-S-<tab>") 'my-tab-prev)
(define-key global-map (kbd "C-x t v") 'toggle-frame-tab-bar)
(define-key global-map (kbd "C-x t s") 'tab-switcher)
(define-key global-map (kbd "C-x t 2") 'new-tab)

(use-package "ace-window"
  :config (global-set-key (kbd "C-x o") 'ace-window))

(provide 'window)

;;; window.el ends here
