;;; memacs.el --- GNU Emacs Mini Init File

;;; Commentary:
;;;
;;; A micro config for quick Emacs use.  All the code is taken from
;;; `config.org' in the global settings section, i highly recommend
;;; visiting this file instrad since this is where the docs are.
;;;
;;; I use this mostly in the terminal with an alias that allows me to
;;; load only this file at Emacs startup, making in really fast.
;;; There are no packages, pure vanilla Emacs config.

;;; Code:

(defconst *is-a-mac* (eq system-type 'darwin)
  "Check whether system is mac.")

(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)

(defun add-to-path (path)
  "Add a path to `exec-path' and Emacs \"PATH\" variable."
  (add-to-list 'exec-path (substring path 1))
  (setenv "PATH" (concat (getenv "PATH") path)))

;; Avoid a few issues on MacOS
(when *is-a-mac*
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        select-enable-clipboard t))

;; Enable downcase/upcase region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Remove Toolbar
(tool-bar-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

;; Trash can support
(setq delete-by-moving-to-trash t)

;; Set tabs to 2
(setq-default tab-width 2)

;; Indent using spaces
(setq-default indent-tabs-mode nil)

;; Title Bar Settings
(when *is-a-mac*
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil))

;; Make that damn bell shut up
(setq ring-bell-function 'ignore)

;; Add useful path to exec-path and PATH
(add-to-path ":/usr/local/bin")
(add-to-path ":/Library/TeX/texbin")

;; Default truncate lines
(setq-default truncate-lines t)

(setq custom-safe-themes t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(if *is-a-mac*
    (setq explicit-shell-file-name "/usr/local/bin/bash")
  (setq explicit-shell-file-name "/bin/bash"))

;; Resizing
(global-set-key (kbd "M--") 'shrink-window)
(global-set-key (kbd "M-+") 'enlarge-window)
(global-set-key (kbd "C--") 'shrink-window-horizontally)
(global-set-key (kbd "C-+") 'enlarge-window-horizontally)

;; Other window
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") '(lambda ()
                               (interactive)
                               (other-window -1)))

(fset 'yes-or-no-p 'y-or-n-p)
(display-time-mode t)
(column-number-mode t)
(display-battery-mode t)
(global-linum-mode t)
(setq backup-directory-alist '(("." . "~/.emacs.d/.backups")))
(winner-mode t)
(delete-selection-mode +1)
(scroll-bar-mode -1)
(show-paren-mode t)
(setq ediff-split-window-function 'split-window-horizontally)
(add-hook 'prog-mode 'electric-pair-mode)

(ido-mode t)
(setq ido-everywhere t
      ido-enable-flex-matching t)

(load-theme 'wombat)

;;; memacs.el ends here
