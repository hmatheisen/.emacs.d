;;; global.el --- Editor settings

;;; Commentary:
;;; global editor settings

;;; Code:

;; Default binary for *ansi-term*
(if *is-a-mac*
    (setq explicit-shell-file-name "/usr/local/bin/bash")
  (setq explicit-shell-file-name "/bin/bash"))

;; Enable downcase-region
(put 'downcase-region 'disabled nil)
;; Enable upcase-region
(put 'upcase-region 'disabled nil)

;; Change splash screen image with a better looking one
(setq fancy-splash-image "~/.emacs.d/logo-small.png")

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Modeline info
(display-time-mode t)
(column-number-mode t)
(display-battery-mode t)

;; Remove Toolbar
(tool-bar-mode -1)

;; Disable menu bar
(menu-bar-mode -1)

;; Line numbers
(global-linum-mode t)

;; Make sure all backup files only live in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/.backups")))

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

;; Set fringe color to nil
(set-face-attribute 'fringe nil :background nil)

;; Toggle fullscreen on startup
(add-hook 'after-init-hook 'toggle-frame-fullscreen)

;; Add useful path to exec-path and PATH
(defun add-to-path (path)
  "Add a path to `exec-path' and Emacs \"PATH\" variable."
  (add-to-list 'exec-path (substring path 1))
  (setenv "PATH" (concat (getenv "PATH") path)))

(add-to-path ":/usr/local/bin")
(add-to-path ":/Library/TeX/texbin")

;; Flycheck
(use-package flycheck :config (global-flycheck-mode +1))

;; Make that damn bell shut up
(setq ring-bell-function 'ignore)

;; Winner mode
(winner-mode t)

;; Default truncate lines
(setq-default truncate-lines t)

;; Replace the active region just by typing text, just like modern editors
(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

;; Disable scroll-bar
(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode -1))

;; Enable column numbers
(use-package simple
  :ensure nil
  :config (column-number-mode +1))

;; Show matching parentheses
(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode +1))

;; Enter ediff with side-by-side buffers to better compare the differences.
(use-package ediff
  :ensure nil
  :config (setq ediff-split-window-function 'split-window-horizontally))

;; Auto-pairing quotes and parentheses etc.
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4))

(provide 'global)
;;; global.el ends here

