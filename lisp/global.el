;;; global.el --- Editor settings

;;; Commentary:
;;; global editor settings

;;; Code:

;; Variables/Functions without packages
(use-package emacs
  :preface
  (defun add-to-path (path)
    "Add a path to `exec-path' and Emacs \"PATH\" variable."
    (add-to-list 'exec-path (substring path 1))
    (setenv "PATH" (concat (getenv "PATH") path)))
  :ensure nil
  :config
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
  (setq-default truncate-lines t))

;; Default binary for new terminal
(use-package term
  :ensure nil
  :config
  (if *is-a-mac*
      (setq explicit-shell-file-name "/usr/local/bin/bash")
    (setq explicit-shell-file-name "/bin/bash")))

;; Use another image in startup screen
(use-package startup
  (setq fancy-splash-image "~/.emacs.d/logo-small.png"))

;; Answer by "y" or "n" instead of "yes" or "no"
(use-package subr
  :ensure nil
  :config (fset 'yes-or-no-p 'y-or-n-p))

(use-package time
  :ensure nil
  :config (display-time-mode t))

(use-package simple
  :ensure nil
  :config (column-number-mode t))

(use-package battery
  :ensure nil
  :config (display-battery-mode t))

;; Enable line numbers everywhere
(use-package linum
  :ensure nil
  :config (global-linum-mode t))

;; Make sure all backup files only live in one place
(use-package files
  :ensure nil
  :config (setq backup-directory-alist '(("." . "~/.emacs.d/.backups"))))

;; Toggle fullscreen on startup
(use-package frame
  :ensure nil
  :config (add-hook 'after-init-hook 'toggle-frame-fullscreen))

(use-package winner
  :ensure nil
  :config (winner-mode t))

;; Replace the active region just by typing text, just like modern editors
(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

;; Disable scroll-bar
(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode -1))

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

;; Flycheck
(use-package flycheck
  :config (global-flycheck-mode +1))

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

(provide 'global)
;;; global.el ends here

