;;; init.el --- Emacs Init File

;;; Commentary:
;;; My Emacs Init File
;;; This file is generted by `config.org', since there are no comments in here,
;;; you should go have a look there for more information.

;;; Code:

(defconst *is-a-mac* (eq system-type 'darwin)
  "Check whether system is mac.")

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(add-to-list 'load-path "~/.emacs.d/site-lisp/theme-switcher")
(add-to-list 'load-path "~/.emacs.d/site-lisp/new-term")

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
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil)
  ;; Make that damn bell shut up
  (setq ring-bell-function 'ignore)
  ;; Add useful path to exec-path and PATH
  (add-to-path ":/usr/local/bin")
  (add-to-path ":/Library/TeX/texbin")
  ;; Default truncate lines
  (setq-default truncate-lines t))

(use-package custom
  :ensure nil
  :config 
  (setq custom-safe-themes t)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(use-package term
  :ensure nil
  :config
  (if *is-a-mac*
      (setq explicit-shell-file-name "/usr/local/bin/bash")
    (setq explicit-shell-file-name "/bin/bash")))

(use-package "window"
  :ensure nil
  :config
  ;; Resizing
  (global-set-key (kbd "M--") 'shrink-window)
  (global-set-key (kbd "M-+") 'enlarge-window)
  (global-set-key (kbd "C--") 'shrink-window-horizontally)
  (global-set-key (kbd "C-+") 'enlarge-window-horizontally)
  ;; Other window
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "M-O") '(lambda ()
                                 (interactive)
                                 (other-window -1))))

(use-package "subr"
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

(use-package linum
  :ensure nil
  :config (global-linum-mode t))

(use-package files
  :ensure nil
  :config (setq backup-directory-alist '(("." . "~/.emacs.d/.backups"))))

(use-package frame
  :ensure nil
  :config (add-hook 'after-init-hook 'toggle-frame-fullscreen))

(use-package winner
  :ensure nil
  :config (winner-mode t))

(use-package delsel
  :ensure nil
  :config (delete-selection-mode +1))

(use-package scroll-bar
  :ensure nil
  :config (scroll-bar-mode -1))

(use-package paren
  :ensure nil
  :init (setq show-paren-delay 0)
  :config (show-paren-mode t))

(use-package ediff
  :ensure nil
  :config (setq ediff-split-window-function 'split-window-horizontally))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package "buff-menu"
  :ensure nil
  :preface
  (defun my-list-buffers (&optional arg)
    "Wrapper around the `buffer-menu-other-window' function to
switch to the newly opened window."
    (interactive "P")
    (buffer-menu-other-window)
    (other-window))
  :config (global-set-key (kbd "C-x C-b") 'my-list-buffers))

(use-package spacemacs-common
  :ensure spacemacs-theme)

(use-package moe-theme)

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s" . swiper-isearch))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package lsp-mode
  :hook ((python-mode . lsp)))

(use-package company-mode
  :init
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous))
  :ensure company
  :hook (after-init . global-company-mode))

(use-package company-lsp
  :after (company lsp)
  :init
  (push 'company-lsp company-backends))

(use-package org
  :preface
  (defun my-org-mode-hook ()
    (org-indent-mode 1)
    (visual-line-mode 1)
    (linum-mode -1))
  :init
  (add-hook 'org-mode-hook 'my-org-mode-hook))

(use-package org-bullets :hook (org-mode . org-bullets-mode))

(use-package toc-org
  :hook ((org-mode      . toc-org-mode)
         (markdown-mode . toc-org-mode)))

(use-package magit
  :ensure t
  :bind ("C-x g" . 'magit-status))

(use-package flycheck
  :config (global-flycheck-mode t))

(use-package treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4))

(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'official
        dashboard-items '((recents . 5)
                          (bookmarks . 5))
        dashboard-center-content t
        dashboard-banner-logo-title "Welcome to He-Macs!"))

(use-package clojure-mode)

(use-package cider)

(use-package new-term
  :preface
  (defun my-new-term-hook ()
    (define-key term-raw-map (kbd "C-c <up>") 'bigger-term-window)
    (define-key term-raw-map (kbd "C-c <down>") 'smaller-term-window)
    (define-key term-raw-map (kbd "C-c q") 'quit-term))
  :ensure nil
  :init
  (setq new-shell "/usr/local/bin/bash")
  (global-set-key (kbd "C-x t") 'toggle-term-window)
  (add-hook 'term-mode-hook 'my-new-term-hook))

(use-package theme-switcher
  :ensure nil
  :init
  (setq day-theme 'moe-dark)
  (setq night-theme 'spacemacs-dark))

;;; init.el ends here
