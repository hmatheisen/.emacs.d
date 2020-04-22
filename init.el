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
  (defun garbage-collect-defer ()
    "Defer garbage collection."
    (setq gc-cons-threshold most-positive-fixnum
          gc-cons-percentage 0.6))
  (defun garbage-collect-restore ()
    "Return garbage collection to slightly higher parameter."
    (setq gc-cons-threshold 100000000
          gc-cons-percentage 0.1))
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
  ;; Indent using spaces
  (setq-default indent-tabs-mode nil)
  ;; Set tabs to 2
  (setq-default tab-width 2)
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
  (add-to-path ":~/go/bin")
  ;; Default truncate lines
  (setq-default truncate-lines t)
  ;; Set utf8 everywhere
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  ;; Set garbage collection
  (garbage-collect-defer)
  (add-hook 'emacs-startup-hook #'garbage-collect-restore)
  (add-hook 'minibuffer-setup-hook #'garbage-collect-defer)
  (add-hook 'minibuffer-exit-hook #'garbage-collect-restore))

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
  :preface
  (defun my-split-window-right ()
    "Splits window on the right then focus on that window"
    (interactive)
    (split-window-right)
    (other-window 1))
  (defun my-split-window-below ()
    "Splits windmow below then focus on that window"
    (interactive)
    (split-window-below)
    (other-window 1))
  :config
  ;; Resizing
  (global-set-key (kbd "M--") 'shrink-window)
  (global-set-key (kbd "M-+") 'enlarge-window)
  (global-set-key (kbd "C--") 'shrink-window-horizontally)
  (global-set-key (kbd "C-+") 'enlarge-window-horizontally)
  ;; Other window (windmove is also setup but this can be easier)
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "M-O") '(lambda ()
                                 (interactive)
                                 (other-window -1)))
  ;; scroll window up/down by one line
  (global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
  (global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
  ;; Use by own split functions
  (global-set-key (kbd "C-x 2") 'my-split-window-below)
  (global-set-key (kbd "C-x 3") 'my-split-window-right))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package "subr"
  :ensure nil
  :config (fset 'yes-or-no-p 'y-or-n-p))

(use-package time
  :ensure nil
  :config (display-time-mode t))

(use-package simple
  :ensure nil
  :config 
  (column-number-mode t)
  (global-set-key (kbd "C-z") 'advertised-undo))

(use-package battery
  :ensure nil
  :config (display-battery-mode t))

(use-package linum
  :ensure nil
  :config (global-linum-mode t))

(use-package files
  :ensure nil
  :config 
  (setq backup-directory-alist '(("." . "~/.emacs.d/.backups")))
  (setq confirm-kill-emacs #'yes-or-no-p))

(use-package frame
  :ensure nil
  :config
  (add-hook 'after-init-hook 'toggle-frame-fullscreen)
  (set-frame-font "JetBrains Mono-13"))

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

(use-package "ibuffer"
  :config
  ;; Replace command to ibuffer
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  ;; Filter groups
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("dashboard"    (name . "\*dashboard\*"))
           ("Magit"        (name . "\magit*"))
           ("emacs-config" (filename . ".emacs.d"))
           ("Org"          (mode . org-mode))
           ("dired"        (mode . dired-mode)))))
  ;; Add hook
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-switch-to-saved-filter-groups "default")))
  ;; Do not show groups that are empty
  (setq ibuffer-show-empty-filter-groups nil)
  ;; Do not prompt when deleting a new buffer
  (setq ibuffer-expert t))

(use-package compile
  :ensure nil
  :config
  (global-set-key (kbd "C-c C-k") 'recompile))

(use-package spacemacs-common :defer t :ensure spacemacs-theme)
(use-package moe-theme :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package modus-vivendi-theme
  :defer t
  :init
  (setq modus-vivendi-theme-distinct-org-blocks t
        modus-vivendi-theme-rainbow-headings t
        modus-vivendi-theme-section-headings nil
        modus-vivendi-theme-visible-fringe t
        modus-vivendi-theme-slanted-constructs t
        modus-vivendi-theme-bold-constructs t
        modus-vivendi-theme-3d-modeline nil
        modus-vivendi-theme-subtle-diff t
        modus-vivendi-theme-proportional-fonts nil))
(use-package modus-operandi-theme
  :defer t
  :init
  (setq modus-operandi-theme-distinct-org-blocks t
        modus-operandi-theme-rainbow-headings t
        modus-operandi-theme-section-headings nil
        modus-operandi-theme-visible-fringe t
        modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-3d-modeline nil
        modus-operandi-theme-subtle-diff t
        modus-operandi-theme-proportional-fonts nil))

(use-package diminish
  :defer t
  :after use-package)

(use-package counsel
  :defer t
  :diminish ivy-mode counsel-mode
  :bind (("C-s" . swiper-isearch))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package lsp-mode
  :defer t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         (go-mode . lsp)
         (ruby-mode . lsp)
         (typescript-mode . lsp)
         (web-mode . lsp))
  :commands lsp)

(use-package company-mode
  :defer t
  :diminish company-mode
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
  :hook ((after-init . global-company-mode)
          (company-mode . (lambda ()
                            (diminish 'company-mode)))))

(use-package company-lsp
  :defer t
  :after (company lsp)
  :init
  (push 'company-lsp company-backends)
  :commands company-lsp)

(use-package org
  :defer t
  :diminish visual-line-mode
  :preface
  (defun my-org-mode-hook ()
    (org-indent-mode 1)
    (visual-line-mode 1)
    (linum-mode -1)
    (flyspell-mode 1))
  :hook ((org-mode . my-org-mode-hook)
         (org-indent-mode . (lambda ()
                              "Only way I found to make diminish work"
                              (diminish 'org-indent-mode))))
  :config
  (set-face-attribute 'org-document-title nil :height 200)
  (set-face-attribute 'org-level-1        nil :height 160)
  (set-face-attribute 'org-level-2        nil :height 150))

(use-package org-bullets 
  :defer t
  :hook (org-mode . org-bullets-mode))

(use-package toc-org
  :defer t
  :hook ((org-mode      . toc-org-mode)
         (markdown-mode . toc-org-mode)))

(use-package magit
  :defer t
  :bind ("C-x g" . 'magit-status))

(use-package flycheck
  :defer t
  :diminish
  :config (global-flycheck-mode t))

(use-package projectile
  :defer t
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package neotree
  :defer t
  :config 
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind (([f8] . neotree-toggle)))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.4))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package all-the-icons
  :defer t)

(use-package dashboard
  :diminish page-break-lines-mode
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'official
        dashboard-items '((bookmarks . 10)
                          (recents . 5))
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons    t
        dashboard-banner-logo-title "Welcome to He-Macs!"))

(use-package clojure-mode
  :defer t)

(use-package cider
 :defer t)

(use-package typescript-mode
  :defer t
  :config
  (setq typescript-mode-indent-size 2))

(use-package docker
  :defer t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)

(use-package yaml-mode
  :defer t)

(use-package go-mode
  :defer t
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

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
  (setq day-hour 09)
  (setq night-hour 16)
  (setq day-theme 'modus-operandi)
  (setq night-theme 'modus-vivendi))

;;; init.el ends here
