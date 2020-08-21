;;; init.el --- Emacs Init File

;;; Commentary:
;;; My Emacs Init File
;;; This file is generted by `config.org', since there are no comments in here,
;;; you should go have a look there for more information.

;;; Code:

(defconst *is-a-mac* (eq system-type 'darwin)
  "Check whether system is mac.")
(defconst *mono-font* "JetBrains Mono"
  "Mono font to be used")
(defconst *font-size* 12
  "Font size in points")

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/"))
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
  :ensure nil
  :config
  ;; Avoid a few issues on MacOS
  (when *is-a-mac*
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super
          mac-right-option-modifier 'nil
          select-enable-clipboard t))
  ;; title bar settings
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil))

(use-package emacs
  :ensure nil
  :config
  ;; Remove Toolbar
  (tool-bar-mode -1)
  ;; Disable menu bar
  (menu-bar-mode -1)
  ;; Enable downcase/upcase region
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  ;; Trash can support
  (when *is-a-mac*
    (setq trash-directory "~/.Trash"))
  (setq delete-by-moving-to-trash t)
  ;; Indent using spaces
  (setq-default indent-tabs-mode nil)
  ;; Set tabs to 2
  (setq-default tab-width 2)
  ;; Make that damn bell shut up
  (setq ring-bell-function 'ignore)
  ;; Default truncate lines
  (setq-default truncate-lines t)
  ;; Unbind suspend keys
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(use-package emacs
  :ensure nil
  :preface
  (defun garbage-collect-defer ()
    "Defer garbage collection."
    (setq gc-cons-threshold most-positive-fixnum
          gc-cons-percentage 0.6))
  (defun garbage-collect-restore ()
    "Return garbage collection to slightly higher parameter."
    (setq gc-cons-threshold 100000000
          gc-cons-percentage 0.1))
  :config
  ;; Set garbage collection
  (garbage-collect-defer)
  (add-hook 'emacs-startup-hook #'garbage-collect-restore)
  (add-hook 'minibuffer-setup-hook #'garbage-collect-defer)
  (add-hook 'minibuffer-exit-hook #'garbage-collect-restore))

;; Resolve path issues
(use-package emacs
  :preface
  (defun add-to-path (path)
    "Add a path to `exec-path' and Emacs \"PATH\" variable."
    (add-to-list 'exec-path path)
    (setenv "PATH" (concat (getenv "PATH") ":" path)))
  :ensure nil
  :config
  ;; Add useful path to exec-path and PATH
  (add-to-path "/usr/local/bin")
  (add-to-path "/Library/TeX/texbin")
  (add-to-path "~/go/bin")
  (add-to-path "~/.cargo/bin"))

(use-package emacs
  :ensure nil
  :config
  ;; Set utf8 everywhere
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(use-package emacs
  :ensure nil
  :preface
  (defun zz-scroll-half-page (direction)
    "Scrolls half page up if `direction' is non-nil, otherwise will scroll half page down."
    (let ((opos (cdr (nth 6 (posn-at-point)))))
      ;; opos = original position line relative to window
      (move-to-window-line nil)  ;; Move cursor to middle line
      (if direction
          (recenter-top-bottom -1)  ;; Current line becomes last
        (recenter-top-bottom 0))  ;; Current line becomes first
      (move-to-window-line opos)))  ;; Restore cursor/point position

  (defun zz-scroll-half-page-down ()
    "Scrolls exactly half page down keeping cursor/point position."
    (interactive)
    (zz-scroll-half-page nil))

  (defun zz-scroll-half-page-up ()
    "Scrolls exactly half page up keeping cursor/point position."
    (interactive)
    (zz-scroll-half-page t))
  :config
  (global-set-key (kbd "C-v") 'zz-scroll-half-page-down)
  (global-set-key (kbd "M-v") 'zz-scroll-half-page-up))

(use-package custom
  :ensure nil
  :config
  (setq custom-safe-themes t)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(use-package auth-source
  :ensure nil
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo"))
  (setq user-full-name "Henry MATHEISEN")
  (setq user-mail-address "henry.mthsn@gmail.com")
  ;; Disable external pin entry
  (setenv "GPG_AGENT_INFO" nil)
  ;; Solve ioctl common error with GPG
  (setenv "GPG_TTY" "$(tty)"))

(use-package smtpmail
  :config
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl
        ;; Set smtp method for sending mail
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'message-smtpmail-send-it))

(use-package message
  :ensure nil
  :config
  (setq mail-signature "Henry MATHEISEN\nhenry.mthsn@gmail.com\n"
        message-signature "Henry MATHEISEN\nhenry.mthsn@gmail.com\n"))

(use-package "epa-file"
  :ensure nil
  :config
  (setq epa-pinentry-mode 'loopback))

(use-package gnus
  :defer t
  :ensure nil
  :config
  (setq gnus-select-method '(nnnil))
  (setq gnus-secondary-select-methods
        '((nnimap "GMAIL"
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port "imaps")
                 (nnimap-stream ssl))))
  ;; Make Gnus NOT ignore [Gmail] mailboxes
  (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"))

(use-package term
  :ensure nil
  :config
  (if *is-a-mac*
      (setq explicit-shell-file-name "/usr/local/bin/bash")
    (setq explicit-shell-file-name "/bin/bash")))

(use-package "window"
  :ensure nil
  :preface
  (defun hma/split-window-right ()
    "Splits window on the right then focus on that window"
    (interactive)
    (split-window-right)
    (other-window 1))
  (defun hma/split-window-below ()
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
  (global-set-key (kbd "M-n") '(lambda ()
                                 (interactive)
                                 (scroll-up-command 1)))
  (global-set-key (kbd "M-p") '(lambda ()
                                 (interactive)
                                 (scroll-down-command 1)))
  ;; Use by own split functions
  (global-set-key (kbd "C-x 2") 'hma/split-window-below)
  (global-set-key (kbd "C-x 3") 'hma/split-window-right)
  (global-set-key (kbd "C-<tab>") 'next-buffer)
  (global-set-key (kbd "C-S-<tab>") 'previous-buffer))

(use-package ace-window
  :config (global-set-key (kbd "C-x o") 'ace-window))

(use-package windmove
  :ensure nil
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
  (global-set-key (kbd "s-<backspace>")
                  (lambda ()
                    (interactive)
                    (kill-line 0))))

(use-package battery
  :ensure nil
  :config (display-battery-mode t))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
        (yaml-mode . display-line-numbers-mode))

(use-package files
  :ensure nil
  :config
  (setq backup-directory-alist '(("." . "~/.emacs.d/.backups")))
  (setq confirm-kill-emacs #'yes-or-no-p))

(use-package frame
  :ensure nil
  :config
  (add-hook 'after-init-hook 'toggle-frame-fullscreen))

(use-package faces
  :ensure nil
  :config
  (set-face-attribute 'default
                      nil
                      :family *mono-font*
                      :height (* *font-size* 10))

  (set-face-attribute 'fixed-pitch
                      nil
                      :family *mono-font*
                      :height (* *font-size* 10))

  (set-face-attribute 'variable-pitch
                      nil
                      :family "Raleway"
                      :height (* *font-size* 12)))

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
  :ensure nil
  :config
  ;; Replace command to ibuffer
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  ;; Filter groups
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("buffers"      (or (name . "\*dashboard\*")
                               (name . "\*scratch\*")))
           ("clojure"      (or (mode . clojure-mode)
                               (name . "\*cider")
                               (name . "\*nrepl")))
           ("magit"        (name . "magit*"))
           ("he-macs"      (filename . ".emacs.d"))
           ("org"          (mode . org-mode))
           ("dired"        (mode . dired-mode))
           ("code"         (filename . "Projets")))))
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

(use-package info
  :ensure nil
  :config
  (define-key Info-mode-map (kbd "M-n") '(lambda ()
                                           (interactive)
                                           (scroll-up-command 1))))

(use-package js
  :ensure nil
  :config
  (setq js-indent-level 2))

(use-package sh-script
  :ensure nil
  :config (setq sh-basic-offset 2))

(use-package align
  :ensure nil
  :config
  (defun hma/align-equals (beg end)
    "Align `=' signs in a given region."
    (interactive "r")
    (align-regexp beg
                  end
                  "\\(\\s-*\\)=")))

(use-package mwheel
  :ensure nil
  :config (setq mouse-wheel-progressive-speed nil
                mouse-wheel-scroll-amount '(1 ((shift) . 1))))

(use-package modus-vivendi-theme
  :defer t
  :init
  (setq modus-vivendi-theme-distinct-org-blocks t
        modus-vivendi-theme-rainbow-headings t
        modus-vivendi-theme-slanted-constructs t
        modus-vivendi-theme-bold-constructs t
        modus-vivendi-theme-scale-headings t
        modus-vivendi-theme-scale-1 1.05
        modus-vivendi-theme-scale-2 1.1
        modus-vivendi-theme-scale-3 1.15
        modus-vivendi-theme-scale-4 1.2))
(use-package modus-operandi-theme
  :defer t
  :init
  (setq modus-operandi-theme-distinct-org-blocks t
        modus-operandi-theme-rainbow-headings t
        modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-scale-headings t
        modus-operandi-theme-scale-1 1.05
        modus-operandi-theme-scale-2 1.1
        modus-operandi-theme-scale-3 1.15
        modus-operandi-theme-scale-4 1.2))

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
         (rust-mdoe . lsp)
         (web-mode . lsp))
  :commands lsp
  :config
  ;; Do not use lsp for linting
  (setq lsp-diagnostic-package :none))

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
  :diminish visual-line-mode auto-fill-function
  :preface
  (defun hma/org-mode-hook ()
    (org-indent-mode 1)
    (visual-line-mode 1)
    (flyspell-mode 1)
    (auto-fill-mode 1)
    (variable-pitch-mode))
  :hook ((org-mode . hma/org-mode-hook)
         (org-indent-mode . (lambda ()
                              (diminish 'org-indent-mode)))
         (flyspell-mode . (lambda ()
                            (diminish 'flyspell-mode))))
  :config
  ;; Do not set headings face attributes if onve of the modus themes
  ;; is enabled since they already set this up.
  (set-face-attribute 'org-document-title nil :height 200)
  (unless (or (member 'modus-operandi custom-enabled-themes)
              (member 'modus-vivendi custom-enabled-themes))
    (set-face-attribute 'org-level-1        nil :height 160)
    (set-face-attribute 'org-level-2        nil :height 150))
  ;; Unbind C-<tab> to use 'next-buffer
  (define-key org-mode-map (kbd "C-<tab>") nil))

(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode))

(use-package toc-org
  :defer t
  :hook ((org-mode      . toc-org-mode)
         (markdown-mode . toc-org-mode)))

(use-package markdown-mode
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package magit
  :defer t
  :bind ("C-x g" . 'magit-status))

(use-package flycheck
  :diminish
  :config (global-flycheck-mode t))

(use-package projectile
  :defer t
  :diminish
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package neotree
  :defer t
  :config
  (setq neo-window-fixed-size nil)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-hidden-regexp-list '("\\.git$"))
  :bind (([f8] . neotree-toggle)))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1)
  (setq which-key-idle-delay 0.2
        which-key-idle-secondary-delay 0.2))

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

(use-package yasnippet
  :diminish yas-minor-mode
  :config (yas-global-mode t))

(use-package clojure-mode
  :defer t)

(use-package cider
  :defer t)

(use-package typescript-mode
  :defer t
  :config
  (setq typescript-indent-level 2))

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

(use-package tex
  :defer t
  :diminish auto-fill-function
  :ensure auctex
  :config
  ;; Disable auto locale
  (setq TeX-auto-local nil)
  ;; Set TEXINPUTS to recognize classes in custom directory on MacOS
  (when *is-a-mac*
    (setenv "TEXINPUTS" (concat (getenv "TEXINPUTS")
                                ":$HOME/Documents/Notes/classes")))
  :hook (LaTeX-mode . (lambda () (auto-fill-mode 1)
                                 (set-fill-column 80))))

(use-package rvm
  :config
  ;; Unset BUNDLE_PATH set by rvm because somehow it causes bundler to
  ;; install gems in another path than the default one
  (rvm-use-default)
  (setenv "BUNDLE_PATH"))

;; Auto close for ruby
(use-package ruby-electric
  :diminish ruby-electric-mode
  :defer t
  :hook (ruby-mode . ruby-electric-mode))

(use-package olivetti
  :defer t
  :config (setq olivetti-body-width 110))

(use-package terraform-mode
  :defer t
  :bind (("C-c SPC" . hma/align-equals)))

(use-package beacon
  :diminish beacon-mode
  :config (beacon-mode t))

(use-package imenu-list
  :config (global-set-key (kbd "C-:") #'imenu-list-smart-toggle))

(use-package evil  
  :config
  ;; Switch on Evil mode
  (evil-mode t)
  ;; Default state is emacs so Evil is only active when toggling it
  ;; with `C-z'
  (setq evil-default-state 'emacs))

(use-package new-term
  :preface
  (defun hma/new-term-hook ()
    (define-key term-raw-map (kbd "C-c <up>") 'bigger-term-window)
    (define-key term-raw-map (kbd "C-c <down>") 'smaller-term-window)
    (define-key term-raw-map (kbd "C-c q") 'quit-term))
  :ensure nil
  :init
  (setq new-shell "/usr/local/bin/bash")
  (global-set-key (kbd "C-x t") 'toggle-term-window)
  (add-hook 'term-mode-hook 'hma/new-term-hook))

(use-package theme-switcher
  :ensure nil
  :init
  (setq day-hour 09)
  (setq night-hour 15)
  (setq day-theme 'modus-operandi)
  (setq night-theme 'modus-vivendi))

;;; init.el ends here
