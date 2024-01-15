;;; init.el --- Emacs base config file               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Henry MATHEISEN

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
;; along with this program.  If not, see <https://www.gnu.orglicenses/>.

;;; Commentary:

;;

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(completion-styles '(orderless basic partial-completion emacs22))
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-safe-themes
   '("ca934a76aae4ff950288e082be75a68eb7bac6e8d3dd58b28649993540412ed6" "714394050e703db8a773ed350ca6f9cb6636d4bf2e348514804a48929aafc762" "d0f3adfe292c9d633930e35c3458cda77796073bb25af852689f999bbb3d9398" "242f33ba517c05f45e075d8ed3d13c0a7b7d1392e0c95d66830029e561607085" "51f3fb81f9233280cb28ee3023e43e82c9307d59d158626881ca14f964d2abeb" default))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(display-line-numbers nil)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(dynamic-completion-mode t)
 '(ediff-merge-split-window-function 'split-window-vertically)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-pair-mode t)
 '(fill-column 80)
 '(follow-auto t)
 '(global-auto-revert-mode t)
 '(global-goto-address-mode t)
 '(global-so-long-mode t)
 '(global-tab-line-mode nil)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(mode-line-compact nil)
 '(ns-auto-hide-menu-bar nil)
 '(ns-use-fullscreen-animation t)
 '(package-selected-packages
   '(eglot prettier ruby-electric ibuffer-project dired-git-info helpful doom-modeline diredfl dired-x cider clojure-mode markdown-mode evil docker yaml-mode dockerfile-mode minions ef-themes pixel-scroll treemacs rich-minority page-break-lines yasnippet which-key vertico toc-org org-modern orderless marginalia magit iedit corfu consult cape))
 '(pixel-scroll-precision-mode t)
 '(recentf-mode t)
 '(repeat-mode t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(show-paren-delay 0)
 '(show-trailing-whitespace nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(trash-directory "~/.Trash")
 '(truncate-lines t)
 '(use-package-always-ensure t)
 '(user-mail-address "haineriz@posteo.de")
 '(windmove-default-keybindings '([ignore]))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Comfy Fixed" :foundry "nil" :slant normal :weight regular :height 160 :width normal)))))

;;; ============================================================================
;;; Consts
;;; ============================================================================

(defconst *is-a-mac* (eq system-type 'darwin))

;;; ============================================================================
;;; Packages
;;; ============================================================================

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Load lisp code in other directories
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;;; ============================================================================
;;; UI
;;; ============================================================================

(use-package theme-switcher
  :ensure nil
  :config
  (setq theme-switcher-day-theme 'ef-day
        theme-switcher-night-theme 'ef-winter)
  (theme-switcher-mode t))

;; Line numbers in prog mode only
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(when *is-a-mac*
  (setq ns-auto-hide-menu-bar nil
        ns-use-fullscreen-animation t)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-right-option-modifier 'nil)
  (add-hook 'emacs-startup-hook 'toggle-frame-fullscreen))

(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

(use-package which-key
  :init
  (setq which-key-idle-delay 0)
  (which-key-mode))

;; Minibuffer
(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package page-break-lines
  :config (global-page-break-lines-mode))

(use-package treemacs
  :custom
  (treemacs-no-png-images t)
  (treemacs-width 40)
  (treemacs-width-is-initially-locked nil)
  :bind (("s-b" . treemacs)))

;; (use-package minions
;;   :config (minions-mode 1))

(use-package doom-modeline
  :config
  (setq doom-modeline-height 20
        doom-modeline-buffer-encoding nil
        doom-modeline-icon nil)
  (doom-modeline-mode 1))

;; A better *help* buffer
(use-package helpful
  :bind (("C-h v" . helpful-variable)
         ("C-h f" . helpful-callable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

(use-package ibuffer
  :custom
  (ibuffer-use-other-window t)
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

;; Group ibuffer's list by project
(use-package ibuffer-project
  :preface
  (defun ibuffer-hook ()
    "Group ibuffer's list by project."
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  :hook (ibuffer . ibuffer-hook))

;;; ============================================================================
;;; Completion & Navigation
;;; ============================================================================

;; Completions
(use-package orderless)

;; Replace dabbrev-expand with hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; In buffer completion
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 3)
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

;; Completion at point extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet
  :config (yas-global-mode t))

;; Search and navigation
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history)))

;;; ============================================================================
;;; Files
;;; ============================================================================

;; Backup files live in user emacs directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
;; Auto save files live in temporary file directory
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Dired
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-q" . wdired-change-to-wdired-mode))
  :config
  (when (and *is-a-mac* (executable-find "gls"))
    (setq insert-directory-program "gls"
          dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"))
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  ;; Guess a default target directory
  (setq dired-dwim-target t))

(use-package diredfl
  :config (diredfl-global-mode))

;;; ============================================================================
;;; Languages
;;; ============================================================================

(require 'format-buffer)

;; Ruby
(format-lang ruby
  :command "stree"
  :args '("format" "--print-width=100"))

;; XML
(format-lang nxml
  :command "xmllint"
  :args '("--format" "-"))

;; SQL
(format-lang sql
  :command "pg_format")

;; Flymake
(use-package flymake
  :hook
  ((prog-mode-hook . flymake-mode-hook))
  :bind
  (:map flymake-mode-map
        ("M-n" . 'flymake-goto-next-error)
        ("M-p" . 'flymake-goto-prev-error)))

;; YAML mode
(use-package yaml-mode)

;; Remap major modes to their treesit equivalent
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (css-mode . css-ts-mode)
        (go-mode . go-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (tsx-mode . tsx-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(add-hook 'ruby-ts-mode-hook 'eglot-ensure)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)

;;; ============================================================================
;;; Org
;;; ============================================================================

(use-package org
  :preface
  (defun my-org-mode-hook ()
    (org-indent-mode 1)
    (visual-line-mode 1)
    (auto-fill-mode 1)
    (windmove-mode -1))
  :hook ((org-mode . my-org-mode-hook))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  ;; files
  (setq org-directory "~/org/")
  (setq org-agenda-files (list org-directory))
  ;; others
  (setq org-startup-folded 'fold)
  (setq org-log-done 'time)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "ON GOING(g)" "|" "DONE(d)" "WON'T DO(w)")
          (sequence "TODO(T)" "IN PROGRESS(P)" "TO REVIEW(R)" "TO TEST(F)" "READY TO MERGE(M)" "|" "DONE(D)")))
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file (lambda () (concat org-directory "tasks.org")))
           "* TODO %?\n")
          ("i" "Emacs Ideas" entry
           (file+headline (lambda () (concat org-directory "journal.org")) "Ideas")
           "* %?\nEntered on: %u")
          ("T" "Ticket" entry
           (file+headline (lambda () (concat org-directory "sprint.org")) "Tickets")
           "* IN PROGRESS %?\nSCHEDULED: %t"))))

;; Beautiful Org mode
(use-package org-modern
  :init
  (global-org-modern-mode))

(use-package toc-org
  :defer t
  :hook ((org-mode      . toc-org-mode)
         (markdown-mode . toc-org-mode)))

;;; ============================================================================
;;; Text
;;; ============================================================================

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(defun align-equals (beg end)
  "Align `=' signs in a given region, from BEG to END."
  (interactive "r")
  (align-regexp beg
                end
                "\\(\\s-*\\)="))

;; Whitespace cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Change whitespace style for markdown files
(add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local
             whitespace-style
             (delq 'trailing whitespace-style))))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t ;; C-u is Vi behaviour
        evil-default-state 'emacs ;; Default mode is emacs
        evil-insert-state-modes nil ;; Override list of buffers where default state is something else than "emacs"
        evil-motion-state-modes nil
        evil-disable-insert-state-bindings t ;; Use Emacs binding in insert state
        evil-insert-state-cursor nil) ;; Don't change cursor in insert mode
  ;; (setq evil-normal-state-modes '(ruby-mode typescript-mode web-mode))
  :config
  (evil-set-undo-system 'undo-redo) ;; Customize undo system
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "SPC") 'evil-ex)) ;; Use SPC instead of : for commands
  (evil-mode 1))

(use-package iedit)

;; Auto insert mode
(auto-insert-mode t)
(add-to-list 'auto-insert-alist
             '(("\\.rb\\'" . "Ruby frozen string header")
               nil "# frozen_string_literal: true

"))

(require 'isearch-transient)

;;; ============================================================================
;;; Git
;;; ============================================================================

(use-package magit
  :defer t
  :bind (("C-x g" . 'magit-status)
         :map magit-file-section-map
         ("RET" . magit-diff-visit-file-other-window)
         :map magit-hunk-section-map
         ("RET" . magit-diff-visit-file-other-window))
  :config
  (transient-append-suffix 'magit-log "-A"
    '("-m" "No Merges" "--no-merges")))

;;; ============================================================================
;;; Utils
;;; ============================================================================

(defun add-to-path (path)
  "Add a path to variable `exec-path' and Emacs \"PATH\" variable."
  (add-to-list 'exec-path path)
  (setenv "PATH" (concat (getenv "PATH") ":" path)))

(add-to-path "/usr/local/bin")
(add-to-path "/Library/TeX/texbin")
(add-to-path "/Users/henry/.rbenv/shims")
(add-to-path "/Users/henry/.local/bin")

(defun macroexpand-point (sexp)
  "Expand macro SEXP at point to temp buffer."
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*"
    (emacs-lisp-mode)
    (read-only-mode)))

(defun new-buffer (new-buffer-name)
  "Create a new buffer named NEW-BUFFER-NAME and switch to it."
  (interactive "sNew buffer name: ")
  (switch-to-buffer
   (concat "*" new-buffer-name "*")))

(global-set-key (kbd "C-x B") 'new-buffer)

;;; ============================================================================
;;; Window
;;; ============================================================================

;; Move between windows
(defun other-window-backward ()
  "Move to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-backward)

;;; ============================================================================
;;; Tools
;;; ============================================================================

(use-package vterm
  :load-path (lambda ()
               (expand-file-name "emacs-libvterm" user-emacs-directory)))


(provide 'init)

;;; init.el ends here
