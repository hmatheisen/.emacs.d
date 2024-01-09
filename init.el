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
   '("242f33ba517c05f45e075d8ed3d13c0a7b7d1392e0c95d66830029e561607085" "51f3fb81f9233280cb28ee3023e43e82c9307d59d158626881ca14f964d2abeb" default))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(display-line-numbers nil)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(dynamic-completion-mode t)
 '(ediff-merge-split-window-function 'split-window-vertically)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-layout-mode t)
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
   '(markdown-mode evil docker yaml-mode dockerfile-mode minions ef-themes pixel-scroll treemacs rich-minority page-break-lines yasnippet which-key vertico toc-org org-modern orderless marginalia magit iedit corfu consult cape))
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

;;; ================================================================================
;;; Consts
;;; ================================================================================

(defconst *is-a-mac* (eq system-type 'darwin))

;;; ================================================================================
;;; Packages
;;; ================================================================================

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Load lisp code in other directories
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;;; ================================================================================
;;; UI
;;; ================================================================================

(use-package theme-switcher
  :ensure nil
  :config
  (setq theme-switcher-day-theme 'ef-day
        theme-switcher-night-theme 'ef-dark)
  (theme-switcher-mode t))

;; Line numbers in prog mode only
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(when *is-a-mac*
  (setq ns-auto-hide-menu-bar nil
        ns-use-fullscreen-animation t)
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
  (treemacs-width-is-initially-locked nil))

(use-package minions
  :config (minions-mode 1))

;;; ================================================================================
;;; Completion & Navigation
;;; ================================================================================

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
  (corfu-auto-prefix 2)
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

;; Completion at point extensions
(use-package cape)

(use-package yasnippet
  :config (yas-global-mode t))

;; Search and navigation
(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o" . consult-outline)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("C-x r j" . consult-register)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)))

;;; ================================================================================
;;; Files
;;; ================================================================================

;; Backup files live in user emacs directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
;; Auto save files live in temporary file directory
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Dired
(when *is-a-mac*
  (setq insert-directory-program "gls"
        dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"))
(setq dired-dwim-target t)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;;; ================================================================================
;;; Languages
;;; ================================================================================

(require 'format-buffer)

;; Ruby
(nry-format ruby
  :command "stree"
  :args '("format" "--print-width=100"))

;; XML
(nry-format nxml
  :command "xmllint"
  :args '("--format" "-"))

;; SQL
(nry-format sql
  :command "pg_format")

;; Flymake
(use-package flymake
  :hook
  ((prog-mode-hook . flymake-mode-hook))
  :bind
  (:map flymake-mode-map
        ("M-n" . 'flymake-goto-next-error)
        ("M-p" . 'flymake-goto-prev-error)))

;;; ================================================================================
;;; Org
;;; ================================================================================

(use-package org
  :ensure t
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
           "* IN PROGRESS %?\nScheduled: %t"))))


;; Beautiful Org mode
(use-package org-modern
  :init
  (global-org-modern-mode))

(use-package toc-org
  :ensure t
  :defer t
  :hook ((org-mode      . toc-org-mode)
         (markdown-mode . toc-org-mode)))

;;; ================================================================================
;;; Text
;;; ================================================================================

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

(auto-insert-mode t)
(add-to-list 'auto-insert-alist
             '(("\\.rb\\'" . "Ruby frozen string header")
               nil "# frozen_string_literal: true

"))

;;; ================================================================================
;;; Git
;;; ================================================================================

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . 'magit-status)
         :map magit-file-section-map
         ("RET" . magit-diff-visit-file-other-window)
         :map magit-hunk-section-map
         ("RET" . magit-diff-visit-file-other-window))
  :config
  (transient-append-suffix 'magit-log "-A"
    '("-m" "No Merges" "--no-merges")))

;;; ================================================================================
;;; Utils
;;; ================================================================================

(defun nry-add-to-path (path)
  "Add a path to variable `exec-path' and Emacs \"PATH\" variable."
  (add-to-list 'exec-path path)
  (setenv "PATH" (concat (getenv "PATH") ":" path)))

(nry-add-to-path "/usr/local/bin")
(nry-add-to-path "/Library/TeX/texbin")
(nry-add-to-path "/Users/henry/.rbenv/shims")
(nry-add-to-path "/Users/henry/.local/bin")

(defun nry-macroexpand-point (sexp)
  "Expand macro SEXP at point to temp buffer."
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

(defun nry-new-buffer (new-buffer-name)
  "Create a new buffer named NEW-BUFFER-NAME and switch to it."
  (interactive "sNew buffer name: ")
  (switch-to-buffer
   (concat "*" new-buffer-name "*")))

(global-set-key (kbd "C-x B") 'nry-new-buffer)

;;; ================================================================================
;;; Window
;;; ================================================================================

;; Move between windows
(defun other-window-backward ()
  "Move to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-backward)

;;; ================================================================================
;;; Tools
;;; ================================================================================

(use-package vterm
  :load-path (lambda ()
               (expand-file-name "emacs-libvterm" user-emacs-directory)))


(provide 'init)

;;; init.el ends here
