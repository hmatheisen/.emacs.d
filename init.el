;;; init.el --- Emacs base config file               -*- lexical-binding: t; -*-

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
   '("0c5d7ffa7cdf8b889342d2cd38f6ddd2cc716561048feca89b17fda56677d6b8" "694dbeb8f98dddfb603a2fe0c04101f3fe457ee49bf90a6a581271e7f9c580c8" "b216e9b72dc8c2b702e4fcfd3c0af2d73c87eba46fd4db824ddb50863447d6a9" "765b9109dfdd2f82590bf4d5452cc134d7de2163e24a160efd1887ee57c59413" "2777f300b438d2d061560c6a1afac9723e7f840413b12a471055428269ee17dd" "2ca3da7d36b0d326f984530a07be54b272b5c313b1361989acf747d8b5616162" "9ed206ff6874db89cb4a588c6cdc75a7b056fecbc9880e9758881bdef6d9d79a" "01aef17f41edea53c665cb57320bd80393761f836be5ab0bd53292afc94bd14d" "ca934a76aae4ff950288e082be75a68eb7bac6e8d3dd58b28649993540412ed6" "714394050e703db8a773ed350ca6f9cb6636d4bf2e348514804a48929aafc762" "d0f3adfe292c9d633930e35c3458cda77796073bb25af852689f999bbb3d9398" "242f33ba517c05f45e075d8ed3d13c0a7b7d1392e0c95d66830029e561607085" "51f3fb81f9233280cb28ee3023e43e82c9307d59d158626881ca14f964d2abeb" default))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(display-line-numbers nil)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(dynamic-completion-mode t)
 '(ediff-merge-split-window-function 'split-window-vertically)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-pair-mode t)
 '(elfeed-feeds '("https://planet.emacslife.com/atom.xml"))
 '(fill-column 80)
 '(follow-auto t)
 '(global-auto-revert-mode t)
 '(global-goto-address-mode t)
 '(global-revert-mode t)
 '(global-so-long-mode t)
 '(global-tab-line-mode nil)
 '(go-ts-mode-indent-offset 4)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(lua-indent-level 4)
 '(mode-line-compact nil)
 '(ns-auto-hide-menu-bar nil)
 '(ns-use-fullscreen-animation t)
 '(package-selected-packages
   '(altcaps lorem-ipsum go-mode multiple-cursors jsdoc vundo rainbow-mode lua-mode fennel-mode multi-vterm company flymake-kondor restclient sass-mode beacon sly olivetti emmet prodigy ac-geiser geiser-guile geiser eglot flymake-eslint emmet-mode diff-hl rubocop csv-mode hl-todo elfeed inf-ruby undo-tree wgrep embark-consult embark prettier ruby-electric ibuffer-project dired-git-info helpful doom-modeline diredfl dired-x cider clojure-mode markdown-mode evil docker yaml-mode dockerfile-mode minions ef-themes pixel-scroll treemacs rich-minority page-break-lines yasnippet which-key vertico toc-org org-modern orderless marginalia magit iedit corfu consult cape))
 '(pixel-scroll-precision-mode t)
 '(recentf-mode t)
 '(repeat-mode t)
 '(ring-bell-function 'ignore)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(show-paren-delay 0)
 '(show-trailing-whitespace nil)
 '(smtpmail-smtp-server "posteo.de")
 '(smtpmail-smtp-service 25)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(trash-directory "~/.Trash")
 '(truncate-lines t)
 '(use-package-always-ensure t)
 '(user-mail-address "haineriz@posteo.de")
 '(warning-minimum-level :emergency)
 '(windmove-default-keybindings '([ignore]))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka Comfy Fixed" :foundry "nil" :slant normal :weight regular :height 160 :width normal))))
 '(org-document-title ((t (:height 1.7))))
 '(org-level-1 ((t (:height 1.5))))
 '(org-level-2 ((t (:height 1.3))))
 '(org-level-3 ((t (:height 1.1)))))

;;; ============================================================================
;;; Consts
;;; ============================================================================

(defconst *is-a-mac* (eq system-type 'darwin))

;;; ============================================================================
;;; Packages
;;; ============================================================================

(require 'package)

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
  :custom
  (theme-switcher-day-theme 'ef-arbutus)
  (theme-switcher-night-theme 'ef-cherie)
  :config
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
  :custom
  (which-key-idle-delay 1.0)
  :init
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
  (treemacs-width 50)
  (treemacs-width-is-initially-locked nil)
  :bind (("s-b" . treemacs)))

(use-package doom-modeline
  :custom
  (doom-modeline-height 20)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-icon nil)
  (doom-modeline-time-analogue-clock nil)
  :config
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
  (ibuffer-expert t)
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

;; Reorder so Dabbrev is tried first
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

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

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(require 'workspace)

(setq workspace-config
      '(("~/Code/elevo-rails/" . (dired magit))
        ("~/org/"              . ("sprint.org" "tasks.org"))
        ("~/.emacs.d/"         . (dired magit))))

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
(require 'tasks)

;; Eglot
(use-package eglot
  :ensure nil
  :preface
  (defun setup-other-flymake-backends ()
    "Add other backends to flymake when using eglot"
    (cond ((derived-mode-p 'typescript-ts-mode 'tsx-ts-mode)
           (flymake-eslint-enable))
          ((derived-mode-p 'ruby-ts-mode)
           (add-hook 'flymake-diagnostic-functions
                     'ruby-flymake-auto))))
  :hook ((ruby-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (eglot-managed-mode . setup-other-flymake-backends))
  :config
  (add-to-list 'eglot-server-programs '(fennel-mode . ("fennel-ls"))))

;; Ruby
(format-lang ruby-ts
  :command "stree"
  :args '("format" "--print-width=100"))

(use-package ruby-electric
  :hook ((ruby-ts-mode . ruby-electric-mode)))

;; Rails commands
(define-task rails-gettext
  :command "bundle exec rails gettext:update"
  :project-path "~/Code/elevo-rails/"
  :description "Update gettext entries")

(define-task rails-migrate
  :command "bundle exec rails db:migrate"
  :project-path "~/Code/elevo-rails/"
  :description "Run rails migration")

(define-task sort-forestschema
  :command "bin/sort_forestschema"
  :project-path "~/Code/elevo-rails/"
  :description "Sort `.forestadmin-schema.json' file"
  :async nil)

(define-task yarn-client-tests
  :command "yarn client:test"
  :project-path "~/Code/elevo-rails/"
  :description "Run all client tests")

(define-task annotate-models
  :command "bundle exec rails annotate_models"
  :project-path "~/Code/elevo-rails/"
  :description "Annotate rails models")

;; Typescript
(use-package prettier
  :bind (:map tsx-ts-mode-map
         ("C-c f" . prettier-prettify)
         :map typescript-ts-mode-map
         ("C-c f" . prettier-prettify)))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(use-package emmet-mode
  :hook ((tsx-ts-mode . emmet-mode)))

;; Enable eslint for flymake
(use-package flymake-eslint)

;; Emacs lisp
(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

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
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (tsx-mode . tsx-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode . yaml-ts-mode)))

;; Markdown
(use-package markdown-mode
  :hook ((markdown-mode . (lambda ()
                            ;; Do not remove trailing whitespace when cleaning in markdown mode
                            (setq-local whitespace-style
                                        (delq 'trailing whitespace-style)))))
  :config
  (define-skeleton markdown-release-skeleton
    "Release template."
    "Insert Release Tag: "
    "# " str "\n\n"
    "## Stories\n\n"
    "## Bugfixes\n\n"
    "## Tasks\n\n"
    "## Migrations\n\n"
    "## Rollback Plan\n")
  ;; Use `markdown-release' as auto-insert on 'release.md' files.
  (define-auto-insert
    '("release.md" . "Release template") 'markdown-release-skeleton))

;; Quicklisp
;; (load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

;; Clojure
(use-package cider)
(use-package clojure-mode)
(use-package flymake-kondor
  :ensure t
  :hook (clojure-mode . flymake-kondor-setup))

;; Lua
(format-lang lua
  :command "lua-format"
  :args '("-i"))

;; Fennel
(use-package fennel-mode)
(use-package lua-mode)

;; Golang
(use-package go-mode)

;;; ============================================================================
;;; Org
;;; ============================================================================

(use-package org
  :preface
  (defun my-org-mode-hook ()
    (org-indent-mode 1)
    (visual-line-mode 1)
    (auto-fill-mode 1)
    (abbrev-mode 1)
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
           "* IN PROGRESS %?\nSCHEDULED: %t")))
  (define-skeleton org-refinement-skeleton
    "Refinement Template."
    "Insert Refinement subject: "
    "TITLE: " str "\n"
    "#+OPTIONS: toc:nil\n\n"
    "* Notes\n"
    "* Tech Solution\n"
    "** Backend\n"
    "*** Routes\n"
    "*** Validations\n"
    "*** Serializers/Presenters\n"
    "*** Policies\n"
    "** Frontend\n"
    "* Questions\n"
    "** Product\n"
    "** Tech\n"
    "* Tickets\n")
  (define-auto-insert
    '("refinements/.*\.org" . "Refinement template")
    'org-refinement-skeleton))

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

(require 'isearch-transient)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(defun align-equals (beg end)
  "Align `=' signs in a given region, from BEG to END."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)="))

;; Whitespace cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; (use-package evil
;;   :init
;;   (setq evil-want-C-u-scroll t
;;         evil-default-state 'emacs
;;         evil-insert-state-modes nil
;;         evil-motion-state-modes nil
;;         evil-disable-insert-state-bindings t
;;         evil-insert-state-cursor nil)
;;   :config
;;   (evil-set-undo-system 'undo-redo)
;;   (evil-mode 1))

(use-package iedit)

;; Auto insert mode
(auto-insert-mode t)
(define-auto-insert
  '("\\.rb\\'" . "Ruby frozen string header")
  '(nil
    "# frozen_string_literal: true\n"
    "\n"))

;; Visual undo tree
;; (use-package undo-tree
;;   :config
;;   (setq undo-tree-history-directory-alist
;;         `(("." . ,(concat user-emacs-directory "undo"))))
;;   (global-undo-tree-mode))

;; Redo binding with super
(global-set-key (kbd "s-Z") 'undo-redo)

(use-package vundo
  :bind (("C-x u" . vundo)))

;; Kill to end of line
(defun kill-beg-line ()
  "Kill a line from point to column 0."
  (interactive)
  (kill-line 0))

(global-set-key (kbd "s-<backspace>") 'kill-beg-line)

;; Auto fill in text mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Ispell
(setq ispell-program-name "aspell")

;; Abbrev mode by default in all buffers
(setq-default abbrev-mode t)

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(add-to-list 'grep-find-ignored-directories "node_modules")
(add-to-list 'grep-find-ignored-directories "vendor")

;;; ============================================================================
;;; Project
;;; ============================================================================

(defmacro with-current-project-root (root &rest body)
  "Execute BODY with ROOT as the current project root."
  (declare (indent 1))
  `(let* ((project (project-current t))
          (root (project-root project)))
     ,@body))

;; Copy file absolute path
(defun project-absolute-file-path ()
  "Print and kill the absolute file path of the current buffer in a project."
  (interactive)
  (with-current-project-root (root)
    (let ((absolute-file-path (file-relative-name buffer-file-name root)))
      (kill-new absolute-file-path)
      (message (concat "Saved \"" absolute-file-path "\" to kill ring")))))

(define-key project-prefix-map "\C-y" 'project-absolute-file-path)

;; Open vterm at root of project
(defun project-vterm ()
  "Open a vterm at the root of the current project."
  (interactive)
  (with-current-project-root (root)
    (vterm)
    (vterm-send-string (concat "cd " root))
    (vterm-send-return)
    (vterm-clear)))

(define-key project-prefix-map "T" 'project-vterm)

(defun project-rails-console ()
  "Open a rails console at the root of the current project."
  (interactive)
  (with-current-project-root (root)
    (inf-ruby-console-rails root)))

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

(use-package diff-hl
  :after (magit)
  :config
  (diff-hl-margin-mode t)
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

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
  (let ((buffer-name "*el-macroexpansion**"))
    (with-output-to-temp-buffer buffer-name
      (pp (macroexpand sexp)))
    (with-current-buffer buffer-name
      (emacs-lisp-mode)
      (view-mode))))

(defun new-buffer (new-buffer-name)
  "Create a new buffer named NEW-BUFFER-NAME and switch to it."
  (interactive "sNew buffer name: ")
  (switch-to-buffer
   (concat "*" new-buffer-name "*")))

(global-set-key (kbd "C-x B") 'new-buffer)

;; Set async shell command output buffer to view-mode
(defun set-buffer-to-view-mode (_command &optional output-buffer _error-buffer)
  "Advice function to set async shell command OUTPUT-BUFFER to view mode."
  (let ((buffer (or output-buffer shell-command-buffer-name-async)))
    (with-current-buffer buffer
      (view-mode))))

(advice-add 'async-shell-command
            :after
            #'set-buffer-to-view-mode)

(defun repeat-last-async-shell-command ()
  "Repeats the last shell command in as an `async-shell-command'."
  (interactive)
  (async-shell-command
   (car shell-command-history)))

(put 'list-timers 'disabled nil)

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

(defun window-half-height ()
  "Return half the height of a window."
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-half-page-up-command ()
  "Scroll up half the height of a window."
  (interactive)
  (scroll-up-command (window-half-height)))

(defun scroll-half-page-down-command ()
  "Scroll down half the height of a window."
  (interactive)
  (scroll-down-command (window-half-height)))

(global-set-key (kbd "C-v") 'scroll-half-page-up-command)
(global-set-key (kbd "M-v") 'scroll-half-page-down-command)

(defun split-window-right-focus ()
  "Splits whe window below and move point to new window."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-window-below-focus ()
  "Splits whe window below and move point to new window."
  (interactive)
  (split-window-below)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'split-window-below-focus)
(global-set-key (kbd "C-x 3") 'split-window-right-focus)

;;; ============================================================================
;;; Tools
;;; ============================================================================

(use-package vterm
  :load-path (lambda ()
               (expand-file-name "emacs-libvterm" user-emacs-directory)))

(use-package copilot
  :load-path (lambda ()
               (expand-file-name "copilot.el" user-emacs-directory))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)))

(provide 'init)

;;; init.el ends here
