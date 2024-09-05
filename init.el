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
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(completion-styles '(orderless basic partial-completion emacs22))
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-safe-themes
   '("835d934a930142d408a50b27ed371ba3a9c5a30286297743b0d488e94b225c5f"
     "f4d1b183465f2d29b7a2e9dbe87ccc20598e79738e5d29fc52ec8fb8c576fcfd"
     "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
     "c3e62e14eb625e02e5aeb03d315180d5bb6627785e48f23ba35eb7b974a940af"
     "587ce9a1a961792114991fd488ef9c3fc37f165f6fea8b89d155640e81d165a3"
     "841b6a0350ae5029d6410d27cc036b9f35d3bf657de1c08af0b7cbe3974d19ac"
     "ffdf8617d6e0f1264e5879d3ac919d0f1d8c91d38f2c769e4fa633ddbab248bf" default))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(display-line-numbers nil)
 '(display-line-numbers-width 4)
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(dynamic-completion-mode t)
 '(ediff-merge-split-window-function 'split-window-vertically)
 '(ediff-split-window-function 'split-window-vertically)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-pair-mode t)
 '(fill-column 80)
 '(follow-auto t)
 '(geiser-chez-binary "chez")
 '(global-auto-revert-mode t)
 '(global-goto-address-mode t)
 '(global-revert-mode t)
 '(global-so-long-mode t)
 '(global-tab-line-mode nil)
 '(go-ts-mode-indent-offset 4)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(lua-indent-level 4)
 '(mode-line-compact 'long)
 '(ns-auto-hide-menu-bar nil)
 '(ns-use-fullscreen-animation t)
 '(package-selected-packages
   '(acme-theme altcaps cape casual-suite cider copilot corfu diff-hl diredfl
                dogears doom-themes ef-themes eglot embark-consult emmet-mode
                fennel-mode flymake-eslint flymake-kondor gcmh geiser-chez
                glsl-mode helpful ibuffer-project inf-ruby lua-mode magit
                marginalia markdown-mode multiple-cursors nerd-icons-corfu
                olivetti orderless org-modern page-break-lines prettier
                rainbow-delimiters ruby-electric sass-mode sly standard-themes
                toc-org treemacs uwu-theme vertico vterm vundo wgrep yaml-mode
                yari yasnippet))
 '(package-vc-selected-packages
   '((copilot :vc-backend Git :url
              "https://www.github.com/copilot-emacs/copilot.el")))
 '(pixel-scroll-precision-mode t)
 '(project-switch-commands
   '((project-find-file "Find file" nil) (project-find-regexp "Find regexp" nil)
     (project-find-dir "Find directory" nil) (project-vc-dir "VC-Dir" nil)
     (project-eshell "Eshell" nil) (project-any-command "Other" nil)))
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
 '(tab-width 4)
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
 '(default ((t (:family "Go Mono" :foundry "nil" :slant normal :weight regular :height 140 :width normal))))
 '(emoji ((t (:height 1))))
 '(org-document-title ((t (:height 1.7))))
 '(org-level-1 ((t (:height 1.5))))
 '(org-level-2 ((t (:height 1.3))))
 '(org-level-3 ((t (:height 1.1))))
 '(variable-pitch ((t (:height 150 :family "Go")))))


;;; Consts

(defconst *is-a-mac* (eq system-type 'darwin))


;;; Packages

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Load lisp code in other directories
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))


;;; System

(use-package gcmh
  :config (gcmh-mode 1))


;;; UI

(use-package ef-themes)

(use-package theme-switcher
  :ensure nil
  :after ef-themes
  :custom
  (theme-switcher-day-theme 'acme)
  (theme-switcher-night-theme 'doom-ir-black)
  :config
  (theme-switcher-mode t))

;; Line numbers in prog mode only
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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
  :bind (("s-t" . treemacs)))

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

(when *is-a-mac*
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-right-option-modifier 'nil))

;; Display buffer alist
(add-to-list 'display-buffer-alist
             '("\\*eldoc\\*"
               (display-buffer-below-selected)
               (window-height . 0.25)))

(add-to-list 'display-buffer-alist
             '("\\*Flymake diag.+\\*"
               (display-buffer-below-selected)
               (window-height . 0.25)))

;; (add-to-list 'display-buffer-alist
;;              '("\\*Customize.+\\*"
;;                (display-buffer-reuse-window)
;;                (side . right)))

;; (add-to-list 'display-buffer-alist
;;              '("\\*info\\*"
;;                (display-buffer-reuse-window)
;;                (side . right)))

(defvar init-minor-mode-alist minor-mode-alist)

(defvar mode-line-hidden-minor-modes
  '(diff-hl-mode
    emmet-mode
    ruby-electric-mode
    copilot-mode
    yas-minor-mode
    page-break-lines-mode
    which-key-mode
    gcmh-mode
    eldoc-mode
    abbrev-mode
    with-editor-mode))

(defun clean-minor-mode-line ()
  "Clean up minor mode line."
  (interactive)
  (dolist (mode mode-line-hidden-minor-modes nil)
    (let ((cell (cdr (assoc mode minor-mode-alist))))
      (when cell
        (setcar cell "")))))

(add-hook 'after-change-major-mode-hook 'clean-minor-mode-line)

(use-package casual-suite
  :config
  (require 'casual-suite)
  (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
  (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
  (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)
  (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
  (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
  (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
  (keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
  (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
  (keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu)
  (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
  (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
  (keymap-global-set "M-g" #'casual-avy-tmenu))

;;; Completion & Navigation

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

(use-package nerd-icons-corfu)

;; In buffer completion
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 3)
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
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
         ("s-b" . consult-buffer)
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
         ("M-r" . consult-history))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))


;;; Files

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


;;; Languages

(require 'format-buffer)
(require 'tasks)

;; Eglot
(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
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
         (go-ts-mode . eglot-ensure)
         (eglot-managed-mode . setup-other-flymake-backends)))

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
  :config
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c f") 'prettier-prettify)))
  (add-hook 'tsx-ts-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c f") 'prettier-prettify))))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(use-package emmet-mode
  :hook ((tsx-ts-mode . emmet-mode)))

;; Sass
(use-package sass-mode)

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

;; Golang
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mod\\'" . go-mod-ts-mode))

(format-lang c
  :command "clang-format"
  :args '("--style=file"))

(add-hook 'c-mode-hook (lambda ()
                         (display-fill-column-indicator-mode 1)))
(setq-default c-basic-offset 4)

;; Glsl
(use-package glsl-mode)

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

;; Scheme
(use-package geiser)
(use-package geiser-chez)

;; Treesit
(setq treesit-language-source-alist
      '((ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (c "https://github.com/tree-sitter/tree-sitter-c")))

(defun install-treesit-languages ()
  "Install all treesit languages."
  (interactive)
  (mapc (lambda (lang-alist)
          (let ((lang (car lang-alist)))
            (unless (treesit-language-available-p lang)
              (message "Language %s not available" lang))
            (treesit-install-language-grammar lang)))
        treesit-language-source-alist))


;; Remap major modes to their treesit equivalent
(setq major-mode-remap-alist
      '((ruby-mode . ruby-ts-mode)
        (tsx-mode . tsx-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)
        (c-mode . c-ts-mode)))


;;; Org

(use-package org
  :preface
  (defun my-org-mode-hook ()
    (org-indent-mode 1)
    (visual-line-mode 1)
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
    "#+TITLE: " str "\n"
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
  :hook ((org-mode      . toc-org-mode)
         (markdown-mode . toc-org-mode)))


;;; Text

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

;; Auto insert mode
(auto-insert-mode t)
(define-auto-insert
  '("\\.rb\\'" . "Ruby frozen string header")
  '(nil
    "# frozen_string_literal: true\n"
    "\n"))

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
(add-to-list 'grep-find-ignored-directories "log")
(add-to-list 'grep-find-ignored-directories "tmp")
(add-to-list 'grep-find-ignored-directories "coverage")
(add-to-list 'grep-find-ignored-directories "vendor")

(defun kill-ring-save-whole-buffer ()
  "Save whole buffer to the kill ring."
  (interactive)
  (kill-ring-save
   (point-min)
   (point-max))
  (message "Buffer saved to kill ring."))

(global-set-key (kbd "C-x C-y") 'kill-ring-save-whole-buffer)

(use-package avy
  :bind (("M-i" . avy-goto-char)
         ("M-j" . avy-goto-char-timer)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package altcaps)

(global-set-key (kbd "C-S-j") 'join-line)

;;; Project

(defmacro with-current-project-root (root &rest body)
  "Execute BODY with ROOT as the current project root."
  (declare (indent 1))
  (let ((project (gensym)))
    `(let* ((,project (project-current t))
            (root (project-root ,project)))
       ,@body)))

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

;; Too slow on current project and messes up with completion
;; (defun project-rails-console ()
;;   "Open a rails console at the root of the current project."
;;   (interactive)
;;   (with-current-project-root (root)
;;     (inf-ruby-console-rails root)))

(defvar project-rails-console-buffer nil
  "Reference to the currently open rails console buffer.")

(defun project-rails-console ()
  "Open a rails console at the root of the current project."
  (interactive)
  (if (buffer-live-p project-rails-console-buffer)
      (switch-to-buffer project-rails-console-buffer)
    (with-current-project-root (root)
      (term shell-file-name)
      (term-send-string (get-buffer-process (current-buffer))
                        (concat "cd " root " && bin/rails c\n"))
      (setq project-rails-console-buffer (current-buffer)))))

(defun project-rails-console-send-region (start end)
  "Send the current region to the rails console from START to END."
  (interactive "r")
  (with-current-project-root (root)
    (term-send-string (get-buffer-process project-rails-console-buffer)
                      (buffer-substring-no-properties start end))))

(define-key global-map (kbd "s-p") project-prefix-map)
(define-key project-prefix-map (kbd "g") 'consult-ripgrep)
(define-key project-prefix-map (kbd "t") 'project-vterm)

;;; Git

(use-package magit
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
  :custom
  (diff-hl-draw-borders nil)
  :config
  (diff-hl-margin-mode t)
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


;;; Utils

(defun add-to-path (path)
  "Add a path to variable `exec-path' and Emacs \"PATH\" variable."
  (add-to-list 'exec-path path)
  (setenv "PATH" (concat (getenv "PATH") ":" path)))

(defun chez-scheme-set-env (dir)
  "List dirs in chez scheme lib DIR and sets the proper env."
  (let* ((scheme-lib-dirs
          (directory-files dir t
                           directory-files-no-dot-files-regexp))
         (env (string-join scheme-lib-dirs ":")))
    (setenv "CHEZSCHEMELIBDIRS" (concat env ":."))))

(when *is-a-mac*
  (chez-scheme-set-env "/Users/henry/Code/scheme/lib"))

(add-to-path "/usr/local/bin")
(add-to-path "/Library/TeX/texbin")
(add-to-path "/Users/henry/.rbenv/shims")
(add-to-path "/Users/henry/.local/bin")

(when *is-a-mac*
  (setenv "DYLD_LIBRARY_PATH" "/opt/homebrew/lib")
  (setenv "LIBRARY_PATH" "/opt/homebrew/lib")
  (setenv "C_PATH" "/opt/homebrew/include"))

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


;;; Window

;; Move between windows
(defun other-window-backward ()
  "Move to previous window."
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'other-window-backward)

(use-package ace-window
  :ensure nil
  :bind ("C-x o" . ace-window))

(defun window-half-height ()
  "Return half the height of a window."
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-half-page-up-command ()
  "Scroll up half the height of a window."
  (interactive)
  (let ((scroll-preserve-screen-position t))
    (scroll-up-command (window-half-height))))

(defun scroll-half-page-down-command ()
  "Scroll down half the height of a window."
  (interactive)
  (let ((scroll-preserve-screen-position t))
    (scroll-down-command (window-half-height))))

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

;; Unbind suspend frame in GUI mode
(when (display-graphic-p)
  (global-unset-key (kbd "C-z")))


;;; Tools

(cl-defun vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote if it's not already installed.
This is a thin wrapper around `package-vc-install' in order to make
non-interactive usage more ergonomic.  Takes the following named arguments:

- FETCHER the remote where to get the package (e.g., \"gitlab\").
  If omitted, this defaults to \"github\".

- REPO should be the name of the repository (e.g.,
  \"slotThe/arXiv-citation\".

- NAME, REV, and BACKEND are as in `package-vc-install' (which
  see)."
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))

(use-package copilot
  :ensure nil
  :init (vc-install :fetcher "github" :repo "copilot-emacs/copilot.el")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("M-<tab>" . copilot-accept-completion)
              ("M-TAB" . copilot-accept-completion)))

(use-package vterm)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init)

;;; init.el ends here
