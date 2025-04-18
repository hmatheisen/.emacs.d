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
;; along with this program.  If not, see <https://www.gnu.icenses/>.

;;; Commentary:

;;

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert-mode t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-safe-themes
   '("48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "59c36051a521e3ea68dc530ded1c7be169cd19e8873b7994bfc02a216041bf3b" default))
 '(default-frame-alist '((ns-transparent-titlebar . t)))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(display-line-numbers nil)
 '(display-line-numbers-width 4)
 '(dynamic-completion-mode t)
 '(ediff-merge-split-window-function 'split-window-vertically)
 '(ediff-split-window-function 'split-window-vertically)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-ignored-server-capabilities '(:inlayHintProvider))
 '(eldoc-echo-area-use-multiline-p nil)
 '(electric-pair-mode t)
 '(epg-pinentry-mode 'loopback)
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(global-goto-address-mode t)
 '(global-page-break-lines-mode t nil (page-break-lines))
 '(global-so-long-mode t)
 '(grep-command "rg --no-heading ")
 '(grep-use-null-device nil)
 '(indent-tabs-mode nil)
 '(mode-line-compact 'long)
 '(modus-themes-bold-constructs t)
 '(modus-themes-italic-constructs t)
 '(ns-antialias-text t)
 '(ns-use-fullscreen-animation t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(acme-theme cape consult copilot copilot-chat diredfl doom-themes ef-themes
                emmet-mode exec-path-from-shell flymake-eslint forge gcmh
                google-translate helpful ibuffer-project marginalia
                multiple-cursors ns-auto-titlebar orderless page-break-lines
                prettier rainbow-delimiters rg rich-minority sass-mode sly
                treemacs vertico vterm vundo yaml-mode yasnippet))
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main")))
 '(pixel-scroll-precision-mode t)
 '(recentf-mode t)
 '(repeat-mode t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc") "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/"
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(show-paren-delay 0)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(trash-directory "~/.Trash")
 '(truncate-lines t)
 '(use-package-always-ensure t)
 '(user-mail-address "henry.mthsn@gmail.com")
 '(warning-minimum-level :emergency)
 '(windmove-default-keybindings '([ignore]))
 '(winner-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :height 140 :family "Go Mono"))))
 '(error ((t :underline nil)))
 '(fixed-pitch ((t (:inherit 'default :familiy "Go Mono"))))
 '(flymake-error ((t (:underline nil))))
 '(flymake-note ((t (:underline nil))))
 '(flymake-warning ((t (:underline nil))))
 '(variable-pitch ((t (:inherit 'default :family "Go"))))
 '(warning ((t :underline nil))))


;;; Consts

(defconst *is-a-mac* (eq system-type 'darwin))


;;; Load my own lisp code

;; Load lisp code in other directories
(let ((path (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path)
  (add-to-list 'elisp-flymake-byte-compile-load-path path))


;;; System

;; (use-package gcmh
;;   :config
;;   (gcmh-mode t))

(setq user-mail-address "henry.mthsn@gmail.com"
      smtpmail-smtp-service 587
      smtpmail-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it)

(setq gnus-select-method
      '(nnimap "imap.gmail.com"))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;; UI

;; Modus themes

(setq modus-themes-common-palette-overrides
      '(;; Make line numbers less intense
        (fg-line-number-inactive "gray50")
        (fg-line-number-active fg-main)
        (bg-line-number-inactive unspecified)
        (bg-line-number-active unspecified)
        ;; Make the fringe invisible
        (fringe unspecified)))

(use-package ef-themes
  :custom
  (ef-themes-headings '((0 1.7)
                        (1 1.5)
                        (2 1.3)
                        (3 1.1))))

(use-package doom-themes
  :custom
  ((doom-themes-enable-bold  t)
   (doom-themes-enable-italic t)
   (doom-gruvbox-dark-variant "hard")
   (doom-gruvbox-light-variant "soft")))

;; Line numbers in prog mode only
(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode t)
            (hl-line-mode t)))

(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

(use-package which-key
  :custom
  (which-key-idle-delay 1.0)
  :init
  (which-key-mode))

(use-package helpful
  :defer t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

;; Minibuffer

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("C-v" . vertico-scroll-up)
              ("M-v" . vertico-scroll-down))
  :custom
  (vertico-count 15))

(use-package marginalia
  :custom
  (marginalia-field-width 100)
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

(when *is-a-mac*
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-right-option-modifier 'nil))

;; Display buffer alist
(defun display-buffer-below (buffer-name)
  "Display BUFFER-NAME below the selected window."
  (add-to-list 'display-buffer-alist
               `(,buffer-name
                 (display-buffer-below-selected)
                 (window-height . 0.3))))

(display-buffer-below "\\*Flymake diag.+\\*")
(display-buffer-below "\\*eldoc.*\\*")

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)

(use-package ibuffer-project
  :config
  (add-hook
   'ibuffer-hook
   (lambda ()
     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
     (unless (eq ibuffer-sorting-mode 'project-file-relative)
       (ibuffer-do-sort-by-project-file-relative)))))

;; Clean modeline
;; Remove `vc-mode' from modeline
(setq-default mode-line-format
              (delete '(vc-mode vc-mode) mode-line-format))

(use-package rich-minority
  :custom
  (rm-blacklist (mapconcat 'identity '() "\\|"))
  :config
  (rich-minority-mode t))

;; Line spacing
(setq-default line-spacing 1)

;; Clean title bar
(setq-default ns-use-proxy-icon nil)

;;; Completion & Navigation

;; Orderless completion mode
(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

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
;; (use-package corfu
;;   :bind
;;   (:map corfu-map ("M-SPC" . corfu-insert-separator))
;;   :init
;;   (global-corfu-mode)
;;   (corfu-popupinfo-mode)
;;   (corfu-history-mode))

;; Test new completion-preview-mode
;; (global-completion-preview-mode)
;; (define-key completion-preview-active-mode-map
;;             (kbd "M-n")
;;             'completion-preview-next-candidate)
;; (define-key completion-preview-active-mode-map
;;             (kbd "M-p")
;;             'completion-preview-prev-candidate)
;; (define-key completion-preview-active-mode-map
;;             (kbd "M-TAB")
;;             'completion-preview-insert)

;; Setup *Completions* buffer
(setq completion-auto-help 'always
      completion-auto-select 'second-tab
      completions-max-height 15
      completions-format 'one-column
      completions-sort 'historical)

(define-key completion-list-mode-map
            (kbd "M-n")
            'minibuffer-next-completion)
(define-key completion-list-mode-map
            (kbd "M-p")
            'minibuffer-previous-completion)

;; Completion at point extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet
  :custom (yas-indent-line 'fixed)
  :config (yas-global-mode t))

;; Search and navigation
(use-package consult
  :config
  ;; Use consult/vertico for completion
  (setq completion-in-region-function #'consult-completion-in-region))

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
  (when (executable-find "gls")
    (setq insert-directory-program "gls"
          dired-listing-switches "-aGFhlv --dired --group-directories-first
          --time-style=long-iso"))
  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)
  ;; Guess a default target directory
  (setq dired-dwim-target t))

(use-package diredfl
  :config (diredfl-global-mode))

;; Quicker switch to buffer
(global-set-key (kbd "s-b") 'switch-to-buffer)

;; Invoke recentf
(global-set-key (kbd "s-r") 'recentf)

;; Ripgrep
(use-package rg
  :config
  (rg-enable-default-bindings)
  (rg-define-toggle "--context=10" "C" nil)
  (rg-define-search project-search-regex
    "Search in project using regex."
    :query ask
    :format regexp
    :files "everything"
    :dir project)
  (define-key project-prefix-map (kbd "g") 'project-search-regex))

;;; Languages

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
                     'ruby-flymake-auto)))) ; Enables rubocop
  :hook ((ruby-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (eglot-managed-mode . setup-other-flymake-backends))
  :config
  ;; Ruby LSP is promising but sucks ass right now
  ;; (add-to-list
  ;;  'eglot-server-programs
  ;;  '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  )

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

;; Sass
(use-package sass-mode)

;; Enable eslint for flymake
(use-package flymake-eslint)

;; Flymake
(use-package flymake
  :bind
  (:map flymake-mode-map
        ("M-n" . 'flymake-goto-next-error)
        ("M-p" . 'flymake-goto-prev-error)
        ("M-g f" . 'flymake-show-buffer-diagnostics)))

;; YAML mode
(use-package yaml-mode)

;; Markdown
(use-package markdown-mode
  :hook
  ((markdown-mode . (lambda ()
                      ;; Do not remove trailing whitespace when cleaning in
                      ;; markdown mode
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

;; Treesit
(defvar treesit-language-source-alist
  '((ruby "https://github.com/tree-sitter/tree-sitter-ruby")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (bash "https://github.com/tree-sitter/tree-sitter-bash")))

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
        (c++-mode . c++-ts-mode)))

;; Golang
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(setq go-ts-mode-indent-offset 4)
(add-hook 'go-ts-mode-hook
          (lambda ()
            (setq-local fill-column 100)
            (display-fill-column-indicator-mode t)
            (local-set-key (kbd "C-c f") 'eglot-format-buffer)))

;; Ruby auto insert header on new files
(define-auto-insert
  '("\\.rb\\'" . "Ruby frozen string header")
  '(nil
    "# frozen_string_literal: true\n"
    "\n"))

(add-hook 'ruby-ts-mode-hook
          (lambda ()
            (setq fill-column 100)))

;; C/C++
(add-hook 'c-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode t)
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq c-basic-offset 4)))
(add-hook 'c++-ts-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode t)
            (local-set-key (kbd "C-c f") 'eglot-format-buffer)))

;; Lisp
(use-package sly
  :config
  (setq inferior-lisp-program "sbcl --dynamic-space-size 4096"))

;;; Site lisp config

;; Run commands in given directories
(require 'tasks)

(define-task rails-migrate
  :command "bundle exec rails db:migrate"
  :description "Run migrations in rails project"
  :project-path "~/Code/elevo-rails/")

(define-task rails-gettext
  :command "bundle exec rails gettext:update"
  :description "Regenerate translation files in rails project"
  :project-path "~/Code/elevo-rails/")

(define-task sort-forestschema
  :command "bundle exec bin/sort_forestschema"
  :project-path "~/Code/elevo-rails/"
  :description "Sort `.forestadmin-schema.json' file"
  :async nil)

(define-task annotate-models
  :command "bundle exec rails annotate_models"
  :project-path "~/Code/elevo-rails/"
  :description "Annotate rails models")

(define-task pre-commit
  :command "pre-commit run --all-files"
  :project-path "~/Code/elevo-rails"
  :description "Run all pre-commit checks against all files")

;; Format buffer with the given command
(require 'format-buffer)

(format-lang ruby-ts
  :command "stree"
  :args '("format" "--print-width=100" "--plugin=plugin/trailing_comma"))

(format-lang nxml
  :command "xmllint"
  :args '("--format" "-"))

(format-lang sql
  :command "pg_format")

(format-lang c
  :command "clang-format")

(use-package prettier
  :config
  (let ((prettify-hook (lambda ()
                         (local-set-key (kbd "C-c f") 'prettier-prettify))))
    (add-hook 'typescript-ts-mode-hook prettify-hook)
    (add-hook 'tsx-ts-mode-hook prettify-hook)))

;;; Org

(use-package org
  :ensure nil
  :preface
  (defun my-org-mode-hook ()
    ;; (org-indent-mode 1)
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
          (sequence "TODO(T)" "IN PROGRESS(P)" "TO REVIEW(R)" "TO TEST(F)"
          "READY TO MERGE(M)" "|" "DONE(D)")))
  (setq org-capture-templates
        '(("i" "Emacs Ideas" entry
           (file+headline (lambda () (concat org-directory "journal.org"))
                          "Ideas")
           "* %?\nEntered on: %u")
          ("m" "Merge Request" entry
           (file (lambda () (concat org-directory "tasks.org"))
                 "Merge Requests")
           "* %?\n")
          ("t" "Todo" entry
           (file (lambda () (concat org-directory "tasks.org")))
           "* TODO %?\n")
          ("T" "Ticket" entry
           (file+headline (lambda () (concat org-directory "tasks.org"))
                          "Tickets")
           "* IN PROGRESS %?\nSCHEDULED: %t")))
  (define-skeleton org-refinement-skeleton
    "Refinement Template."
    "Insert Refinement subject: "
    "# -*- eval: (auto-fill-mode -1) -*-'\n"
    "#+TITLE: " str "\n"
    "#+OPTIONS: toc:nil\n\n"
    "* Notes :noexport:\n"
    "* Tech Solution\n"
    "** Models\n"
    "** Services\n"
    "** Controllers\n"
    "** Serializers\n"
    "** Policies/Abilities\n"
    "** Scenes\n"
    "** Components\n"
    "* Questions\n"
    "** Product\n"
    "** Tech\n"
    "* Tickets\n")
  (define-auto-insert
    '("refinements/.*\.org" . "Refinement template")
    'org-refinement-skeleton)
  (define-skeleton org-tmp-skeleton
    "tmp notes/to be imported elsewhere."
    "Insert title: "
    "# -*- eval: (auto-fill-mode -1) -*-"
    "#+TITLE: " str "\n"
    "#+DATE: " "<" (format-time-string "%Y-%m-%d") ">" "\n"
    "#+OPTIONS: toc:nil\n\n")
  (define-auto-insert
    '("tmp/.*\.org" . "Tmp notes")
    'org-tmp-skeleton))

;;; Text

(use-package google-translate
  :config
  (setq google-translate-translation-directions-alist '(("en" . "fr")
                                                        ("fr" . "en")))
  :bind
  (("C-c t" . google-translate-smooth-translate)))

(require 'isearch-transient)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'scroll-left 'disabled nil)

(defun align-equals (beg end)
  "Align `=' signs in a given region, from BEG to END."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)="))

;; Whitespace cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Auto insert mode
(auto-insert-mode t)

;; Redo binding with super
(global-set-key (kbd "s-Z") 'undo-redo)

;; Kill to end of line
(defun kill-beg-line ()
  "Kill a line from point to column 0."
  (interactive)
  (kill-line 0))

(global-set-key (kbd "s-<backspace>") 'kill-beg-line)

;; Auto fill in text mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Abbrev mode by default in all buffers
(setq-default abbrev-mode t)

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(defun kill-ring-save-whole-buffer ()
  "Save whole buffer to the kill ring."
  (interactive)
  (kill-ring-save
   (point-min)
   (point-max))
  (message "Buffer saved to kill ring."))

(global-set-key (kbd "C-x C-y") 'kill-ring-save-whole-buffer)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(global-set-key (kbd "M-j") 'join-line)

;;; Project

(define-key global-map (kbd "s-p") project-prefix-map)

(defmacro with-current-project-root (root &rest body)
  "Execute BODY with ROOT as the current project root."
  (declare (indent 1))
  (let ((project (gensym)))
    `(let* ((,project (project-current t))
            (,root (project-root ,project)))
       ,@body)))

;; Copy file absolute path
(defun project-absolute-file-path ()
  "Print and kill the absolute file path of the current buffer in a project."
  (interactive)
  (with-current-project-root root
    (let ((absolute-file-path (file-relative-name buffer-file-name root)))
      (kill-new absolute-file-path)
      (message (concat "Saved \"" absolute-file-path "\" to kill ring")))))

(define-key project-prefix-map "\C-y" 'project-absolute-file-path)

(defvar main-session 1)
(defvar rails-console-session 2)

;; open vterm at root of project
(defun project-vterm ()
  "Open a vterm at the root of the current project."
  (interactive)
  (with-current-project-root root
    (vterm main-session)
    (vterm-send-string (concat "cd " root))
    (vterm-send-return)
    (vterm-clear)))

(define-key project-prefix-map "t" 'project-vterm)

(defun project-rails-console ()
  "Open a rails console at the root of the current project."
  (interactive)
  (with-current-project-root root
    (vterm rails-console-session)
    (vterm-send-string (concat "cd " root " && bin/rails c\n"))
    (vterm-send-return)
    (vterm-clear)))

(define-key project-prefix-map (kbd "t") 'project-vterm)

(defun project-clean-buffers ()
  "Like `project-kill-buffers' but keeps some arbitrary ones."
  (interactive)
  (save-some-buffers)
  (let* ((project (project-current t))
         (project-root (project-root project))
         (project-buffers (project-buffers project))
         (dired-buffer-name (buffer-name (dired-noselect project-root))))
    (mapcar (lambda (buffer)
              (let ((buffer-name (buffer-name buffer)))
                (unless (or (string-equal buffer-name dired-buffer-name)
                            (string-equal buffer-name (concat "magit: " dired-buffer-name))
                            (string-equal buffer-name "*vterm*")
                            (string-match-p "*EGLOT" buffer-name))
                  (kill-buffer buffer))))
            project-buffers)))

(define-key project-prefix-map (kbd "k") 'project-clean-buffers)

;;; Git

(use-package transient)

(use-package magit
  :bind (("C-x g" . 'magit-status)
         :map magit-file-section-map
         ("RET" . magit-diff-visit-file-other-window)
         :map magit-hunk-section-map
         ("RET" . magit-diff-visit-file-other-window))
  :config
  (transient-append-suffix 'magit-log "-A" '("-m" "No Merges" "--no-merges")))

(use-package forge
  :after magit)

;;; Utils

(defun add-to-path (path)
  "Add a path to variable `exec-path' and Emacs \"PATH\" variable."
  (add-to-list 'exec-path path)
  (setenv "PATH" (concat (getenv "PATH") ":" path)))

(defun chez-scheme-set-env (dir)
  "List dirs in chez scheme lib DIR and set the proper env."
  (let* ((scheme-lib-dirs
          (directory-files dir t
                           directory-files-no-dot-files-regexp))
         (env (string-join scheme-lib-dirs ":")))
    (setenv "CHEZSCHEMELIBDIRS" (concat env ":."))))

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

(defun repeat-last-async-shell-command ()
  "Repeats the last shell command in as an `async-shell-command'."
  (interactive)
  (async-shell-command (car shell-command-history)))

;;; Window

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
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-<wheel-up>"))
  (global-unset-key (kbd "C-<wheel-down>")))


;;; Tools

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :bind (:map copilot-completion-map
              ("C-<return>" . copilot-accept-completion)
              ("C-RET" . copilot-accept-completion)))

(use-package copilot-chat)

(use-package vterm)

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init)

;;; init.el ends here
