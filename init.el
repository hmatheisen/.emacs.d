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
 '(auto-insert-mode t)
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-safe-themes
   '("b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "30d174000ea9cbddecd6cc695943afb7dba66b302a14f9db5dd65074e70cc744" "00d7122017db83578ef6fba39c131efdcb59910f0fac0defbe726da8072a0729" "b41d0a9413fb0034cea34eb8c9f89f6e243bdd76bccecf8292eb1fefa42eaf0a" "36c5acdaf85dda0dad1dd3ad643aacd478fb967960ee1f83981d160c52b3c8ac" default))
 '(default-frame-alist
   '((ns-transparent-titlebar . t)
     (width . 110)
     (height . 80)
     (top . 0)
     (right . 10)
     (horizontal-scroll-bars)))
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(display-line-numbers nil)
 '(display-line-numbers-width 4)
 '(doom-modeline-mode t)
 '(dynamic-completion-mode t)
 '(ediff-merge-split-window-function 'split-window-vertically)
 '(ediff-split-window-function 'split-window-vertically)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eglot-ignored-server-capabilities '(:inlayHintProvider))
 '(eldoc-echo-area-use-multiline-p nil)
 '(electric-pair-mode t)
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(global-goto-address-mode t)
 '(global-page-break-lines-mode t nil (page-break-lines))
 '(global-so-long-mode t)
 '(grep-command "rg --no-heading ")
 '(grep-use-null-device nil)
 '(indent-tabs-mode nil)
 '(mode-line-compact 'long)
 '(ns-antialias-text t)
 '(ns-use-fullscreen-animation t)
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(centaur-tabs helpful doom-modeline doom-themes sly almost-mono-themes embark prettier acme-theme rg cape yasnippet yaml-mode which-key wgrep vterm vertico treemacs sass-mode rich-minority rainbow-delimiters page-break-lines orderless markdown-mode marginalia magit flymake-eslint ef-themes diredfl corfu copilot consult))
 '(package-vc-selected-packages
   '((copilot :vc-backend Git :url "https://www.github.com/copilot-emacs/copilot.el")))
 '(pixel-scroll-precision-mode t)
 '(recentf-mode t)
 '(repeat-mode t)
 '(ring-bell-function 'ignore)
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
 '(user-mail-address "haineriz@posteo.de")
 '(warning-minimum-level :emergency)
 '(windmove-default-keybindings '([ignore]))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 150 :family "Anonymous Pro"))))
 '(flymake-error ((t (:underline nil))))
 '(flymake-note ((t (:underline nil))))
 '(flymake-warning ((t (:underline nil))))
 '(variable-pitch ((t (:height 140 :family "Futura")))))


;;; Consts

(defconst *is-a-mac* (eq system-type 'darwin))


;;; Load my own lisp code

;; Load lisp code in other directories
(let ((path (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path path)
  (add-to-list 'elisp-flymake-byte-compile-load-path path))


;;; UI

(use-package doom-themes)

(use-package doom-modeline
  :custom
  ((doom-modeline-icon nil)))

(use-package ef-themes
  :custom
  (ef-themes-headings '((0 1.7)
                        (1 1.5)
                        (2 1.3)
                        (3 1.1))))

(load-theme 'doom-old-hope)

;; (use-package theme-switcher
;;   :ensure nil
;;   :custom
;;   (theme-switcher-day-theme 'ef-arbutus)
;;   (theme-switcher-night-theme 'ef-autumn)
;;   :config
;;   (theme-switcher-mode t))

;; Line numbers in prog mode only
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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
              ("DEL" . vertico-directory-delete-char)))

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

(display-buffer-below "\\*Async Shell Command\\*")
(display-buffer-below "\\*Flymake diag.+\\*")
(display-buffer-below "\\*eldoc.*\\*")
(display-buffer-below "\\*compilation\\*")

;; Clean modeline
(use-package rich-minority
  :custom
  (rm-blacklist (mapconcat 'identity '() "\\|"))
  :config
  (rich-minority-mode t))

;; Line spacing
(setq-default line-spacing 1)

;; Remove `vc-mode' from modeline
(setq-default mode-line-format
              (delete '(vc-mode vc-mode) mode-line-format))

;;; Completion & Navigation

;; Orderless completion mode
(use-package orderless
  :custom
  (completion-styles '(orderless)))

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
  :bind
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
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
(use-package consult)

;; (use-package multiple-cursors
;;   :config
;;   (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;   (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
;;   (global-unset-key (kbd "M-<down-mouse-1>"))
;;   (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))


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

;; Ripgrep
(use-package rg
  :config
  (rg-enable-default-bindings)
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
         (eglot-managed-mode . setup-other-flymake-backends)))

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
        (c++ "https://github.com/tree-sitter/tree-sitter-cpp")))

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

;; Ruby auto insert header on new files
(define-auto-insert
  '("\\.rb\\'" . "Ruby frozen string header")
  '(nil
    "# frozen_string_literal: true\n"
    "\n"))

;; C++
(add-hook 'c++-ts-mode-hook (lambda ()
                             (display-fill-column-indicator-mode t)))

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
  :command "bin/sort_forestschema"
  :project-path "~/Code/elevo-rails/"
  :description "Sort `.forestadmin-schema.json' file"
  :async nil)

(define-task annotate-models
  :command "bundle exec rails annotate_models"
  :project-path "~/Code/elevo-rails/"
  :description "Annotate rails models")

;; Format buffer with the given command
(require 'format-buffer)

(format-lang ruby-ts
  :command "stree"
  :args '("format" "--print-width=100"))

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
          (sequence "TODO(T)" "IN PROGRESS(P)" "TO REVIEW(R)" "TO TEST(F)"
          "READY TO MERGE(M)" "|" "DONE(D)")))
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file (lambda () (concat org-directory "tasks.org")))
           "* TODO %?\n")
          ("i" "Emacs Ideas" entry
           (file+headline (lambda () (concat org-directory "journal.org"))
                          "Ideas")
           "* %?\nEntered on: %u")
          ("T" "Ticket" entry
           (file+headline (lambda () (concat org-directory "sprint.org"))
                          "Tickets")
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

(require 'isearch-transient)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)

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

(use-package avy
  :bind (("M-i" . avy-goto-char)
         ("M-j" . avy-goto-char-timer)))

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

(global-set-key (kbd "C-S-j") 'join-line)

;;; Project

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

;; open vterm at root of project
(defun project-vterm ()
  "Open a vterm at the root of the current project."
  (interactive)
  (with-current-project-root root
    (vterm)
    (vterm-send-string (concat "cd " root))
    (vterm-send-return)
    (vterm-clear)))

(define-key project-prefix-map "t" 'project-vterm)

(defvar project-rails-console-buffer nil
  "Reference to the currently open rails console buffer.")

(defun project-rails-console ()
  "Open a rails console at the root of the current project."
  (interactive)
  (if (buffer-live-p project-rails-console-buffer)
      (switch-to-buffer project-rails-console-buffer)
    (with-current-project-root root
      (vterm)
      (vterm-send-string (concat "cd " root " && bin/rails c\n"))
      (setq project-rails-console-buffer (current-buffer)))))

(define-key global-map (kbd "s-p") project-prefix-map)
(define-key project-prefix-map (kbd "t") 'project-vterm)

;;; Git

(use-package magit
  :bind (("C-x g" . 'magit-status)
         :map magit-file-section-map
         ("RET" . magit-diff-visit-file-other-window)
         :map magit-hunk-section-map
         ("RET" . magit-diff-visit-file-other-window))
  :config
  (transient-append-suffix 'magit-log "-A" '("-m" "No Merges" "--no-merges")))

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
  (global-unset-key (kbd "C-z")))


;;; Tools

;; Temporary before Emacs 30 fixes this
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
  :bind (:map copilot-completion-map
              ("C-<return>" . copilot-accept-completion)
              ("C-RET" . copilot-accept-completion)))

(use-package vterm)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init)

;;; init.el ends here
(put 'scroll-left 'disabled nil)
